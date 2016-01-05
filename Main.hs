{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad
import Control.Monad.Trans.State (evalState, state)
import Data.Attoparsec.Text (parseOnly)
import Data.Foldable
import Foreign
import Foreign.C
import GLObjects
import Graphics.GL.ARB.DirectStateAccess
import Graphics.GL.ARB.SeparateShaderObjects
import Graphics.GL.Core33
import Graphics.GL.KHR.DebugCore
import Graphics.GL.Types
import Linear hiding (basis)
import Render
import System.Random (randomR, getStdGen)
import Text.Printf
import qualified Data.Text.IO as T
import qualified ObjParser as Obj
import qualified SDL

screenWidth, screenHeight :: Int
(screenWidth,screenHeight) = (1024,1024)

main :: IO ()
main =
  do SDL.initializeAll
     win <-
       SDL.createWindow
         "SSAO Example"
         SDL.defaultWindow {SDL.windowInitialSize =
                              fromIntegral <$> V2 screenWidth screenHeight
                           ,SDL.windowOpenGL =
                              Just (SDL.defaultOpenGL {SDL.glProfile =
                                                         SDL.Core SDL.Normal 3 3})}
     SDL.glCreateContext win >>= SDL.glMakeCurrent win
     installDebugHook
     feisarDiffuse <- textureFromBMP "feisar.bmp"
     depthRenderbuffer <- newRenderbuffer GL_DEPTH_COMPONENT32F 1024 1024
     depthTexture <- newTexture2D 1 GL_DEPTH_COMPONENT32F 1024 1024
     ssaoResult <- newTexture2D 1 GL_R32F 1024 1024
     ssaoBlurredIntermediate <- newTexture2D 1 GL_R32F 1024 1024
     ssaoBlurred <- newTexture2D 1 GL_R32F 1024 1024
     depthFBO <-
       newFramebuffer
         (\case
            DepthAttachment -> Just (AttachToTexture depthTexture 0)
            _ -> Nothing)
     ssaoFBO <-
       newFramebuffer
         (\case
            ColorAttachment 0 -> Just (AttachToTexture ssaoResult 0)
            DepthAttachment -> Just (AttachToRenderbuffer depthRenderbuffer)
            _ -> Nothing)
     ssaoBlurFBO1 <-
       newFramebuffer
         (\case
            ColorAttachment 0 ->
              Just (AttachToTexture ssaoBlurredIntermediate 0)
            DepthAttachment -> Nothing
            _ -> Nothing)
     ssaoBlurFBO2 <-
       newFramebuffer
         (\case
            ColorAttachment 0 -> Just (AttachToTexture ssaoBlurred 0)
            DepthAttachment -> Nothing
            _ -> Nothing)
     deferDepth <-
       do depthVS <- T.readFile "depth_vs.glsl"
          depthFS <- T.readFile "depth_fs.glsl"
          newProgram
            (\case
               VertexShader -> Just depthVS
               FragmentShader -> Just depthFS)
     ssao <-
       do ssaoVS <- T.readFile "ssao_vs.glsl"
          ssaoFS <- T.readFile "ssao_fs.glsl"
          newProgram
            (\case
               VertexShader -> Just ssaoVS
               FragmentShader -> Just ssaoFS)
     blur <-
       do blurVS <- T.readFile "blur_vs.glsl"
          blurFS <- T.readFile "blur_fs.glsl"
          newProgram
            (\case
               VertexShader -> Just blurVS
               FragmentShader -> Just blurFS)
     ship <-
       do shipVS <- T.readFile "ship_vs.glsl"
          shipFS <- T.readFile "ship_fs.glsl"
          newProgram
            (\case
               VertexShader -> Just shipVS
               FragmentShader -> Just shipFS)
     for_ [deferDepth,ssao,ship] $
       \(Program program) ->
         do do uView <-
                 withCString "u_view"
                             (glGetUniformLocation program)
               with (lookAt (V3 0 60 0)
                            (V3 0 0 0)
                            (V3 0 0 (-1)) :: M44 Float)
                    (glProgramUniformMatrix4fv program
                                               uView
                                               1
                                               (fromIntegral GL_TRUE) .
                     castPtr)
            do uProj <-
                 withCString "u_proj"
                             (glGetUniformLocation program)
               with (perspective 1.047 1 0.1 100 :: M44 Float)
                    (glProgramUniformMatrix4fv program
                                               uProj
                                               1
                                               (fromIntegral GL_TRUE) .
                     castPtr)
            do uModel <-
                 withCString "u_model"
                             (glGetUniformLocation program)
               with (scaled (V4 0.1 0.1 0.1 1) :: M44 Float)
                    (glProgramUniformMatrix4fv program
                                               uModel
                                               1
                                               (fromIntegral GL_TRUE) .
                     castPtr)
     do uKernel <-
          withCString "kernel"
                      (glGetUniformLocation (programName ssao))
        kernel <- newSamplingKernel
        withArray kernel
                  (glProgramUniform4fv (programName ssao)
                                       uKernel
                                       (fromIntegral (length kernel)) .
                   castPtr)
     do uRotations <-
          withCString "rotations"
                      (glGetUniformLocation (programName ssao))
        glProgramUniform1i (programName ssao)
                           uRotations
                           1
     do uDiffuse <-
          withCString "diffuseMap"
                      (glGetUniformLocation (programName ship))
        glProgramUniform1i (programName ship)
                           uDiffuse
                           1
     rotationTexture <- newRotations >>= uploadTexture2D
     uBlurBasis <-
       withCString "basis"
                   (glGetUniformLocation (programName blur))
     shipVao <-
       T.readFile "feisar.obj" >>=
       fromObj . either error id . parseOnly Obj.objLines
     glEnable GL_DEPTH_TEST
     tick GfxData {..} 0
     return ()

data GfxData =
  GfxData {depthFBO :: Framebuffer
          ,ssaoFBO :: Framebuffer
          ,deferDepth :: Program
          ,ssao :: Program
          ,ssaoBlurred :: Texture
          ,ship :: Program
          ,blur :: Program
          ,depthTexture :: Texture
          ,shipVao :: VertexArrayObject
          ,rotationTexture :: Texture
          ,ssaoBlurFBO1 :: Framebuffer
          ,ssaoResult :: Texture
          ,ssaoBlurFBO2 :: Framebuffer
          ,ssaoBlurredIntermediate :: Texture
          ,uBlurBasis :: GLint
          ,feisarDiffuse :: Texture
          ,win :: SDL.Window}

tick :: GfxData -> Float -> IO ()
tick gfx@GfxData{..} t =
  do _ <- SDL.pollEvents
     let modelTransform =
           m33_to_m44
             (fromQuaternion
                (axisAngle (V3 1 0 0)
                           (t * 2)) !*!
              fromQuaternion (axisAngle (V3 0 1 0) t)) !*!
           scaled (V4 0.1 0.1 0.1 1)
     pass depthPass
          [DrawCommand {dcVertexArrayObject = shipVao
                       ,dcProgram = deferDepth
                       ,dcTextures = []
                       ,dcModelTransform = modelTransform
                       ,dcNVertices = 5048
                       ,dcUniforms = []}]
     pass ssaoPass
          [DrawCommand {dcVertexArrayObject = shipVao
                       ,dcProgram = ssao
                       ,dcTextures = [depthTexture,rotationTexture]
                       ,dcModelTransform = modelTransform
                       ,dcNVertices = 5048
                       ,dcUniforms = []}]
     for_ [(ssaoBlurPass1,V2 1 0,ssaoResult)
          ,(ssaoBlurPass2,V2 0 1,ssaoBlurredIntermediate)]
          (\(p,basis,source) ->
             pass p
                  [DrawCommand {dcVertexArrayObject = fullScreenTriangle
                               ,dcProgram = blur
                               ,dcTextures = [source]
                               ,dcModelTransform = identity
                               ,dcNVertices = 3
                               ,dcUniforms = [("basis",basis)]}])
     pass forwardPass
          [DrawCommand {dcVertexArrayObject = shipVao
                       ,dcProgram = ship
                       ,dcTextures = [ssaoBlurred,feisarDiffuse]
                       ,dcUniforms = []
                       ,dcNVertices = 5048
                       ,dcModelTransform = modelTransform}]
     SDL.glSwapWindow win
     tick gfx (t + 1.0e-2)
  where fullscreen = (0,0,1024,1024)
        depthPass = Pass depthFBO fullscreen
        ssaoPass = Pass ssaoFBO fullscreen
        ssaoBlurPass1 = Pass ssaoBlurFBO1 fullscreen
        ssaoBlurPass2 = Pass ssaoBlurFBO2 fullscreen
        forwardPass = Pass (Framebuffer 0) fullscreen
        fullScreenTriangle = shipVao

newSamplingKernel :: IO [V4 Float]
newSamplingKernel =
  fmap (evalState (mapM (\i ->
                           do v <-
                                fmap normalize
                                     (V4 <$> state (randomR (-1,1)) <*>
                                      state (randomR (-1,1)) <*>
                                      state (randomR (0,1)) <*>
                                      pure 0)
                              let scale =
                                    fromIntegral (i :: Int) / (4 * 4) :: V1 Float
                              pure (v ^*
                                    case lerp 0.1 1.0 (scale * scale) of
                                      V1 x -> x))
                        [0 .. 4 * 4 + 1]))
       getStdGen

newRotations :: IO [[V4 Float]]
newRotations =
  fmap (evalState (replicateM
                     4
                     (replicateM
                        4
                        (fmap ((+ 0.5) . (^* 0.5) . normalize)
                              (V4 <$> state (randomR (-1,1)) <*>
                               state (randomR (-1,1)) <*>
                               pure 0 <*>
                               pure 0)))))
       getStdGen

installDebugHook :: IO ()
installDebugHook
  | gl_KHR_debug =
    do cb <- makeGLDEBUGPROC glCallback
       glDebugMessageCallback cb nullPtr
       glEnable GL_DEBUG_OUTPUT_SYNCHRONOUS
  | otherwise = return ()

glCallback :: GLenum
           -> GLenum
           -> GLuint
           -> GLenum
           -> GLsizei
           -> Ptr GLchar
           -> Ptr ()
           -> IO ()
glCallback source t ident severity _ message _ =
  do message' <- peekCString message
     putStrLn $
       printf "opengl %s [%s] %s (%s): %s" t' severity' source' (show ident) message'
  where source' =
          case source of
            GL_DEBUG_SOURCE_API ->
              "API" :: String
            GL_DEBUG_SOURCE_WINDOW_SYSTEM -> "Window System"
            GL_DEBUG_SOURCE_SHADER_COMPILER -> "Shader Compiler"
            GL_DEBUG_SOURCE_THIRD_PARTY -> "Third Party"
            GL_DEBUG_SOURCE_APPLICATION -> "Application"
            GL_DEBUG_SOURCE_OTHER -> "Other"
            _ -> "Unknown"
        t' =
          case t of
            GL_DEBUG_TYPE_ERROR ->
              "Error" :: String
            GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR -> "Deprecated Behaviour"
            GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR -> "Undefined Behaviour"
            GL_DEBUG_TYPE_PORTABILITY -> "Portability"
            GL_DEBUG_TYPE_PERFORMANCE -> "Performance"
            GL_DEBUG_TYPE_OTHER -> "Other"
            GL_DEBUG_TYPE_MARKER -> "Marker"
            _ -> "Unknown"
        severity' =
          case severity of
            GL_DEBUG_SEVERITY_HIGH ->
              "High" :: String
            GL_DEBUG_SEVERITY_MEDIUM -> "Medium"
            GL_DEBUG_SEVERITY_LOW -> "Low"
            GL_DEBUG_SEVERITY_NOTIFICATION -> "Notification"
            _ -> "Unknown"

data Vertex =
  Vertex (V3 Float)
         (V3 Float)
         (V2 Float)
  deriving (Show)

instance Storable Vertex where
  sizeOf ~(Vertex a b c) = sizeOf a + sizeOf b + sizeOf c
  peek ptr =
    do Vertex <$> peek (castPtr ptr) <*>
         peek (castPtr (ptr `plusPtr`
                        fromIntegral (sizeOf (undefined :: V3 Float)))) <*>
         peek (castPtr (ptr `plusPtr`
                        fromIntegral (sizeOf (undefined :: V3 Float) * 2)))
  poke ptr (Vertex a b c) =
    do poke (castPtr ptr) a
       poke (castPtr (ptr `plusPtr`
                      fromIntegral (sizeOf (undefined :: V3 Float))))
            b
       poke (castPtr (ptr `plusPtr`
                      fromIntegral (sizeOf (undefined :: V3 Float) * 2)))
            c
  alignment _ = 0

fromObj :: [Obj.Line] -> IO VertexArrayObject
fromObj objLines =
  do shipVbo <- create glCreateBuffers
     withArray objVertices
               (\ptr ->
                  glNamedBufferData
                    shipVbo
                    (fromIntegral
                       (sizeOf (undefined :: Vertex) * length objVertices))
                    (castPtr ptr)
                    GL_STATIC_DRAW)
     shipVao <- create glCreateVertexArrays
     let bindingIndex = 0
     glVertexArrayVertexBuffer shipVao
                               bindingIndex
                               shipVbo
                               0
                               (fromIntegral (sizeOf (undefined :: Vertex)))
     for_ [attribPosition,attribNormal,attribUV]
          (\attrib ->
             do glEnableVertexArrayAttrib shipVao attrib
                glVertexArrayAttribBinding shipVao attrib bindingIndex)
     glVertexArrayAttribFormat shipVao attribPosition 3 GL_FLOAT (fromIntegral GL_FALSE) 0
     glVertexArrayAttribFormat shipVao
                               attribNormal
                               3
                               GL_FLOAT
                               (fromIntegral GL_FALSE)
                               (fromIntegral (sizeOf (0 :: V3 Float)))
     glVertexArrayAttribFormat shipVao
                               attribUV
                               2
                               GL_FLOAT
                               (fromIntegral GL_FALSE)
                               (fromIntegral (sizeOf (0 :: V3 Float) * 2))
     pure (VertexArrayObject shipVao)
  where objPositions =
          concatMap (\case
                       Obj.LineVertex lv -> [lv]
                       _ -> [])
                    objLines
        objNormals =
          concatMap (\case
                       Obj.LineVertexNormal lv ->
                         [lv]
                       _ -> [])
                    objLines
        objTextureCoordinates =
          concatMap (\case
                       Obj.LineTextureCoordinate tc ->
                         [tc]
                       _ -> [])
                    objLines
        mkVertex faceVertex =
          Vertex (case objPositions !! pred (Obj.fvVertex faceVertex) of
                    Obj.Vertex x y z _ ->
                      fmap realToFrac (V3 x y z))
                 (case Obj.fvVertexNormal faceVertex of
                    Just tcIndex ->
                      case objNormals !! pred tcIndex of
                        Obj.Vertex x y z _ ->
                          fmap realToFrac (V3 x y z)
                    Nothing -> 0)
                 (case Obj.fvTextureCoordinate faceVertex of
                    Just tcIndex ->
                      case objTextureCoordinates !! pred tcIndex of
                        Obj.TextureCoordinate u v _ ->
                          fmap realToFrac (V2 u (1 - v))
                    Nothing -> V2 0 0)
        objTriangles =
          concatMap (\case
                       tri@[_,_,_] ->
                         [map mkVertex tri]
                       [a,b,c,d] ->
                         [map mkVertex [a,b,c],map mkVertex [a,c,d]]
                       faces ->
                         error (show faces)) $
          concatMap (\case
                       Obj.LineFace vertices ->
                         [vertices]
                       _ -> [])
                    objLines
        objVertices =
          [v | tri <- objTriangles
             , v <- tri]
        objIndices :: [Word16]
        objIndices =
          map fst
              (zip [0 ..]
                   (concat objTriangles))
