{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Codec.Picture
import Control.Monad
import Control.Monad.Trans.State (evalState, state)
import Data.Attoparsec.Text (parseOnly)
import Data.Foldable
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Text (Text, unpack)
import Data.Traversable
import Foreign
import Foreign.C
import Graphics.GL.Core33
import Graphics.GL.ARB.DirectStateAccess
import Graphics.GL.ARB.SeparateShaderObjects
import Graphics.GL.KHR.DebugCore
import Graphics.GL.Types
import Linear hiding (basis)
import System.Random (randomR, getStdGen)
import Text.Printf
import qualified Data.Text.IO as T
import qualified Data.Vector.Storable as SV
import qualified ObjParser as Obj
import qualified SDL

newtype Texture =
  Texture {textureName :: GLuint}

newTexture2D
  :: GLsizei -> GLenum -> GLsizei -> GLsizei -> IO Texture
newTexture2D levels internalFormat width height =
  do name <-
       create (glCreateTextures GL_TEXTURE_2D)
     glTextureStorage2D name levels internalFormat width height
     pure (Texture name)

uploadTexture2D :: (Foldable t,Storable a)
                => t [a] -> IO Texture
uploadTexture2D pixels =
  do t <- newTexture2D 1 GL_RGBA32F 4 4
     glPixelStorei GL_UNPACK_LSB_FIRST 0
     glPixelStorei GL_UNPACK_SWAP_BYTES 0
     glPixelStorei GL_UNPACK_ROW_LENGTH 0
     glPixelStorei GL_UNPACK_IMAGE_HEIGHT 0
     glPixelStorei GL_UNPACK_SKIP_ROWS 0
     glPixelStorei GL_UNPACK_SKIP_PIXELS 0
     glPixelStorei GL_UNPACK_SKIP_IMAGES 0
     glPixelStorei GL_UNPACK_ALIGNMENT 1
     withArray (concat pixels)
               (glTextureSubImage2D (textureName t)
                                    0
                                    0
                                    0
                                    4
                                    4
                                    GL_RGBA
                                    GL_FLOAT .
                castPtr)
     pure t

textureFromBMP :: FilePath -> IO Texture
textureFromBMP filePath =
  do res <- readImage filePath
     case res of
       Left e -> error e
       Right (ImageRGB8 (Image width height pixels)) ->
         do t <-
              newTexture2D 1
                           GL_RGB8
                           (fromIntegral width)
                           (fromIntegral height)
            SV.unsafeWith
              pixels
              (glTextureSubImage2D (textureName t)
                                   0
                                   0
                                   0
                                   (fromIntegral width)
                                   (fromIntegral height)
                                   GL_RGB
                                   GL_UNSIGNED_BYTE .
               castPtr)
            return t
       Right _ -> error "Unknown image type"

newtype Renderbuffer =
  Renderbuffer {renderbufferName :: GLuint}

newRenderbuffer :: GLenum -> GLsizei -> GLsizei -> IO Renderbuffer
newRenderbuffer internalFormat width height =
  do name <- create glCreateRenderbuffers
     glNamedRenderbufferStorage name internalFormat width height
     pure (Renderbuffer name)

newtype Framebuffer =
  Framebuffer {framebufferName :: GLuint}

data AttachTo
  = AttachToTexture Texture
                    GLint
  | AttachToRenderbuffer Renderbuffer

data FramebufferAttachment
  = ColorAttachment GLenum
  | DepthAttachment
  | StencilAttachment

attachmentForGL :: FramebufferAttachment -> GLenum
attachmentForGL (ColorAttachment n) = GL_COLOR_ATTACHMENT0 + fromIntegral n
attachmentForGL DepthAttachment = GL_DEPTH_ATTACHMENT
attachmentForGL StencilAttachment = GL_STENCIL_ATTACHMENT

newFramebuffer
  :: (FramebufferAttachment -> Maybe AttachTo) -> IO Framebuffer
newFramebuffer f =
  do name <- create glCreateFramebuffers
     maxColorAttachments <-
       alloca (\ptr ->
                 glGetIntegerv GL_MAX_COLOR_ATTACHMENTS ptr *> peek ptr)
     for_ (DepthAttachment :
           StencilAttachment :
           map ColorAttachment [0 .. fromIntegral maxColorAttachments - 1])
          (\attachment ->
             for_ (f attachment)
                  (\case
                     AttachToTexture (Texture t) level ->
                       glNamedFramebufferTexture name
                                                 (attachmentForGL attachment)
                                                 t
                                                 level
                     AttachToRenderbuffer (Renderbuffer rb) ->
                       glNamedFramebufferRenderbuffer name
                                                      (attachmentForGL attachment)
                                                      GL_RENDERBUFFER
                                                      rb))
     status <-
       glCheckNamedFramebufferStatus name GL_FRAMEBUFFER
     putStrLn (case status of
                 GL_FRAMEBUFFER_UNDEFINED -> "Framebuffer undefined"
                 GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT -> "Incomplete attachment"
                 GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT -> "Missing attachment"
                 GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER -> "Incomplete draw buffer"
                 GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER -> "Incomplete read buffer"
                 GL_FRAMEBUFFER_UNSUPPORTED -> "Unsupported"
                 GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE -> "Incomplete multisample"
                 GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS -> "Incomplete layer targets"
                 GL_FRAMEBUFFER_COMPLETE -> "Complete"
                 _ -> "Unknown status")
     pure (Framebuffer name)

data StageSource
  = VertexShader
  | FragmentShader
  deriving (Show)

glShaderStage :: StageSource -> GLenum
glShaderStage VertexShader = GL_VERTEX_SHADER
glShaderStage FragmentShader = GL_FRAGMENT_SHADER

newtype Program =
  Program {programName :: GLuint}

newProgram
  :: (StageSource -> Maybe Text) -> IO Program
newProgram f =
  do name <- glCreateProgram
     shaders <-
       for [VertexShader,FragmentShader]
           (\stage ->
              for (f stage)
                  (\src ->
                     do shaderName <-
                          glCreateShader (glShaderStage stage)
                        withCString
                          (unpack src)
                          (\srcPtr ->
                             withArray [srcPtr]
                                       (\srcs ->
                                          glShaderSource shaderName 1 srcs nullPtr))
                        glCompileShader shaderName
                        compiled <-
                          alloca (\ptr ->
                                    glGetShaderiv shaderName GL_COMPILE_STATUS ptr *>
                                    peek ptr)
                        unless (fromIntegral compiled == GL_TRUE)
                               (do putStrLn ("Shader stage failed to compile: " <>
                                             show stage)
                                   T.putStrLn src
                                   logLen <-
                                     alloca (\ptr ->
                                               glGetShaderiv shaderName GL_INFO_LOG_LENGTH ptr *>
                                               peek ptr)
                                   allocaBytes (fromIntegral logLen) $
                                     \infoLogPtr ->
                                       alloca $
                                       \lenPtr ->
                                         do glGetShaderInfoLog shaderName 1024 lenPtr infoLogPtr
                                            peekCString infoLogPtr >>= putStrLn)
                        glAttachShader name shaderName
                        pure shaderName))
     withCString "a_position"
                 (glBindAttribLocation name attribPosition)
     withCString "a_normal"
                 (glBindAttribLocation name attribNormal)
     withCString "a_uv"
                 (glBindAttribLocation name attribUV)
     glLinkProgram name
     compiled <-
       alloca (\ptr ->
                 glGetProgramiv name GL_LINK_STATUS ptr *> peek ptr)
     unless (fromIntegral compiled == GL_TRUE)
            (do putStrLn "Program failed to link"
                logLen <-
                  alloca (\ptr ->
                            glGetProgramiv name GL_INFO_LOG_LENGTH ptr *>
                            peek ptr)
                allocaBytes (fromIntegral logLen) $
                  \infoLogPtr ->
                    alloca $
                    \lenPtr ->
                      do glGetProgramInfoLog name 1024 lenPtr infoLogPtr
                         peekCString infoLogPtr >>= putStrLn)
     for_ (catMaybes shaders) (glDetachShader name)
     pure (Program name)

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
     depthRenderbuffer <-
       newRenderbuffer GL_DEPTH_COMPONENT32F 1024 1024
     depthTexture <-
       newTexture2D 1 GL_DEPTH_COMPONENT32F 1024 1024
     ssaoResult <-
       newTexture2D 1 GL_R32F 1024 1024
     ssaoBlurredIntermediate <-
       newTexture2D 1 GL_R32F 1024 1024
     ssaoBlurred <-
       newTexture2D 1 GL_R32F 1024 1024
     Framebuffer depthFBO <-
       newFramebuffer
         (\case
            DepthAttachment ->
              Just (AttachToTexture depthTexture 0)
            _ -> Nothing)
     Framebuffer ssaoFBO <-
       newFramebuffer
         (\case
            ColorAttachment 0 ->
              Just (AttachToTexture ssaoResult 0)
            DepthAttachment ->
              Just (AttachToRenderbuffer depthRenderbuffer)
            _ -> Nothing)
     Framebuffer ssaoBlurFBO1 <-
       newFramebuffer
         (\case
            ColorAttachment 0 ->
              Just (AttachToTexture ssaoBlurredIntermediate 0)
            DepthAttachment -> Nothing
            _ -> Nothing)
     Framebuffer ssaoBlurFBO2 <-
       newFramebuffer
         (\case
            ColorAttachment 0 ->
              Just (AttachToTexture ssaoBlurred 0)
            DepthAttachment -> Nothing
            _ -> Nothing)
     Program deferDepth <-
       do depthVS <- T.readFile "depth_vs.glsl"
          depthFS <- T.readFile "depth_fs.glsl"
          newProgram
            (\case
               VertexShader -> Just depthVS
               FragmentShader -> Just depthFS)
     Program ssao <-
       do ssaoVS <- T.readFile "ssao_vs.glsl"
          ssaoFS <- T.readFile "ssao_fs.glsl"
          newProgram
            (\case
               VertexShader -> Just ssaoVS
               FragmentShader -> Just ssaoFS)
     Program blur <-
       do blurVS <- T.readFile "blur_vs.glsl"
          blurFS <- T.readFile "blur_fs.glsl"
          newProgram
            (\case
               VertexShader -> Just blurVS
               FragmentShader -> Just blurFS)
     Program ship <-
       do shipVS <- T.readFile "ship_vs.glsl"
          shipFS <- T.readFile "ship_fs.glsl"
          newProgram
            (\case
               VertexShader -> Just shipVS
               FragmentShader -> Just shipFS)
     for_ [deferDepth,ssao,ship] $
       \program ->
         do do uView <-
                 withCString "u_view"
                             (glGetUniformLocation program)
               with (lookAt (V3 0 60 0)
                            (V3 0 0 0)
                            (V3 0 0 (-1)) :: M44 Float)
                    (glProgramUniformMatrix4fv program uView 1 (fromIntegral GL_TRUE) .
                     castPtr)
            do uProj <-
                 withCString "u_proj"
                             (glGetUniformLocation program)
               with (perspective 1.047 1 0.1 100 :: M44 Float)
                    (glProgramUniformMatrix4fv program uProj 1 (fromIntegral GL_TRUE) .
                     castPtr)
            do uModel <-
                 withCString "u_model"
                             (glGetUniformLocation program)
               with (scaled (V4 0.1 0.1 0.1 1) :: M44 Float)
                    (glProgramUniformMatrix4fv program uModel 1 (fromIntegral GL_TRUE) .
                     castPtr)
     do uKernel <-
          withCString "kernel"
                      (glGetUniformLocation ssao)
        kernel <- newSamplingKernel
        withArray kernel
                  (glProgramUniform4fv ssao
                                       uKernel
                                       (fromIntegral (length kernel)) .
                   castPtr)
     do uRotations <-
          withCString "rotations"
                      (glGetUniformLocation ssao)
        glProgramUniform1i ssao uRotations 1
     do uDiffuse <-
          withCString "diffuseMap"
                      (glGetUniformLocation ship)
        glProgramUniform1i ship uDiffuse 1
     rotationTexture <- newRotations >>= uploadTexture2D
     uBlurBasis <-
       withCString "basis"
                   (glGetUniformLocation blur)
     shipVao <-
       T.readFile "feisar.obj" >>=
       fromObj . either error id . parseOnly Obj.objLines
     glEnable GL_DEPTH_TEST
     tick GfxData {..} 0
     return ()

data GfxData =
  GfxData {depthFBO :: GLuint
          ,ssaoFBO :: GLuint
          ,deferDepth :: GLuint
          ,ssao :: GLuint
          ,ssaoBlurred :: Texture
          ,ship :: GLuint
          ,blur :: GLuint
          ,depthTexture :: Texture
          ,shipVao :: GLuint
          ,rotationTexture :: Texture
          ,ssaoBlurFBO1 :: GLuint
          ,ssaoResult :: Texture
          ,ssaoBlurFBO2 :: GLuint
          ,ssaoBlurredIntermediate :: Texture
          ,uBlurBasis :: GLint
          ,feisarDiffuse :: Texture
          ,win :: SDL.Window}

tick :: GfxData -> Float -> IO ()
tick gfx@GfxData{..} t =
  do _ <- SDL.pollEvents
     for_ [deferDepth,ssao,ship] $
       \program ->
         do uModel <-
              withCString "u_model"
                          (glGetUniformLocation program)
            with (m33_to_m44
                    (fromQuaternion
                       (axisAngle (V3 1 0 0)
                                  (t * 2)) !*!
                     fromQuaternion (axisAngle (V3 0 1 0) t)) !*!
                  scaled (V4 0.1 0.1 0.1 1) :: M44 Float)
                 (glProgramUniformMatrix4fv program uModel 1 (fromIntegral GL_TRUE) . castPtr)
     do glBindFramebuffer GL_FRAMEBUFFER depthFBO
        glViewport 0 0 1024 1024
        glClearColor 0 0 0 1
        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
        glUseProgram deferDepth
        glBindVertexArray shipVao
        glDrawArrays GL_TRIANGLES 0 5048
     do glBindFramebuffer GL_FRAMEBUFFER ssaoFBO
        glViewport 0 0 1024 1024
        glClearColor 1 1 1 1
        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
        glUseProgram ssao
        glActiveTexture GL_TEXTURE0
        glBindTexture GL_TEXTURE_2D
                      (textureName depthTexture)
        glActiveTexture GL_TEXTURE1
        glBindTexture GL_TEXTURE_2D
                      (textureName rotationTexture)
        glBindVertexArray shipVao
        glDrawArrays GL_TRIANGLES 0 5048
     for_ [(ssaoBlurFBO1,V2 1 0,ssaoResult)
          ,(ssaoBlurFBO2,V2 0 1,ssaoBlurredIntermediate)]
          (\(fbo,basis,source) ->
             do glBindFramebuffer GL_FRAMEBUFFER fbo
                glViewport 0 0 1024 1024
                glDisable GL_DEPTH_TEST
                glUseProgram blur
                with (basis :: V2 Float)
                     (glProgramUniform2fv blur uBlurBasis 1 . castPtr)
                glActiveTexture GL_TEXTURE0
                glBindTexture GL_TEXTURE_2D
                              (textureName source)
                glBindVertexArray shipVao
                glDrawArrays GL_TRIANGLES 0 3)
     do glBindFramebuffer GL_FRAMEBUFFER 0
        glViewport 0 0 1024 1024
        glClearColor 0 0 0 1
        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
        glEnable GL_DEPTH_TEST
        glUseProgram ship
        glActiveTexture GL_TEXTURE0
        glBindTexture GL_TEXTURE_2D
                      (textureName ssaoBlurred)
        glActiveTexture GL_TEXTURE1
        glBindTexture GL_TEXTURE_2D
                      (textureName feisarDiffuse)
        glBindVertexArray shipVao
        glDrawArrays GL_TRIANGLES 0 5048
     SDL.glSwapWindow win
     tick gfx (t + 1.0e-2)

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

attribPosition, attribNormal, attribUV :: GLuint
attribPosition = 0
attribNormal = 1
attribUV = 2

fromObj :: [Obj.Line] -> IO GLuint
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
     pure shipVao
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

create :: (Num a,Storable b)
       => (a -> Ptr b -> IO c) -> IO b
create m = alloca (\ptr -> m 1 ptr *> peek ptr)
