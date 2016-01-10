{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Lens.Operators
import Control.Monad
import Control.Monad.Trans.State (evalState, state)
import Data.Foldable
import DebugHook
import GLObjects
import Graphics.GL.Core33
import Linear hiding (basis)
import Render
import System.Random (randomR, getStdGen)
import qualified SDL
import Paths_ssao_example

data FrameData =
  FrameData {depthFBO :: Framebuffer
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
            ,feisarDiffuse :: Texture
            ,road :: VertexArrayObject}

screenWidth, screenHeight :: Int
(screenWidth,screenHeight) = (1024,1024)

loadFrameData :: IO FrameData
loadFrameData =
  do feisarDiffuse <-
       textureFromBMP =<< getDataFileName "resources/textures/feisar.bmp"
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
       join (loadVertexFragmentProgram <$>
             getDataFileName "resources/shaders/depth_vs.glsl" <*>
             getDataFileName "resources/shaders/depth_fs.glsl")
     ssao <-
       join (loadVertexFragmentProgram <$>
             getDataFileName "resources/shaders/ssao_vs.glsl" <*>
             getDataFileName "resources/shaders/ssao_fs.glsl")
     blur <-
       join (loadVertexFragmentProgram <$>
             getDataFileName "resources/shaders/blur_vs.glsl" <*>
             getDataFileName "resources/shaders/blur_fs.glsl")
     ship <-
       join (loadVertexFragmentProgram <$>
             getDataFileName "resources/shaders/ship_vs.glsl" <*>
             getDataFileName "resources/shaders/ship_fs.glsl")
     do kernel <- newSamplingKernel
        setUniform v4Array ssao "kernel" kernel
     setUniform textureUnit ssao "rotations" 1
     setUniform textureUnit ship "diffuseMap" 1
     rotationTexture <- newRotations >>= uploadTexture2D
     shipVao <- loadObj =<< getDataFileName "resources/objects/feisar.obj"
     road <- generateRoad
     return FrameData {..}

generateRoad :: IO VertexArrayObject
generateRoad =
  let a = V3 (-3) 0 3
      b = V3 (-3) 0 (-3)
      c = V3 3 0 (-3)
      d = V3 3 0 3
  in uploadTriangles position
                     [[a,b,c],[a,c,d]]

frame :: FrameData -> Float -> IO ()
frame FrameData{..} t =
  do _ <- SDL.pollEvents
     let modelTransform =
           scaled (V4 1.0e-2 1.0e-2 1.0e-2 1) & translation .~ shipPosition
         shipPosition = V3 0 1 0
         projTransform = perspective 1.047 1 1 100
         viewTransform =
           lookAt (V3 (sin (t * 0.1) * 5)
                      (3 + t * 2)
                      (cos (t * 0.1) * 5))
                  shipPosition
                  (V3 0 1 0)
         drawScene =
           [DrawCommand {dcVertexArrayObject = shipVao
                        ,dcProgram = ship
                        ,dcTextures = []
                        ,dcUniforms = []
                        ,dcNElements = 5048
                        ,dcViewTransform = viewTransform
                        ,dcProjectionTransform = projTransform
                        ,dcModelTransform = modelTransform}
           ,DrawCommand {dcVertexArrayObject = road
                        ,dcProgram = ship
                        ,dcTextures = []
                        ,dcUniforms = []
                        ,dcNElements = 6
                        ,dcViewTransform = viewTransform
                        ,dcProjectionTransform = projTransform
                        ,dcModelTransform = identity}]
     -- pass depthPass (map (\dc -> dc {dcProgram = deferDepth}) drawScene)
     -- pass ssaoPass (map (\dc -> dc {dcProgram = ssao}) drawScene)
     -- for_ [(ssaoBlurPass1,V2 1 0,ssaoResult)
     --      ,(ssaoBlurPass2,V2 0 1,ssaoBlurredIntermediate)]
     --      (\(p,basis,source) ->
     --         pass p
     --              [DrawCommand {dcVertexArrayObject = fullScreenTriangle
     --                           ,dcProgram = blur
     --                           ,dcTextures = [source]
     --                           ,dcModelTransform = identity
     --                           ,dcViewTransform = identity
     --                           ,dcProjectionTransform = identity
     --                           ,dcNElements = 3
     --                           ,dcUniforms = [("basis",basis)]}])
     pass forwardPass
          (map (\dc -> dc {dcTextures = [ssaoBlurred,feisarDiffuse]}) drawScene)
  where fullscreen = (0,0,1024,1024)
        depthPass = Pass depthFBO fullscreen
        ssaoPass = Pass ssaoFBO fullscreen
        ssaoBlurPass1 = Pass ssaoBlurFBO1 fullscreen
        ssaoBlurPass2 = Pass ssaoBlurFBO2 fullscreen
        forwardPass = Pass (Framebuffer 0) fullscreen
        fullScreenTriangle = shipVao

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
     glEnable GL_DEPTH_TEST
     -- glEnable GL_CULL_FACE
     glEnable GL_FRAMEBUFFER_SRGB
     installDebugHook
     frameData <- loadFrameData
     traverse_ (\t ->
                  do _ <- SDL.pollEvents
                     frame frameData t
                     SDL.glSwapWindow win)
               (iterate (+ 1.0e-2) 0)
     return ()

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
