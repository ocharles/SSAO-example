{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Main where

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
     depthFBO <-
       newFramebuffer
         (\case
            DepthAttachment ->
              Just (AttachToTexture depthTexture 0)
            _ -> Nothing)
     ssaoFBO <-
       newFramebuffer
         (\case
            ColorAttachment 0 ->
              Just (AttachToTexture ssaoResult 0)
            DepthAttachment ->
              Just (AttachToRenderbuffer depthRenderbuffer)
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
            ColorAttachment 0 ->
              Just (AttachToTexture ssaoBlurred 0)
            DepthAttachment -> Nothing
            _ -> Nothing)
     deferDepth <-
       loadVertexFragmentProgram "depth_vs.glsl" "depth_fs.glsl"
     ssao <-
       loadVertexFragmentProgram "ssao_vs.glsl" "ssao_fs.glsl"
     blur <-
       loadVertexFragmentProgram "blur_vs.glsl" "blur_fs.glsl"
     ship <-
       loadVertexFragmentProgram "ship_vs.glsl" "ship_fs.glsl"
     for_ [deferDepth,ssao,ship] $
       \program ->
         do setUniform
              m44
              program
              "u_view"
              (lookAt (V3 0 60 0)
                      (V3 0 0 0)
                      (V3 0 0 (-1)) :: M44 Float)
            setUniform m44
                       program
                       "u_proj"
                       (perspective 1.047 1 0.1 100 :: M44 Float)
            setUniform m44
                       program
                       "u_model"
                       (scaled (V4 0.1 0.1 0.1 1) :: M44 Float)
     do kernel <- newSamplingKernel
        setUniform v4Array ssao "kernel" kernel
     setUniform textureUnit ssao "rotations" 1
     setUniform textureUnit ship "diffuseMap" 1
     rotationTexture <- newRotations >>= uploadTexture2D
     shipVao <- loadObj "feisar.obj"
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
