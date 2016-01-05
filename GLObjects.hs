{-# LANGUAGE LambdaCase #-}

module GLObjects where

import Data.Maybe (catMaybes)
import Graphics.GL.Core33
import Graphics.GL.ARB.DirectStateAccess
import Foreign
import Data.Foldable
import Data.Text (Text, unpack)
import Codec.Picture
import qualified Data.Vector.Storable as SV
import Data.Traversable
import Data.Monoid
import Control.Monad
import qualified Data.Text.IO as T
import Foreign.C

newtype Texture =
  Texture {textureName :: GLuint}
  deriving (Eq, Ord)

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
  deriving (Eq, Ord)

newRenderbuffer :: GLenum -> GLsizei -> GLsizei -> IO Renderbuffer
newRenderbuffer internalFormat width height =
  do name <- create glCreateRenderbuffers
     glNamedRenderbufferStorage name internalFormat width height
     pure (Renderbuffer name)

newtype Framebuffer =
  Framebuffer {framebufferName :: GLuint}
  deriving (Eq, Ord)

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
  deriving (Eq, Ord)

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

create :: (Num a,Storable b)
       => (a -> Ptr b -> IO c) -> IO b
create m = alloca (\ptr -> m 1 ptr *> peek ptr)

attribPosition, attribNormal, attribUV :: GLuint
attribPosition = 0
attribNormal = 1
attribUV = 2

newtype VertexArrayObject =
  VertexArrayObject {vertexArrayObjectName :: GLuint}
  deriving (Eq, Ord)
