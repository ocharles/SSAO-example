{-# LANGUAGE LambdaCase #-}

module GLObjects where

import Codec.Picture
import Control.Monad
import Data.Attoparsec.Text (parseOnly)
import Data.Foldable
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Text (Text, unpack)
import Data.Traversable
import Foreign
import Foreign.C
import Graphics.GL.ARB.DirectStateAccess
import Graphics.GL.ARB.SeparateShaderObjects
import Graphics.GL.Core33
import Linear
import qualified Data.Text.IO as T
import qualified Data.Vector.Storable as SV
import qualified ObjParser as Obj

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

newtype UniformSetter a =
  UniformSetter {setUniform :: Program -> String -> a -> IO ()}

m44 :: UniformSetter (M44 Float)
m44 =
  UniformSetter
    (\(Program p) uniform value ->
       do uView <-
            withCString uniform
                        (glGetUniformLocation p)
          with value
               (glProgramUniformMatrix4fv p
                                          uView
                                          1
                                          (fromIntegral GL_TRUE) .
                castPtr))

v4Array :: UniformSetter [V4 Float]
v4Array =
  UniformSetter
    (\(Program p) uniform value ->
       do location <-
            withCString uniform
                        (glGetUniformLocation p)
          withArray value
                    (glProgramUniform4fv p
                                         location
                                         (fromIntegral (length value)) .
                     castPtr))

textureUnit :: UniformSetter GLint
textureUnit =
  UniformSetter
    (\(Program p) uniform value ->
       do location <-
            withCString uniform
                        (glGetUniformLocation p)
          glProgramUniform1i p location value)

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

loadObj :: FilePath -> IO VertexArrayObject
loadObj objPath =
  do objLines <-
       T.readFile objPath >>= pure . either error id . parseOnly Obj.objLines
     let objPositions =
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
     shipVbo <- create glCreateBuffers
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
     glVertexArrayAttribFormat shipVao
                               attribPosition
                               3
                               GL_FLOAT
                               (fromIntegral GL_FALSE)
                               0
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

loadVertexFragmentProgram :: FilePath -> FilePath -> IO Program
loadVertexFragmentProgram vs fs =
  do do depthVS <- T.readFile vs
        depthFS <- T.readFile fs
        newProgram
          (\case
             VertexShader -> Just depthVS
             FragmentShader -> Just depthFS)
