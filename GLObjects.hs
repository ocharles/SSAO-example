{-# LANGUAGE LambdaCase #-}

module GLObjects where

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
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

objVertexAttribs :: VertexAttrib Vertex
objVertexAttribs =
  divide (\(Vertex a b c) -> (a,(b,c)))
         position
         (divided normal uv)

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
     VertexArrayObject <$> uploadVertices objVertexAttribs objVertices

loadVertexFragmentProgram :: FilePath -> FilePath -> IO Program
loadVertexFragmentProgram vs fs =
  do do depthVS <- T.readFile vs
        depthFS <- T.readFile fs
        newProgram
          (\case
             VertexShader -> Just depthVS
             FragmentShader -> Just depthFS)

data Attribute = Position | Normal | UV

data VertexAttrib a =
  VertexAttrib {vertexAttribSize :: GLuint
               ,vertexAttribPoke :: Ptr a -> a -> IO ()
               ,vertexAttribFormat :: [(Attribute,GLint,GLenum,GLenum,GLuint)]}

instance Contravariant VertexAttrib where
  contramap f (VertexAttrib sz poke_ fmt) =
    VertexAttrib
      sz
      (\ptr v ->
         poke_ (castPtr ptr)
               (f v))
      fmt

instance Divisible VertexAttrib where
  conquer =
    VertexAttrib 0
                 (\_ _ -> return ())
                 []
  divide f fb fc =
    VertexAttrib {vertexAttribSize = vertexAttribSize fb + vertexAttribSize fc
                 ,vertexAttribPoke =
                    \ptr a ->
                      do let (b,c) = f a
                         vertexAttribPoke fb
                                          (castPtr ptr)
                                          b
                         vertexAttribPoke
                           fc
                           (castPtr ptr `plusPtr`
                            fromIntegral (vertexAttribSize fb))
                           c
                 ,vertexAttribFormat =
                    vertexAttribFormat fb <>
                    (map (\(a,b,c,d,e) ->
                            (a,b,c,d,e + vertexAttribSize fb))
                         (vertexAttribFormat fc))}

position :: VertexAttrib (V3 Float)
position =
  VertexAttrib {vertexAttribSize =
                  fromIntegral (sizeOf (0 :: V3 Float))
               ,vertexAttribPoke = poke
               ,vertexAttribFormat =
                  [(Position,3,GL_FLOAT,GL_FALSE,0)]}

normal :: VertexAttrib (V3 Float)
normal =
  VertexAttrib {vertexAttribSize =
                  fromIntegral (sizeOf (0 :: V3 Float))
               ,vertexAttribPoke = poke
               ,vertexAttribFormat =
                  [(Normal,3,GL_FLOAT,GL_FALSE,0)]}

uv :: VertexAttrib (V2 Float)
uv =
  VertexAttrib {vertexAttribSize =
                  fromIntegral (sizeOf (0 :: V2 Float))
               ,vertexAttribPoke = poke
               ,vertexAttribFormat =
                  [(UV,2,GL_FLOAT,GL_FALSE,0)]}

uploadVertices :: VertexAttrib v -> [v] -> IO GLuint
uploadVertices attribs dat =
  do let sizeInBytes = fromIntegral (vertexAttribSize attribs) * length dat
     buffer <- mallocBytes sizeInBytes
     sequence_ (zipWith (\v offset ->
                           vertexAttribPoke
                             attribs
                             (buffer `plusPtr` fromIntegral offset)
                             v)
                        dat
                        (iterate (+ vertexAttribSize attribs) 0))
     vbo <- create glCreateBuffers
     glNamedBufferData vbo
                       (fromIntegral sizeInBytes)
                       buffer
                       GL_STATIC_DRAW
     vao <- create glCreateVertexArrays
     let bindingIndex = 0
     glVertexArrayVertexBuffer vao
                               bindingIndex
                               vbo
                               0
                               (fromIntegral (vertexAttribSize attribs))
     for_ (vertexAttribFormat attribs)
          (\(attribTy,components,compTy,normalized,offset) ->
             do let attribIndex =
                      case attribTy of
                        Position -> attribPosition
                        Normal -> attribNormal
                        UV -> attribUV
                glEnableVertexArrayAttrib vao attribIndex
                glVertexArrayAttribBinding vao attribIndex bindingIndex
                glVertexArrayAttribFormat vao
                                          attribIndex
                                          components
                                          compTy
                                          (fromIntegral normalized)
                                          offset)
     pure vbo
