{-# LANGUAGE OverloadedStrings #-}

module ObjParser where

import Data.Maybe
import Control.Applicative
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text

data Line
  = LineVertex Vertex
  | LineVertexNormal Vertex
  | LineTextureCoordinate TextureCoordinate
  | LineFace !([FaceVertex])
  deriving ((Show))

data Vertex =
  Vertex !Double
         !Double
         !Double
         !Double
  deriving (((Show)))

data TextureCoordinate =
  TextureCoordinate !Double
                    !Double
                    !Double
  deriving (((Show)))

data FaceVertex =
  FaceVertex {fvVertex :: !Int
             ,fvTextureCoordinate :: !(Maybe Int)
             ,fvVertexNormal :: !(Maybe Int)}
  deriving (((Show)))

line :: Parser (Maybe Line)
line =
  (Just <$>
   ((LineVertex <$> vertex) <|> (LineTextureCoordinate <$> textureCoordinate) <|>
    (LineFace <$> face) <|>
    (LineVertexNormal <$> vertexNormal)) <*
   commentToEOL) <|>
  (Nothing <$
   ((string "mtllib" <|> string "g" <|> string "usemtl" <|> string "o" <|> string "s") <*
    skipWhile (not . isEndOfLine))) <|>
  (Nothing <$ commentToEOL)

objLines :: Parser [Line]
objLines = catMaybes <$> sepBy line endOfLine <* endOfInput

vertex :: Parser Vertex
vertex =
  (pure Vertex <* char 'v' <* oneOrMoreSpaces) <*> (double <* oneOrMoreSpaces) <*>
  (double <* oneOrMoreSpaces) <*>
  double <*>
  (oneOrMoreSpaces *> double <|> pure 1)

vertexNormal :: Parser Vertex
vertexNormal =
  (pure Vertex <* string "vn" <* oneOrMoreSpaces) <*> (double <* oneOrMoreSpaces) <*>
  (double <* oneOrMoreSpaces) <*>
  double <*>
  (oneOrMoreSpaces *> double <|> pure 1)

textureCoordinate :: Parser TextureCoordinate
textureCoordinate =
  (pure TextureCoordinate <* string "vt" <* oneOrMoreSpaces) <*>
  (double <* oneOrMoreSpaces) <*>
  double <*>
  (oneOrMoreSpaces *> double <|> pure 0)

face :: Parser [FaceVertex]
face =
  do _ <- string "f"
     oneOrMoreSpaces
     sepBy1 faceVertex oneOrMoreSpaces

faceVertex :: Parser FaceVertex
faceVertex =
  (pure FaceVertex <*> decimal) <*>
  (pure Just <* char '/' <*> decimal) <*> 
  (pure Just <* char '/' <*> decimal)

commentToEOL :: Parser ()
commentToEOL = do
  skipMany (satisfy isHorizontalSpace)
  (do _ <- char '#'
      skipWhile (not . isEndOfLine)) <|> return ()
  lookAhead (endOfLine <|> endOfInput) <?> "Expected comment or end-of-line, but instead saw extra data"

oneOrMoreSpaces :: Parser ()
oneOrMoreSpaces = skipMany1 (satisfy isHorizontalSpace)
