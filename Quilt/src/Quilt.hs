-- CSCI 360, Fall 2016
-- Project 3: the Quilt language
{-# LANGUAGE GADTs #-}

module Quilt where

import Prelude
import Parsing2

-- | A color is a list of red, green, and blue values between 0.0 - 1.0.
--   For example, [0,0,0] is black, [1,1,1] is white, [0.5, 0, 0.5] is a
--   darkish purple, and so on.
type Color = [Double]

-- | A quilt function produces a Color for any given location.  The
--   parameters are x and y coordinates in the range [-1,1].
type QuiltFun = Double -> Double -> Color


data InferError where
    Dummy :: InferError
    deriving (Show)

showInferError :: InferError -> String
showInferError Dummy = "undefined"

inferType :: Quilt -> Either InferError ()
inferType _ = Right () -- TODO


data InterpError where
    DummyIntErr :: InterpError
    deriving (Show)

showInterpError :: InterpError -> String
showInterpError DummyIntErr = "undefined"

interpQuilt :: Quilt -> Either InterpError QuiltFun
interpQuilt (ColorLit c) = Right $ \x y -> c

-- Parser
data Quilt where
    ColorLit :: Color -> Quilt
    deriving (Show)

toColor :: String -> Color
toColor "red"    = [1,   0,   0]
toColor "green"  = [0,   0.5, 0]
toColor "blue"   = [0,   0,   1]
toColor "black"  = [0,   0,   0]
toColor "white"  = [1,   1,   1]
toColor "yellow" = [1,   1,   0]
toColor "orange" = [1,   0.6, 0]
toColor "gray"   = [0.5, 0.5, 0.5]

lexer :: TokenParser u
lexer = makeTokenParser emptyDef

parens :: Parser a -> Parser a
parens = getParens lexer

identifier :: Parser String
identifier = getIdentifier lexer

reservedOp :: String -> Parser ()
reservedOp = getReservedOp lexer

whiteSpace :: Parser ()
whiteSpace = getWhiteSpace lexer

parseColorLit :: Parser Quilt
parseColorLit = (ColorLit . toColor) <$> identifier

parseQuilt :: Parser Quilt
parseQuilt = parseColorLit

quilt :: Parser Quilt
quilt = whiteSpace *> parseQuilt <* eof


evalQuilt :: String -> Either String QuiltFun
evalQuilt s = case parse quilt s of
    Left err -> Left $ show err -- FIXME
    Right expr -> case inferType expr of
        Left inferErr -> Left $ showInferError inferErr
        Right _ -> case interpQuilt expr of
            Left interpErr -> Left $ showInterpError interpErr
            Right f -> Right f
