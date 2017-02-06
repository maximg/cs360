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
interpQuilt (Param CoordX) = Right $ \x _ -> [x,x,x]
interpQuilt (Param CoordY) = Right $ \_ y -> [y,y,y]
interpQuilt (Add e1 e2) = addFn <$> interpQuilt e1 <*> interpQuilt e2
    where
        addFn f1 f2 = \x y -> vAdd (f1 x y) (f2 x y)
        vAdd = zipWith (+)

-- Parser
data Quilt where
    ColorLit :: Color -> Quilt
    Add :: Quilt -> Quilt -> Quilt
    Param :: Coord -> Quilt
    deriving (Show)

data Coord where
    CoordX :: Coord
    CoordY :: Coord
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

double :: Parser Double
double     = toDouble <$> getNaturalOrFloat lexer
    where
        toDouble (Left i) = fromIntegral i
        toDouble (Right f) = f

parseColorLit :: Parser Quilt
parseColorLit =
        makeColorLitParser "red"
    <|> makeColorLitParser "green"
    <|> makeColorLitParser "blue"
    <|> makeColorLitParser "black"
    <|> makeColorLitParser "white"
    <|> makeColorLitParser "yellow"
    <|> makeColorLitParser "orange"
    <|> makeColorLitParser "gray"

makeColorLitParser :: String -> Parser Quilt
makeColorLitParser s = (ColorLit $ toColor s) <$ reservedOp s 

parseCoord :: Parser Quilt
parseCoord =
        (Param CoordX) <$ reservedOp "x"
    <|> (Param CoordY) <$ reservedOp "y"

parseTriple :: Parser Quilt
parseTriple =
    mkColor <$  reservedOp "["
            <*> double
            <*  reservedOp ","
            <*> double
            <*  reservedOp ","
            <*> double
            <*  reservedOp "]"
    where
        mkColor r b g = ColorLit [r,g,b]

parseQuiltAtom :: Parser Quilt
parseQuiltAtom =
        parseColorLit
    <|> parseCoord
    <|> parseTriple

parseQuilt :: Parser Quilt
parseQuilt = buildExpressionParser table parseQuiltAtom
  where
    table = [ [ Infix (Add <$ reservedOp "+") AssocLeft
              ]  
            ]


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
