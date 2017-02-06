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

data Quilt where
    ColorLit :: Color -> Quilt
    NumberLit :: Double -> Quilt
    BoolLit :: Bool -> Quilt
    Triple :: Quilt -> Quilt -> Quilt -> Quilt
    Param :: Coord -> Quilt
    Add :: Quilt -> Quilt -> Quilt
    If :: Quilt -> Quilt -> Quilt -> Quilt
    deriving (Show)

data Coord where
    CoordX :: Coord
    CoordY :: Coord
    deriving (Show)


{- Parser
 -}

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
    Triple  <$  reservedOp "["
            <*> parseQuilt
            <*  reservedOp ","
            <*> parseQuilt
            <*  reservedOp ","
            <*> parseQuilt
            <*  reservedOp "]"

parseNumber :: Parser Quilt
parseNumber = NumberLit <$> double

parseBool :: Parser Quilt
parseBool =
        BoolLit True  <$ reservedOp "True"
    <|> BoolLit False <$ reservedOp "False"

parseIf :: Parser Quilt
parseIf =
    If  <$  reservedOp "if"
        <*> parseQuilt
        <*  reservedOp "then"
        <*> parseQuilt
        <*  reservedOp "else"
        <*> parseQuilt

parseQuiltAtom :: Parser Quilt
parseQuiltAtom =
        parseColorLit
    <|> parseCoord
    <|> parseTriple
    <|> parseNumber
    <|> parseBool
    <|> parseIf

parseQuilt :: Parser Quilt
parseQuilt = buildExpressionParser table parseQuiltAtom
  where
    table = [ [ Infix (Add <$ reservedOp "+") AssocLeft
              ]
            ]

quilt :: Parser Quilt
quilt = whiteSpace *> parseQuilt <* eof


{- Type check
 -}

data Type where
    TyColor :: Type
    TyNumber :: Type
    TyBool :: Type
    deriving (Show, Eq)

isSubtypeOf :: Type -> Type -> Bool
isSubtypeOf TyColor  TyColor  = True
isSubtypeOf TyNumber TyNumber = True
isSubtypeOf TyBool   TyBool   = True
isSubtypeOf TyNumber TyColor  = True
isSubtypeOf _        _        = False

commonType :: Type -> Type -> Maybe Type
commonType t1 t2 =
    if isSubtypeOf t1 t2
        then Just t2
        else if isSubtypeOf t2 t1
            then Just t1
            else Nothing

data InferError where
    TypeMismatch :: InferError
    BoolInArithm :: InferError
    ExpectedNumber :: InferError
    ExpectedBool :: InferError
    BadExpTypes :: InferError
    deriving (Show)

showInferError :: InferError -> String
showInferError TypeMismatch = "Type mismatch"
showInferError BoolInArithm = "Bool in an arithmetic expression"
showInferError ExpectedNumber = "Can only use numbers in a triplet"
showInferError ExpectedBool = "'if' condition must be a boolean"
showInferError BadExpTypes = "Exponent is only defined for numbers"

inferType :: Quilt -> Either InferError Type
inferType (ColorLit _)  = Right TyColor
inferType (NumberLit _) = Right TyNumber
inferType (BoolLit _)   = Right TyBool
inferType (Triple r g b) = do
    r' <- inferType r
    g' <- inferType g
    b' <- inferType b
    go r' g' b'
    where
        go TyNumber TyNumber TyNumber = Right TyColor
        go _ _ _ = Left ExpectedNumber
inferType (Param _) = Right TyNumber
inferType (If cond e1 e2) = do
    cond' <- inferType cond
    e1' <- inferType e1
    e2' <- inferType e2
    go cond' e1' e2'
    where
        go TyBool t1 t2 =
            if t1 == t2 then Right t1
                        else Left TypeMismatch
        go _ _ _ = Left ExpectedBool

inferType (Add e1 e2) = inferTerms e1 e2 inferArithm
{-
inferType (Sub e1 e2) = inferTerms e1 e2 inferAddSub
inferType (Mul e1 e2) = inferTerms e1 e2 inferMul
inferType (Div e1 e2) = inferTerms e1 e2 inferDiv
inferType (Exp e1 e2) = inferTerms e1 e2 inferExp
    where
        inferExp TyNumber TyNumber = Right TyNumber
        inferExp _        _        = Left BadExpTypes
-}

inferTerms :: Quilt -> Quilt -> (Type -> Type -> Either InferError Type) -> Either InferError Type
inferTerms e1 e2 f = do
    u1 <- inferType e1
    u2 <- inferType e2
    f u1 u2

inferArithm :: Type -> Type -> Either InferError Type
inferArithm t1 t2 = case commonType t1 t2 of
    Nothing -> Left TypeMismatch
    Just t -> case t of
        TyBool -> Left BoolInArithm
        _      -> Right t


{- Interpreter
 -}

data InterpError where
    DummyIntErr :: InterpError
    deriving (Show)

showInterpError :: InterpError -> String
showInterpError DummyIntErr = "undefined"

interpQuilt :: Quilt -> Either InterpError QuiltFun
interpQuilt (ColorLit c) = Right $ \x y -> c
interpQuilt (NumberLit z) = Right $ \x y -> [z,z,z]
interpQuilt (BoolLit z) = Right $ \x y -> [if z then 1 else 0]
interpQuilt (Triple r g b) = go <$> interpQuilt r <*> interpQuilt g <*> interpQuilt b
    where go r' g' b' = \x y -> [head $ r' x y, head $ g' x y, head $ b' x y]
interpQuilt (Param CoordX) = Right $ \x _ -> [x,x,x]
interpQuilt (Param CoordY) = Right $ \_ y -> [y,y,y]
interpQuilt (If cond e1 e2) = do
    cond' <- interpQuilt cond
    e1' <- interpQuilt e1
    e2' <- interpQuilt e2
    Right $ \x y -> if 1 == (head $ cond' x y) then e1' x y
                                               else e2' x y
interpQuilt (Add e1 e2) = addFn <$> interpQuilt e1 <*> interpQuilt e2
    where
        addFn f1 f2 = \x y -> vAdd (f1 x y) (f2 x y)
        vAdd = zipWith (+)


infer expr = case parse quilt expr of
    Left err -> show err
    Right expr -> case inferType expr of
        Left inferErr -> showInferError inferErr
        Right t -> show t

evalQuilt :: String -> Either String QuiltFun
evalQuilt s = case parse quilt s of
    Left err -> Left $ show err -- FIXME
    Right expr -> case inferType expr of
        Left inferErr -> Left $ showInferError inferErr
        Right _ -> case interpQuilt expr of
            Left interpErr -> Left $ showInterpError interpErr
            Right f -> Right f
