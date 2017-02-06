-- CSCI 360, Fall 2016
-- Project 3: the Quilt language
{-# LANGUAGE GADTs #-}

module Quilt where

import Prelude
import Parsing2
import Data.Maybe

-- | A color is a list of red, green, and blue values between 0.0 - 1.0.
--   For example, [0,0,0] is black, [1,1,1] is white, [0.5, 0, 0.5] is a
--   darkish purple, and so on.
type Color = [Double]

-- | A quilt function produces a Color for any given location.  The
--   parameters are x and y coordinates in the range [-1,1].
type QuiltFun = Double -> Double -> Color

data Op where
    Plus  :: Op
    Minus :: Op
    Times :: Op
    Div   :: Op
    Lt    :: Op
    Le    :: Op
    Eq    :: Op
    Ne    :: Op
    Gt    :: Op
    Ge    :: Op
    Neg   :: Op
    Not   :: Op
    And   :: Op
    Or    :: Op
    deriving (Show)

data Quilt where
    ColorLit :: Color -> Quilt
    NumberLit :: Double -> Quilt
    BoolLit :: Bool -> Quilt
    Triple :: Quilt -> Quilt -> Quilt -> Quilt
    Param :: Coord -> Quilt
    Bin :: Op -> Quilt -> Quilt -> Quilt
    Un :: Op -> Quilt -> Quilt
    If :: Quilt -> Quilt -> Quilt -> Quilt
    QuiltOp :: Quilt -> Quilt -> Quilt -> Quilt -> Quilt
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
lexer = makeTokenParser $ emptyDef
    { reservedNames = [
        "x", "y",
        "if", "then", "else",
        "red", "green", "blue", "black", "white", "yellow", "orange", "gray"
    ] }

parens :: Parser a -> Parser a
parens = getParens lexer

identifier :: Parser String
identifier = getIdentifier lexer

reservedOp :: String -> Parser ()
reservedOp = getReservedOp lexer

reserved :: String -> Parser ()
reserved = getReserved lexer

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
makeColorLitParser s = (ColorLit $ toColor s) <$ reserved s

parseCoord :: Parser Quilt
parseCoord =
        (Param CoordX) <$ reserved "x"
    <|> (Param CoordY) <$ reserved "y"

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

parseQuiltOp :: Parser Quilt
parseQuiltOp =
    QuiltOp <$  reservedOp "quilt"
            <*> parseQuilt
            <*> parseQuilt
            <*> parseQuilt
            <*> parseQuilt

parseQuiltAtom :: Parser Quilt
parseQuiltAtom =
        parseColorLit
    <|> parseCoord
    <|> parseTriple
    <|> parseNumber
    <|> parseBool
    <|> parseIf
    <|> parseQuiltOp
    <|> parens parseQuilt

parseQuilt :: Parser Quilt
parseQuilt = buildExpressionParser table parseQuiltAtom
  where
    table = [ [ Prefix (Un Neg   <$ reservedOp "-")
              ]
            , [ Infix (Bin Times <$ reservedOp "*") AssocLeft
              , Infix (Bin Div   <$ reservedOp "/") AssocLeft
              ]
            , [ Infix (Bin Plus  <$ reservedOp "+") AssocLeft
              , Infix (Bin Minus <$ reservedOp "-") AssocLeft
              ]
            , [ Prefix (Un Not   <$ reservedOp "!")
              ]
            , [ Infix (Bin Lt    <$ reservedOp "<")  AssocNone
              , Infix (Bin Eq    <$ reservedOp "==") AssocNone
              , Infix (Bin Gt    <$ reservedOp ">")  AssocNone
              , Infix (Bin Le    <$ reservedOp "<=") AssocNone
              , Infix (Bin Ge    <$ reservedOp ">=") AssocNone
              , Infix (Bin Ne    <$ reservedOp "!=") AssocNone
              ]
            , [ Infix (Bin And   <$ reservedOp "&&") AssocRight
              ]
            , [ Infix (Bin Or    <$ reservedOp "||") AssocRight
              ]
            ]

quilt :: Parser Quilt
quilt = whiteSpace *> parseQuilt <* eof


{- Desugar
-}

desugar :: Quilt -> Quilt
desugar (Un Not e1)     = (If e1 (BoolLit False) (BoolLit True))
desugar (Bin And e1 e2) = (If e1 e2 (BoolLit False))
desugar (Bin Or e1 e2)  = (If e1 (BoolLit True) e2)
desugar (Bin Gt e1 e2)  = (Bin Lt e2 e1)
desugar (Bin Ne e1 e2)  = (If (Bin Eq e1 e2) (BoolLit False) (BoolLit True))
desugar (Bin Le e1 e2)  = (If (Bin Lt e2 e1) (BoolLit False) (BoolLit True))
desugar (Bin Ge e1 e2)  = (If (Bin Lt e1 e2) (BoolLit False) (BoolLit True))
desugar v = v


{- Type check
 -}

data Type where
    TyColor :: Type
    TyNumber :: Type
    TyBool :: Type
    deriving (Show, Eq)

-- checks if the first type is a subtype of the second one
isSubtypeOf :: Type -> Type -> Bool
isSubtypeOf TyColor  TyColor  = True
isSubtypeOf TyNumber TyNumber = True
isSubtypeOf TyBool   TyBool   = True
isSubtypeOf TyNumber TyColor  = True
isSubtypeOf _        _        = False

commonType :: Type -> Type -> Maybe Type
commonType t1 t2 = isSubtypeOfM t1 t2 <|> isSubtypeOfM t2 t1
    where
        isSubtypeOfM st t = if isSubtypeOf st t then Just t
                                                else Nothing

data InferError where
    TypeMismatch :: InferError
    BoolInArithm :: InferError
    ExpectedNumber :: InferError
    ExpectedBool :: InferError
    deriving (Show)

showInferError :: InferError -> String
showInferError TypeMismatch = "Type mismatch"
showInferError BoolInArithm = "Bool in an arithmetic expression"
showInferError ExpectedNumber = "Can only use numbers in a triplet"
showInferError ExpectedBool = "'if' condition must be a boolean"

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
inferType (QuiltOp q1 q2 q3 q4) = do
    t1 <- inferType q1
    t2 <- inferType q2
    t3 <- inferType q3
    t4 <- inferType q4
    case commonType4 t1 t2 t3 t4 of
        Nothing -> Left TypeMismatch
        Just t -> Right t
    where
        commonType4 q1 q2 q3 q4 = do
            t12 <- commonType q1 q2
            t34 <- commonType q3 q4
            commonType t12 t34

inferType (Bin Plus e1 e2)  = checkType2 TyColor e1 e2
inferType (Bin Minus e1 e2) = checkType2 TyColor e1 e2
inferType (Bin Times e1 e2) = checkType2 TyColor e1 e2
inferType (Bin Div e1 e2)   = checkType2 TyColor e1 e2

inferType (Bin Lt e1 e2)    = checkType2 TyNumber e1 e2 *> pure TyBool
inferType (Bin Eq e1 e2)    = checkType2 TyNumber e1 e2 *> pure TyBool

inferType (Un Neg e1)       = inferType e1 >>= (\x ->
        if x == TyBool  then Left BoolInArithm
                        else Right x
    )

-- check if types of subexpressions are (sub)type of type
-- return the common type of subexpressions
checkType2 :: Type -> Quilt -> Quilt -> Either InferError Type
checkType2 t e1 e2 = do
    t1 <- inferType e1
    t2 <- inferType e2
    let t' = commonType t1 t2
    case flip isSubtypeOf t <$> t' of
        Just True -> Right $ fromJust t'
        _         -> Left TypeMismatch


{- Interpreter
 -}

data InterpError where
    DummyIntErr :: InterpError
    deriving (Show)

showInterpError :: InterpError -> String
showInterpError DummyIntErr = "undefined"

toBool :: Color -> Bool
toBool (x:xs) = x == 1.0

fromBool :: Bool -> Color
fromBool True  = [1, 1, 1]
fromBool False = [0, 0, 0]

interpQuilt :: Quilt -> Either InterpError QuiltFun
interpQuilt (ColorLit c) = Right $ \x y -> c
interpQuilt (NumberLit z) = Right $ \x y -> [z,z,z]
interpQuilt (BoolLit z) = Right $ \x y -> fromBool z
interpQuilt (Triple r g b) = go <$> interpQuilt r <*> interpQuilt g <*> interpQuilt b
    where go r' g' b' = \x y -> [head $ r' x y, head $ g' x y, head $ b' x y]
interpQuilt (Param CoordX) = Right $ \x _ -> [x,x,x]
interpQuilt (Param CoordY) = Right $ \_ y -> [y,y,y]
interpQuilt (If cond e1 e2) = do
    cond' <- interpQuilt cond
    e1' <- interpQuilt e1
    e2' <- interpQuilt e2
    Right $ \x y -> if toBool $ cond' x y then e1' x y
                                          else e2' x y
interpQuilt (QuiltOp q1 q2 q3 q4) = do
    q1' <- interpQuilt q1
    q2' <- interpQuilt q2
    q3' <- interpQuilt q3
    q4' <- interpQuilt q4
    Right $ \x y ->
        if x < 0 then if y >= 0 then q1' (x*2 + 1) (y*2 - 1)
                                else q3' (x*2 + 1) (y*2 + 1)
                 else if y >= 0 then q2' (x*2 - 1) (y*2 - 1)
                                else q4' (x*2 - 1) (y*2 + 1)

interpQuilt (Un Neg e1) = go <$> interpQuilt e1
    where go f1 = \x y -> map (\z -> -z) (f1 x y)

interpQuilt (Bin op e1 e2) = runBin op <$> interpQuilt e1 <*> interpQuilt e2
    where
        runBin Plus  = applyArithm (+)
        runBin Minus = applyArithm (-)
        runBin Times = applyArithm (*)
        runBin Div   = applyArithm (/)
        runBin Lt    = applyComp (<)
        runBin Eq    = applyComp (==)
        applyArithm op f1 f2 = \x y -> zipWith op (f1 x y) (f2 x y)
        applyComp op f1 f2 = \x y ->
            fromBool $ op (f1 x y) (f2 x y)


infer expr = case desugar <$> parse quilt expr of
    Left err -> show err
    Right expr -> case inferType expr of
        Left inferErr -> showInferError inferErr
        Right t -> show t

evalQuilt :: String -> Either String QuiltFun
evalQuilt s = case parse quilt s of
    Left err -> Left $ show err -- FIXME
    Right expr -> let desugared = desugar expr in
        case inferType desugared of
        Left inferErr -> Left $ showInferError inferErr
        Right _ -> case interpQuilt desugared of
            Left interpErr -> Left $ showInterpError interpErr
            Right f -> Right f
