{-# LANGUAGE GADTs #-}

module Calc
    ( description
    , helpMsg
    , calc
    ) where

import Prelude
import Parsing hiding ((<$>), (<$), (<*>), (<*), (*>))


description :: String
description = unlines
    [ "Welcome to Maxim's calculator."
    , "This is the best calculator you have ever experienced, period."
    , "Features this calculator supports: +, -, *, /, ^."
    , "You can specify length in km, m, mi, ft or in."
    , "You can convert from one units to other: 10 mi as km."
    , "Type an expression, :help, or :quit."
    ]

helpMsg :: String
helpMsg = unlines
    [ "You can use integers or floating point values,"
    , "negation, or standard arithmetic operators + - * / ^ ."
    ]

data Units where
    Meters :: Units
    Kilometers :: Units
    Miles :: Units
    Feet :: Units
    Inches :: Units
    deriving (Show)

data Value where
    Length :: Double -> Units -> Value
    Number :: Double -> Value
    deriving (Show)

data Arith where
  Lit :: Value -> Arith
  Add :: Arith -> Arith -> Arith
  Sub :: Arith -> Arith -> Arith
  Mul :: Arith -> Arith -> Arith
  Div :: Arith -> Arith -> Arith
  Exp :: Arith -> Arith -> Arith
  Neg :: Arith -> Arith
  -- As  :: Arith -> Units -> Arith
  deriving (Show)

lexer :: TokenParser u
lexer = makeTokenParser emptyDef

parens :: Parser a -> Parser a
parens     = getParens lexer

reservedOp :: String -> Parser ()
reservedOp = getReservedOp lexer

double :: Parser Double
double     = toDouble <$> getNaturalOrFloat lexer
    where
        toDouble (Left i) = fromIntegral i
        toDouble (Right f) = f

whiteSpace :: Parser ()
whiteSpace = getWhiteSpace lexer

parseArithAtom :: Parser Arith
parseArithAtom =
    try(parseDimValue) <|>
    (Lit <$> (Number <$> double)) <|> parens parseArith

parseDimValue :: Parser Arith
parseDimValue = Lit <$> (Length <$> double <*> parseUnits)

parseUnits :: Parser Units
parseUnits =
    (Miles      <$ reservedOp "mi") <|>
    (Meters     <$ reservedOp "m")  <|>
    (Kilometers <$ reservedOp "km") <|>
    (Feet       <$ reservedOp "ft") <|>
    (Inches     <$ reservedOp "in")

parseArith :: Parser Arith
parseArith = buildExpressionParser table parseArithAtom
  where
    table = [ [ Prefix (Neg <$ reservedOp "-") ]
            , [ Infix (Exp <$ reservedOp "^") AssocRight ]
            , [ Infix (Mul <$ reservedOp "*") AssocLeft
            ,   Infix (Div <$ reservedOp "/") AssocLeft ]
            , [ Infix (Add <$ reservedOp "+") AssocLeft
              , Infix (Sub <$ reservedOp "-") AssocLeft
              ]
            ]

arith :: Parser Arith
arith = whiteSpace *> parseArith <* eof


showArith :: Arith -> String
showArith (Lit (Number i)) = show i
showArith (Lit (Length l u)) = show l ++ " " ++ showUnits u
showArith (Add e1 e2) = showOp "+" e1 e2
showArith (Sub e1 e2) = showOp "-" e1 e2
showArith (Mul e1 e2) = showOp "*" e1 e2
showArith (Div e1 e2) = showOp "/" e1 e2
showArith (Exp e1 e2) = showOp "^" e1 e2
showArith (Neg e1)    = "-" ++ showArith e1

showOp :: String -> Arith -> Arith -> String
showOp op e1 e2 = showArith e1 ++ " " ++ op ++ " " ++ showArith e2

showUnits :: Units -> String
showUnits Meters     = "m"
showUnits Kilometers = "km"
showUnits Miles      = "mi"
showUnits Feet       = "ft"
showUnits Inches     = "in"


data InterpError where
    DivisionByZero :: InterpError
    deriving (Show)

showInterpError :: InterpError -> String
showInterpError DivisionByZero = "Division by zero"

data InferError where
    MismatchedUnits :: InferError
    MaxOneUnit :: InferError
    BadDivisorUnits :: InferError
    BadExpUnits :: InferError
    deriving (Show)

showInferError :: InferError -> String
showInferError MismatchedUnits = "Expression requires same units on both terms"
showInferError MaxOneUnit = "Not more than one term can be length"
showInferError BadDivisorUnits = "Only length can be divided by length"
showInferError BadExpUnits = "Exponent can not have units"

inferUnits :: Arith -> Either InferError (Maybe Units)
inferUnits (Lit (Number x)) = Right Nothing
inferUnits (Lit (Length _ u)) = Right (Just Meters)
inferUnits (Add e1 e2) = inferTerms e1 e2 addSubUnits
inferUnits (Sub e1 e2) = inferTerms e1 e2 addSubUnits
inferUnits (Mul e1 e2) = inferTerms e1 e2 mulUnits
inferUnits (Div e1 e2) = inferTerms e1 e2 divUnits
inferUnits (Exp e1 e2) = inferTerms e1 e2 expUnits

inferTerms :: Arith -> Arith -> (Maybe Units -> Maybe Units -> Either InferError (Maybe Units)) -> Either InferError (Maybe Units)
inferTerms e1 e2 f = do
    u1 <- inferUnits e1
    u2 <- inferUnits e2
    f u1 u2

addSubUnits, mulUnits, divUnits, expUnits :: Maybe Units -> Maybe Units -> Either InferError (Maybe Units)
addSubUnits (Just _) (Just _) = Right (Just Meters)
addSubUnits Nothing  Nothing  = Right Nothing
addSubUnits _        _        = Left MismatchedUnits

mulUnits (Just _) Nothing  = Right (Just Meters)
mulUnits Nothing  (Just _) = Right (Just Meters)
mulUnits Nothing  Nothing  = Right Nothing
mulUnits _        _        = Left MaxOneUnit

divUnits (Just _) (Just _) = Right Nothing
divUnits (Just _) Nothing  = Right (Just Meters)
divUnits Nothing  Nothing  = Right Nothing
divUnits _        _        = Left BadDivisorUnits

expUnits Nothing  Nothing  = Right Nothing
expUnits _        _        = Left BadExpUnits


-- Calculations are done in dimensionless numbers,
-- lengths are converted to meters
toNumber :: Value -> Double
toNumber (Number x)            = x
toNumber (Length l Meters)     = l
toNumber (Length l Kilometers) = l * 1000.0
toNumber (Length l Miles)      = l * 1609.344
toNumber (Length l Feet)       = l * 0.3048
toNumber (Length l Inches)     = l * 0.0254


interpArith :: Arith -> Either InterpError Double
interpArith (Lit i) = Right $ toNumber i
interpArith (Add e1 e2) = interpOp (+)  e1 e2
interpArith (Sub e1 e2) = interpOp (-)  e1 e2
interpArith (Mul e1 e2) = interpOp (*)  e1 e2
interpArith (Exp e1 e2) = interpOp (**) e1 e2
interpArith (Div e1 e2) = do
    y <- interpArith e2
    if y == 0.0 then Left DivisionByZero
                else interpArith e1 >>= (\x -> Right (x / y))
interpArith (Neg e1)    = (\x -> -x) <$> interpArith e1

interpOp :: (Double -> Double -> Double) -> Arith -> Arith -> Either InterpError Double
interpOp op e1 e2 = op <$> interpArith e1 <*> interpArith e2


calc :: String -> String
calc input = case parse arith input of
    Left err -> show err
    Right expr -> case inferUnits expr of
        Left inferErr -> showInferError inferErr
        Right units -> showArith expr ++ "\n" ++ interp expr ++ " " ++ showUnits' units
            where
                interp expr = case interpArith expr of
                    Left interpErr -> showInterpError interpErr
                    Right x ->  "  = " ++ (show x)
                showUnits' Nothing = ""
                showUnits' (Just l) = showUnits l
