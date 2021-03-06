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

data Type where
    Number :: Type
    Length :: Units -> Type
    deriving (Show)

data Value where
    Value :: Double -> Type -> Value
    deriving (Show)

data Arith where
  Lit :: Value -> Arith
  Add :: Arith -> Arith -> Arith
  Sub :: Arith -> Arith -> Arith
  Mul :: Arith -> Arith -> Arith
  Div :: Arith -> Arith -> Arith
  Exp :: Arith -> Arith -> Arith
  Neg :: Arith -> Arith
  As  :: Arith -> Units -> Arith
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
        try parseLength
    <|> (Lit <$> (Value <$> double <*> pure Number))
    <|> try parseCast
    <|> parens parseArith


parseLength :: Parser Arith
parseLength = Lit <$> (Value <$> double <*> (Length <$> parseUnits))

parseCast :: Parser Arith
parseCast = As <$> (parens parseArith) <* reservedOp "as" <*> parseUnits

parseUnits :: Parser Units
parseUnits =
    (Miles      <$ reservedOp "miles")   <|>
    (Miles      <$ reservedOp "mile")    <|>
    (Miles      <$ reservedOp "mi")      <|>
    (Meters     <$ reservedOp "meters")  <|>
    (Meters     <$ reservedOp "meter")   <|>
    (Meters     <$ reservedOp "m")       <|>
    (Kilometers <$ reservedOp "km")      <|>
    (Feet       <$ reservedOp "foot")    <|>
    (Feet       <$ reservedOp "feet")    <|>
    (Feet       <$ reservedOp "ft")      <|>
    (Inches     <$ reservedOp "inches")  <|>
    (Inches     <$ reservedOp "inch")    <|>
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
showArith (Lit v) = showValue v
showArith (Add e1 e2) = showOp "+" e1 e2
showArith (Sub e1 e2) = showOp "-" e1 e2
showArith (Mul e1 e2) = showOp "*" e1 e2
showArith (Div e1 e2) = showOp "/" e1 e2
showArith (Exp e1 e2) = showOp "^" e1 e2
showArith (Neg e1)    = "-" ++ showArith e1
showArith (As  e1 u)  = "(" ++ showArith e1 ++ ") as " ++ showUnits u

showOp :: String -> Arith -> Arith -> String
showOp op e1 e2 = showArith e1 ++ " " ++ op ++ " " ++ showArith e2

showValue :: Value -> String
showValue (Value x Number)     = show x
showValue (Value x (Length u)) = show x ++ " " ++ showUnits u

showUnits :: Units -> String
showUnits Meters     = "m"
showUnits Kilometers = "km"
showUnits Miles      = "mi"
showUnits Feet       = "ft"
showUnits Inches     = "in"


data InferError where
    MismatchedUnits :: InferError
    MaxOneUnit :: InferError
    BadDivisorUnits :: InferError
    BadExpTypes :: InferError
    InvalidCast :: InferError
    deriving (Show)

showInferError :: InferError -> String
showInferError MismatchedUnits = "Expression requires same units on both terms"
showInferError MaxOneUnit = "Not more than one term can be length"
showInferError BadDivisorUnits = "Only length can be divided by length"
showInferError BadExpTypes = "Exponent can not have units"
showInferError InvalidCast = "Cannot cast value without units"

inferType :: Arith -> Either InferError Type
inferType (Lit (Value _ t)) = Right t
inferType (Add e1 e2) = inferTerms e1 e2 inferAddSub
inferType (Sub e1 e2) = inferTerms e1 e2 inferAddSub
inferType (Mul e1 e2) = inferTerms e1 e2 inferMul
inferType (Div e1 e2) = inferTerms e1 e2 inferDiv
inferType (Exp e1 e2) = inferTerms e1 e2 inferExp
inferType (As  e1 u)  = inferType e1 >>= inferCast
    where
        inferCast Number = Left InvalidCast
        inferCast _      = Right (Length u)

inferTerms :: Arith -> Arith -> (Type -> Type -> Either InferError Type) -> Either InferError Type
inferTerms e1 e2 f = do
    u1 <- inferType e1
    u2 <- inferType e2
    f u1 u2

inferAddSub, inferMul, inferDiv, inferExp :: Type -> Type -> Either InferError Type

inferAddSub (Length u) (Length _) = Right (Length u)
inferAddSub Number     Number     = Right Number
inferAddSub _          _          = Left MismatchedUnits

inferMul (Length u) Number     = Right (Length u)
inferMul Number     (Length u) = Right (Length u)
inferMul Number     Number     = Right Number
inferMul _          _          = Left MaxOneUnit

inferDiv (Length _) (Length _) = Right Number
inferDiv (Length u) Number     = Right (Length u)
inferDiv Number     Number     = Right Number
inferDiv _          _          = Left BadDivisorUnits

inferExp Number  Number  = Right Number
inferExp _       _       = Left BadExpTypes


-- Calculations are done in typeless doubles,
-- typed values are converted to base units (if any)

data InterpError where
    DivisionByZero :: InterpError
    deriving (Show)

showInterpError :: InterpError -> String
showInterpError DivisionByZero = "Division by zero"

inBaseUnits :: Units -> Double
inBaseUnits Meters     =    1.0
inBaseUnits Kilometers = 1000.0
inBaseUnits Miles      = 1609.344
inBaseUnits Feet       =    0.3048
inBaseUnits Inches     =    0.0254

toNumber :: Value -> Double
toNumber (Value x Number)     = x
toNumber (Value x (Length u)) = x * inBaseUnits u

toValue :: Double -> Type -> Value
toValue x Number     = Value x Number
toValue l (Length u) = Value (l / (inBaseUnits u)) (Length u)

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
interpArith (As e1 _)   = interpArith e1

interpOp :: (Double -> Double -> Double) -> Arith -> Arith -> Either InterpError Double
interpOp op e1 e2 = op <$> interpArith e1 <*> interpArith e2


calc :: String -> String
calc input = case parse arith input of
    Left err -> show err
    Right expr -> case inferType expr of
        Left inferErr -> showInferError inferErr
        Right units ->  showArith expr ++ "\n" ++
                        showResult (showAs units <$> interpArith expr)
        where
            showResult (Left interpErr) = showInterpError interpErr
            showResult (Right str)      = "  = " ++ str
            showAs u x = showValue (toValue x u)
