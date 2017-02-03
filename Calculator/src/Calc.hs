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
    , "Type an expression, :help, or :quit."
    ]

helpMsg :: String
helpMsg = unlines
    [ "You can use integers or floating point values,"
    , "negation, or standard arithmetic operators + - * / ^ ."
    ]

type Value = Double

data Arith where
  Lit :: Value -> Arith
  Add :: Arith -> Arith -> Arith
  Sub :: Arith -> Arith -> Arith
  Mul :: Arith -> Arith -> Arith
  Div :: Arith -> Arith -> Arith
  Exp :: Arith -> Arith -> Arith
  Neg :: Arith -> Arith
  deriving (Show)

lexer :: TokenParser u
lexer = makeTokenParser emptyDef

parens :: Parser a -> Parser a
parens     = getParens lexer

reservedOp :: String -> Parser ()
reservedOp = getReservedOp lexer

double :: Parser Value
double     = toDouble <$> getNaturalOrFloat lexer
    where
        toDouble (Left i) = fromIntegral i
        toDouble (Right f) = f

whiteSpace :: Parser ()
whiteSpace = getWhiteSpace lexer

parseArithAtom :: Parser Arith
parseArithAtom = (Lit <$> double) <|> parens parseArith

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
showArith (Lit i) = show i
showArith (Add e1 e2) = showOp "+" e1 e2
showArith (Sub e1 e2) = showOp "-" e1 e2
showArith (Mul e1 e2) = showOp "*" e1 e2
showArith (Div e1 e2) = showOp "/" e1 e2
showArith (Exp e1 e2) = showOp "^" e1 e2
showArith (Neg e1)    = "-" ++ showArith e1

showOp :: String -> Arith -> Arith -> String
showOp op e1 e2 = showArith e1 ++ " " ++ op ++ " " ++ showArith e2

data InterpError where
    DivisionByZero :: InterpError
    deriving (Show)

showInterpError :: InterpError -> String
showInterpError DivisionByZero = "Division by zero"

interpArith :: Arith -> Either InterpError Value
interpArith (Lit i) = Right i
interpArith (Add e1 e2) = interpOp (+)  e1 e2
interpArith (Sub e1 e2) = interpOp (-)  e1 e2
interpArith (Mul e1 e2) = interpOp (*)  e1 e2
interpArith (Exp e1 e2) = interpOp (**) e1 e2
interpArith (Div e1 e2) = do
    y <- interpArith e2
    if y == 0.0 then Left DivisionByZero
                else interpArith e1 >>= (\x -> Right (x / y))
interpArith (Neg e1)    = (\x -> -x) <$> interpArith e1

interpOp :: (Value -> Value -> Value) -> Arith -> Arith -> Either InterpError Value 
interpOp op e1 e2 = op <$> interpArith e1 <*> interpArith e2


calc :: String -> String
calc input = case parse arith input of
    Left err -> show err
    Right expr -> showArith expr ++ "\n" ++ interp expr
    where
        interp expr = case interpArith expr of
            Left interpErr -> showInterpError interpErr
            Right x ->  "  = " ++ (show x)
