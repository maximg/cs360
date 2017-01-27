{-# LANGUAGE GADTs #-}

import Prelude hiding ((<$>), (<$), (<*>), (<*), (*>))
import Parsing

data Arith where
  Lit :: Integer -> Arith
  Bin :: Op -> Arith -> Arith -> Arith
  Var :: String -> Arith
  Let :: Arith -> Arith -> Arith -> Arith
  deriving (Show)

data Op where
  Plus  :: Op
  Minus :: Op
  Times :: Op
  deriving (Show, Eq)

interpArith :: Arith -> Integer
interpArith (Lit i)           = i
interpArith (Bin Plus e1 e2)  = interpArith e1 + interpArith e2
interpArith (Bin Minus e1 e2) = interpArith e1 - interpArith e2
interpArith (Bin Times e1 e2) = interpArith e1 * interpArith e2

lexer :: TokenParser u
lexer = makeTokenParser $ emptyDef
  { reservedNames = ["let", "in"] }
    -- tell the lexer that "let" and "in" are reserved keywords
    -- which may not be used as variable names

parens :: Parser a -> Parser a
parens     = getParens lexer

reservedOp :: String -> Parser ()
reservedOp = getReservedOp lexer

reserved :: String -> Parser ()
reserved = getReserved lexer

integer :: Parser Integer
integer    = getInteger lexer

whiteSpace :: Parser ()
whiteSpace = getWhiteSpace lexer

identifier :: Parser String
identifier = getIdentifier lexer

parseVar :: Parser Arith
parseVar = Var <$> identifier

parseArithAtom :: Parser Arith
parseArithAtom = (Lit <$> integer) <|> parens parseArith <|> parseLet <|> parseVar

parseLet :: Parser Arith
parseLet =
    Let <$  reserved "let"
        <*> parseVar
        <*  reserved "="
        <*> parseArith
        <*  reserved "in"
        <*> parseArith 

parseArith :: Parser Arith
parseArith = buildExpressionParser table parseArithAtom
  where
    table = [ [ Infix (Bin Times <$ reservedOp "*") AssocLeft ]
            , [ Infix (Bin Plus  <$ reservedOp "+") AssocLeft
              , Infix (Bin Minus <$ reservedOp "-") AssocLeft
              ]
            ]

arith :: Parser Arith
arith = whiteSpace *> parseArith <* eof

eval :: String -> Maybe Integer
eval s = case parse arith s of
  Left _  -> Nothing
  Right e -> Just (interpArith e)
