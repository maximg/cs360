{-# LANGUAGE GADTs #-}

import Prelude hiding ((<$>), (<$), (<*>), (<*), (*>))
import Parsing
import qualified Data.Map as M

data Arith where
  Lit :: Integer -> Arith
  Bin :: Op -> Arith -> Arith -> Arith
  Var :: String -> Arith
  Let :: String -> Arith -> Arith -> Arith
  deriving (Show)

data Op where
  Plus  :: Op
  Minus :: Op
  Times :: Op
  deriving (Show, Eq)

type Env = M.Map String Integer

interpArith :: Env -> Arith -> Integer
interpArith _ (Lit i)           = i
interpArith env (Bin Plus e1 e2)  = interpArith env e1 + interpArith env e2
interpArith env (Bin Minus e1 e2) = interpArith env e1 - interpArith env e2
interpArith env (Bin Times e1 e2) = interpArith env e1 * interpArith env e2
interpArith env (Var v) = case M.lookup v env of
                            Just x -> x
                            Nothing -> error $ "Undefined variable: " ++ v
interpArith env (Let name val expr) = interpArith (M.insert name val' env) expr
    where
        val' = interpArith env val


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
        <*> identifier
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
  Right e -> Just (interpArith M.empty e)
