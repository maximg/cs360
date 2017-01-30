{-# LANGUAGE GADTs #-}

import Parsing2

data Expr where
    Var    :: String -> Expr
    Lit    :: Integer -> Expr
    Add    :: Expr -> Expr -> Expr
    Lambda :: Expr -> Expr -> Expr
    Apply  :: Expr -> Expr -> Expr
    deriving (Show)

-- (^x -> x + 1) 2

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

integer :: Parser Integer
integer = getInteger lexer

parseAtom :: Parser Expr
parseAtom
  =   Var  <$> identifier
  <|> Lit  <$> integer
  <|> parseLambda
  <|> parens parseExpr

parseLambda :: Parser Expr
parseLambda =
    Lambda <$  reservedOp "^"
           <*> (Var <$> identifier)
           <*  reservedOp "->"
           <*> parseExpr

parseAdd :: Parser Expr
parseAdd =
    Add <$> parseAtom 
        <*  reservedOp "+" 
         *> parseAtom

parseApply :: Parser Expr
parseApply =
    Apply <$> parseAtom
          <*> parseAtom


parseExpr :: Parser Expr
parseExpr = buildExpressionParser table parseAtom
  where
    table = [ [ Infix (Apply <$ whiteSpace) AssocLeft
              ]
            , [ Infix (Add <$ reservedOp "+") AssocLeft
              ]  
            ]

expr :: Parser Expr
expr = whiteSpace *> parseExpr <* eof

p :: String -> Expr
p s = case parse expr s of
  Left err -> error (show err)
  Right e  -> e