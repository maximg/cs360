{-# LANGUAGE GADTs #-}

import Prelude
import Parsing hiding ((<$>), (<$), (<*>), (<*), (*>))
import qualified Data.Map as M

data Arith where
    Lit :: Integer -> Arith
    Bin :: Op -> Arith -> Arith -> Arith
    Var :: String -> Arith
    Let :: String -> Arith -> Arith -> Arith
    BFalse :: Arith
    BTrue  :: Arith
    If  :: Arith -> Arith -> Arith -> Arith
    deriving (Show)

data Op where
    Plus  :: Op
    Minus :: Op
    Times :: Op
    Div   :: Op
    Less  :: Op
    Eq    :: Op
    deriving (Show, Eq)

type Env = M.Map String Integer

data InterpError where
    UndefinedVar :: String -> InterpError
    DivisonByZero :: InterpError
    deriving (Show)

showInterpError :: InterpError -> String
showInterpError (UndefinedVar v) = "Undefined variable '" ++ v ++ "'"
showInterpError (DivisonByZero)  = "Division by zero"


{-
interpArith3 :: Env -> Arith -> Either InterpError Integer
interpArith3 _ (Lit i) = Right i
interpArith3 env (Var v) = case M.lookup v env of
                            Just x -> Right x
                            Nothing -> Left $ UndefinedVar v
interpArith3 env (Bin Plus  e1 e2) = (+) <$> interpArith3 env e1 <*> interpArith3 env e2
interpArith3 env (Bin Minus e1 e2) = (-) <$> interpArith3 env e1 <*> interpArith3 env e2
interpArith3 env (Bin Times e1 e2) = (*) <$> interpArith3 env e1 <*> interpArith3 env e2
interpArith3 env (Bin Div   e1 e2) =
    (interpArith3 env e2 >>= div') <*> interpArith3 env e1
    where
        div' 0 = Left DivisonByZero
        div' x = Right (\y -> x `div` y)
interpArith3 env (Let name val expr) =
    interpArith3 env val >>= (\x -> interpArith3 (M.insert name x env) expr)
-}

lexer :: TokenParser u
lexer = makeTokenParser $ emptyDef
  { reservedNames = ["let", "in", "True", "False", "if", "then", "else"] }
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
parseArithAtom =
    (Lit <$> integer) <|> 
    parens parseArith <|> 
    parseLet <|> 
    parseVar <|>
    parseIf <|>
    (BTrue  <$ reserved "True") <|>
    (BFalse <$ reserved "False")

parseLet :: Parser Arith
parseLet =
    Let <$  reserved "let"
        <*> identifier
        <*  reserved "="
        <*> parseArith
        <*  reserved "in"
        <*> parseArith 

parseIf :: Parser Arith
parseIf =
    If  <$  reserved "if"
        <*> parseArith
        <*  reserved "then"
        <*> parseArith
        <*  reserved "else"
        <*> parseArith 

parseArith :: Parser Arith
parseArith = buildExpressionParser table parseArithAtom
  where
    table = [ [ Infix (Bin Times <$ reservedOp "*") AssocLeft
              , Infix (Bin Div   <$ reservedOp "/") AssocLeft
              ]
            , [ Infix (Bin Plus  <$ reservedOp "+") AssocLeft
              , Infix (Bin Minus <$ reservedOp "-") AssocLeft
              ]
            , [ Infix (Bin Less  <$ reservedOp "<") AssocNone
              , Infix (Bin Eq    <$ reservedOp "==") AssocNone
              ]
            ]

arith :: Parser Arith
arith = whiteSpace *> parseArith <* eof

{-
eval :: String -> IO ()
eval s = case parse arith s of
  Left err  -> print err
  Right e -> case interpArith3 M.empty e of
    Left err -> putStrLn (showInterpError err)
    Right val -> print val
    -}
