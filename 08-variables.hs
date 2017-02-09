{-# LANGUAGE GADTs #-}

import Prelude
import Parsing hiding ((<$>), (<$), (<*>), (<*), (*>))
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
  Div   :: Op
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

data InterpError where
    UndefinedVar :: String -> InterpError
    DivisonByZero :: InterpError
    deriving (Show)

showInterpError :: InterpError -> String
showInterpError (UndefinedVar v) = "Undefined variable '" ++ v ++ "'"
showInterpError (DivisonByZero)  = "Division by zero"


interpArith2 :: Env -> Arith -> Either InterpError Integer
interpArith2 _ (Lit i) = Right i
interpArith2 env (Bin Plus e1 e2) = 
    case (interpArith2 env e1, interpArith2 env e2) of
        (Right v1', Right v2') -> Right (v1' + v2')
        (Left err, _) -> Left err
        (_, Left err) -> Left err
interpArith2 env (Var v) = case M.lookup v env of
                            Just x -> Right x
                            Nothing -> Left $ UndefinedVar v
interpArith2 env (Let name val expr) =
    case interpArith2 env val of
        (Right val') -> interpArith2 (M.insert name val' env) expr
        err -> err

{-
(<<$>>) :: (a -> b) -> Either e a -> Either e b
(<<$>>) _ (Left e) = Left e
(<<$>>) f (Right v) = Right (f v)

(<<*>>) :: Either e (a -> b) -> Either e a -> Either e b
(<<*>>) (Left e) _ = Left e
(<<*>>) _ (Left e) = Left e
(<<*>>) (Right f) (Right v) = Right (f v)

(>>>=) :: Either e a -> (a -> Either e b) -> Either e b
Left e1 >>>= _ = Left e1
Right a >>>= f = f a
-}

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
    table = [ [ Infix (Bin Times <$ reservedOp "*") AssocLeft
              , Infix (Bin Div   <$ reservedOp "/") AssocLeft
              ]
            , [ Infix (Bin Plus  <$ reservedOp "+") AssocLeft
              , Infix (Bin Minus <$ reservedOp "-") AssocLeft
              ]
            ]

arith :: Parser Arith
arith = whiteSpace *> parseArith <* eof

eval :: String -> IO ()
eval s = case parse arith s of
  Left err  -> print err
  Right e -> case interpArith3 M.empty e of
    Left err -> putStrLn (showInterpError err)
    Right val -> print val
