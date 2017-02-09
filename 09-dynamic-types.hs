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

data Value where
    VInt :: Integer -> Value
    VBool :: Bool -> Value
    deriving (Show, Eq)

type Env = M.Map String Value

data InterpError where
    UndefinedVar :: String -> InterpError
    DivisonByZero :: InterpError
    TypeError :: InterpError
    deriving (Show)

showInterpError :: InterpError -> String
showInterpError (UndefinedVar v) = "Undefined variable '" ++ v ++ "'"
showInterpError (DivisonByZero)  = "Division by zero"
showInterpError (TypeError)  = "Type error"


interpBool :: Env -> Arith -> Either InterpError Bool
interpBool env e = case interpArith3 env e of
    Right (VBool v) -> Right v
    Right _ -> Left TypeError
    Left e -> Left e

interpInt :: Env -> Arith -> Either InterpError Integer
interpInt env e = case interpArith3 env e of
    Right (VInt v) -> Right v
    Right _ -> Left TypeError
    Left e -> Left e

intOp2 :: Env -> (Integer -> Integer -> Integer) -> Arith -> Arith -> Either InterpError Value
intOp2 env op e1 e2 = do
    l <- interpInt env e1
    r <- interpInt env e2
    return $ VInt (op l r)

interpArith3 :: Env -> Arith -> Either InterpError Value
interpArith3 _ (Lit i) = Right (VInt i)
interpArith3 _ BTrue = Right (VBool True)
interpArith3 _ BFalse = Right (VBool False)
interpArith3 env (Var v) = case M.lookup v env of
                            Just x -> Right x
                            Nothing -> Left $ UndefinedVar v
interpArith3 env (Bin Plus  e1 e2) = intOp2 env (+) e1 e2
interpArith3 env (Bin Minus e1 e2) = intOp2 env (-) e1 e2
interpArith3 env (Bin Times e1 e2) = intOp2 env (*) e1 e2

interpArith3 env (Bin Div   e1 e2) = do
    denom <- interpInt env e2
    if denom == 0 then Left DivisonByZero
                  else do
                        num <- interpInt env e1
                        return $ VInt $ num `div` denom

interpArith3 env (Bin Less e1 e2) = do
    l <- interpInt env e1
    r <- interpInt env e2
    return $ VBool $ l < r
interpArith3 env (Bin Eq e1 e2) = do
    l <- interpInt env e1
    r <- interpInt env e2
    return $ VBool $ l == r

interpArith3 env (Let name val expr) =
    interpArith3 env val >>= (\x -> interpArith3 (M.insert name x env) expr)
interpArith3 env (If eCond eThen eElse) = do
    bCond <- interpBool env eCond
    if bCond then interpArith3 env eThen
             else interpArith3 env eElse


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
            , [ Prefix ((\x -> If x BFalse BTrue) <$ reservedOp "!")
              ]
            , [ Infix (Bin Less  <$ reservedOp "<") AssocNone
              , Infix (Bin Eq    <$ reservedOp "==") AssocNone
              , Infix ((\x y -> Bin Less y x)  <$ reservedOp ">") AssocNone
              , Infix ((\x y -> If (Bin Less y x) BFalse BTrue) <$ reservedOp "<=") AssocNone
              , Infix ((\x y -> If (Bin Less x y) BFalse BTrue) <$ reservedOp ">=") AssocNone
              , Infix ((\x y -> If (Bin Eq x y) BFalse BTrue)   <$ reservedOp "!=") AssocNone
              ]
            , [ Infix ((\x y -> If x y BFalse) <$ reservedOp "&&") AssocRight
              ]
            , [ Infix ((\x y -> If x BTrue y)  <$ reservedOp "||") AssocRight
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
