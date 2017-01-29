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

data Type where
    TBool :: Type
    TInteger :: Type
    deriving (Show, Eq)

type Env = M.Map String Integer
type Ctx = M.Map String Type

data TypeError where
    UndefinedVar :: String -> TypeError
    -- expr, expected type, actual type
    TypeMismatch :: Arith -> Type -> Type -> TypeError
    -- if expr, type then, type else
    IfBranchesMismatch :: Arith -> Type -> Type -> TypeError
    deriving (Show)

showTypeError :: TypeError -> String
showTypeError (UndefinedVar v) = "Undefined variable '" ++ v ++ "'"
showTypeError (TypeMismatch e t1 t2) =
    "Type mismatch: expected " ++ show t1 ++
    " but got " ++ show t2 ++ " in expression " ++ showArith e
showTypeError (IfBranchesMismatch e t1 t2) =
    "Branches have different types: " ++ show t1 ++
    " and " ++ show t2 ++ " in expression " ++ showArith e

data InterpError where
    DivisonByZero :: InterpError
    deriving (Show)

showInterpError :: InterpError -> String
showInterpError (DivisonByZero)  = "Division by zero"


showOp :: Op -> String
showOp Plus  = "+"
showOp Minus = "-"
showOp Times = "*"
showOp Div   = "/"
showOp Less  = "<"
showOp Eq   = "=="


showArith :: Arith -> String
showArith (Lit i) = show i
showArith BTrue = "True"
showArith BFalse = "False"
showArith (Bin op e1 e2) = (showArith e1) ++ showOp op ++ (showArith e2)
showArith (Var x) = x
showArith (Let v e1 e2) = "let " ++ v ++ " = " ++ showArith e1 ++ " in " ++ (showArith e2)
showArith (If cond eThen eElse) =
    "if " ++ showArith cond ++
    " then " ++ showArith eThen ++
    " else " ++ showArith eElse


bopType :: Op -> (Type, Type, Type)
bopType Plus  = (TInteger, TInteger, TInteger)
bopType Minus = (TInteger, TInteger, TInteger)
bopType Times = (TInteger, TInteger, TInteger)
bopType Div   = (TInteger, TInteger, TInteger)
bopType Eq    = (TInteger, TInteger, TBool)
bopType Less  = (TInteger, TInteger, TBool)


infer :: Ctx -> Arith -> Either TypeError Type
infer ctx (Lit _) = Right TInteger
infer ctx BTrue = Right TBool
infer ctx BFalse = Right TBool
infer ctx (Bin op e1 e2) = do
    let (t1,t2, tOut) = bopType op
    check ctx e1 t1
    check ctx e2 t2
    return tOut
infer ctx (Var v) = case M.lookup v ctx of
                        Just x -> Right x
                        Nothing -> Left $ UndefinedVar v
infer ctx e@(If cond eThen eElse) = do
    check ctx cond TBool
    t1 <- infer ctx eThen
    t2 <- infer ctx eElse
    if t1 /= t2 then Left $ IfBranchesMismatch e t1 t2
                else Right t1
infer ctx (Let name val expr) = do
    t <- infer ctx val
    infer (M.insert name t ctx) expr

check :: Ctx -> Arith -> Type -> Either TypeError ()
check ctx e t = do
    t1 <- infer ctx e
    if t == t1 then return ()
               else Left $ TypeMismatch e t t1


inferArith :: Arith -> Either TypeError Type
inferArith = infer M.empty


interpArith :: Env -> Arith -> Either InterpError Integer
interpArith _ (Lit i) = Right i
interpArith _ BTrue = Right 1
interpArith _ BFalse = Right 0

interpArith env (Bin Plus  e1 e2) = (+) <$> interpArith env e1 <*> interpArith env e2
interpArith env (Bin Minus e1 e2) = (-) <$> interpArith env e1 <*> interpArith env e2
interpArith env (Bin Times e1 e2) = (*) <$> interpArith env e1 <*> interpArith env e2
interpArith env (Bin Div   e1 e2) =
    (interpArith env e2 >>= div') <*> interpArith env e1
    where
        div' 0 = Left DivisonByZero
        div' x = Right (\y -> x `div` y)

interpArith env (Bin Less e1 e2) = less <$> interpArith env e1 <*> interpArith env e2
    where less x y = if x < y then 1 else 0
interpArith env (Bin Eq   e1 e2) = eq <$> interpArith env e1 <*> interpArith env e2
    where eq x y = if x == y then 1 else 0

interpArith env (Var v) = case M.lookup v env of
                            Just x -> Right x
                            Nothing -> error "Bug: undefined var not caught by type check"
interpArith env (Let name val expr) =
    interpArith env val >>= (\x -> interpArith (M.insert name x env) expr)
interpArith env (If cond eTrue eFalse) = do
    cond' <- interpArith env cond
    if cond' == 1 then interpArith env eTrue
                  else interpArith env eFalse


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


showAsType :: Type -> Integer -> String
showAsType TBool 0 = "False"
showAsType TBool 1 = "True"
showAsType _ x = show x


eval :: String -> IO ()
eval s = case parse arith s of
    Left err  -> print err
    Right e -> case inferArith e of
        Left err -> putStrLn (showTypeError err)
        Right t -> case interpArith M.empty e of
            Left err -> putStrLn (showInterpError err)
            Right val -> putStrLn $ showAsType t val
