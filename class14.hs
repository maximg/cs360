{-# LANGUAGE GADTs #-}

import Prelude
import Parsing2
import qualified Data.Map as M

data Expr where
    Var    :: String -> Expr
    Lit    :: Integer -> Expr
    Add    :: Expr -> Expr -> Expr
    Lambda :: Expr -> Maybe Type -> Expr -> Expr
    Apply  :: Expr -> Expr -> Expr
    deriving (Show)

data InterpError where
    UnknownError :: InterpError
    deriving (Show)

data Value where
    VInt :: Integer -> Value
    VClosure :: String -> Expr -> Env -> Value

type Env = M.Map String Value
type Ctx = M.Map String Type

data Type where
    TyInt :: Type
    TyFun :: Type -> Type -> Type
    deriving (Show, Eq)

data TypeError where
    RequireType :: Expr -> TypeError
    TypeMismatch :: Expr -> TypeError
    UndefinedVar :: String -> TypeError
    ExpectedIdent :: Expr -> TypeError
    deriving (Show)


showValue :: Value -> String
showValue (VInt i) = show i
showValue (VClosure name expr env) = "function " ++ name ++ ": " ++ show expr

showInterpError :: InterpError -> String
showInterpError _ = "unknown error"

showTypeError :: TypeError -> String
showTypeError (RequireType expr) = "Type annotation required in lambda " ++ show expr
showTypeError (TypeMismatch expr) = "Type mismatch in " ++ show expr
showTypeError (UndefinedVar name) = "Undefined variable " ++ name
showTypeError (ExpectedIdent _) = "Lambda parameter must be an identifier"


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
           <*> optionMaybe (reservedOp "[" *> parseType <* reservedOp "]")
           <*  reservedOp "->"
           <*> parseExpr

parseExpr :: Parser Expr
parseExpr = buildExpressionParser table parseAtom
  where
    table = [ [ Infix (Apply <$ reservedOp "") AssocLeft
              ]
            , [ Infix (Add <$ reservedOp "+") AssocLeft
              ]  
            ]

parseTypeAtom :: Parser Type
parseTypeAtom
    =   TyInt <$ reservedOp "Int"
    <|> parens parseType

parseType :: Parser Type
parseType = buildExpressionParser table parseTypeAtom
  where
    table = [ [ Infix (TyFun <$ reservedOp "->") AssocRight
              ]
            ]


expr :: Parser Expr
expr = whiteSpace *> parseExpr <* eof


infer :: Ctx -> Expr -> Either TypeError Type
infer _ (Lit i) = do return $ TyInt
infer ctx (Var name) = case M.lookup name ctx of
    Just t -> Right t
    Nothing -> Left $ UndefinedVar name
infer ctx (Add e1 e2) = do
    check ctx e1 TyInt
    check ctx e2 TyInt
    return TyInt
infer ctx e@(Lambda par t expr) = case par of
    (Var name) -> case t of
        Nothing -> Left $ RequireType e
        Just tIn -> do
            tOut <- infer (M.insert name tIn ctx) expr
            return $ TyFun tIn tOut
    otherwise -> Left $ ExpectedIdent expr
infer ctx e@(Apply e1 e2) = do
    (TyFun tIn tOut) <- infer ctx e1
    check ctx e2 tIn
    return tOut


check :: Ctx -> Expr -> Type -> Either TypeError ()
check _ e@(Lambda _ Nothing _) _ = Left $ RequireType e
check ctx expr t = do
    t' <- infer ctx expr
    if t' == t then Right ()
               else Left $ TypeMismatch expr 


interpC :: Env -> Expr -> Either InterpError Value
interpC env (Lit i) = do return $ VInt i
interpC env (Var name) = case M.lookup name env of
    Just val -> Right val
    Nothing -> error "Bug: type check failed"
interpC env (Add e1 e2) = do
    v1 <- interpC env e1
    v2 <- interpC env e2
    add v1 v2
    where
        add (VInt x) (VInt y) = Right $ VInt (x+y)
interpC env e@(Lambda par t e1) = case par of
    Var name -> Right $ VClosure name e1 env
    otherwise -> error "Bug: type check failed"
interpC env (Apply f e1) = do
    f' <- interpC env f >>= typeCheck
    interpC env e1 >>= f'
    where
        typeCheck (VClosure name expr env1) = 
            Right (\par -> interpC (M.insert name par env1) expr)
        typeCheck _ = error "Bug in the code, type checking failed"


eval :: String -> IO ()
eval s = case parse expr s of
    Left err  -> print err
    Right e -> case infer M.empty e of
        Left err -> putStrLn $ showTypeError err
        otherwise -> case interpC M.empty e of
            Left err -> putStrLn (showInterpError err)
            Right v -> putStrLn (showValue v)
