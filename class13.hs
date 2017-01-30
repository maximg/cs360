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

-- (^x -> x + 1) 2

data InterpError where
    UndefinedVar :: String -> InterpError
    TypeMismatch :: Expr -> InterpError
    ExpectedIdent :: Expr -> InterpError
    deriving (Show)

data Value where
    VInt :: Integer -> Value
    VClosure :: String -> Expr -> Env -> Value

type Env = M.Map String Value

data Type where
  TyInt :: Type
  TyFun :: Type -> Type -> Type
  deriving (Show)

showInterpError :: InterpError -> String
showInterpError (UndefinedVar name) = "Undefined variable " ++ name
showInterpError (TypeMismatch _) = "Unexpected type" -- FIXME
showInterpError (ExpectedIdent _) = "Lambda parameter must be an identifier"

showValue :: Value -> String
showValue (VInt i) = show i
showValue (VClosure name expr env) = "function " ++ name ++ ": " ++ show expr


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


interpC :: Env -> Expr -> Either InterpError Value
interpC env (Lit i) = do return $ VInt i
interpC env (Var name) = case M.lookup name env of
    Just val -> Right val
    Nothing -> Left $ UndefinedVar name
interpC env (Add e1 e2) = do
    v1 <- interpC env e1
    v2 <- interpC env e2
    add v1 v2
    where
        add (VInt x)        (VInt y)         = Right $ VInt (x+y)
        add (VClosure _ _ _) _               = Left $ TypeMismatch e1
        add _               (VClosure _ _ _) = Left $ TypeMismatch e2
interpC env e@(Lambda par t e1) = case par of
    Var name -> Right $ VClosure name e1 env
    otherwise -> Left $ ExpectedIdent e
interpC env (Apply f e1) = do
    f' <- interpC env f >>= typeCheck
    interpC env e1 >>= f'
    where
        typeCheck (VClosure name expr env1) = 
            Right (\par -> interpC (M.insert name par env1) expr)
        typeCheck (VInt _) = Left $ TypeMismatch f

eval :: String -> IO ()
eval s = case parse expr s of
    Left err  -> print err
    Right e -> case interpC M.empty e of
        Left err -> putStrLn (showInterpError err)
        Right v -> putStrLn (showValue v)
