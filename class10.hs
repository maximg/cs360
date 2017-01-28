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

data Value where
    VInt :: Integer -> Value
    VBool :: Bool -> Value
    deriving (Show, Eq)

type Env = M.Map String Value
type Ctx = M.Map String Type

data TypeError where
    UndefinedVar :: String -> TypeError
    -- expr, expected type, actual type
    TypeMismatch :: Arith -> Type -> Type -> TypeError
    deriving (Show)

showTypeError :: TypeError -> String
showTypeError (UndefinedVar v) = "Undefined variable '" ++ v ++ "'"
showTypeError (TypeMismatch e t1 t2) = "Type mismatch .........."

data InterpError where
    DivisonByZero :: InterpError
    deriving (Show)

showInterpError :: InterpError -> String
showInterpError (DivisonByZero)  = "Division by zero"

infer :: Ctx -> Arith -> Either TypeError Type
infer ctx (Lit _) = Right TInteger
infer ctx BTrue = Right TBool
infer ctx BFalse = Right TBool

check :: Ctx -> Arith -> Type -> Either TypeError ()
check ctx e t = do
    t1 <- infer ctx e
    if t == t1 then return ()
               else Left $ TypeMismatch e t t1

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

{-
eval :: String -> IO ()
eval s = case parse arith s of
  Left err  -> print err
  Right e -> case interpArith3 M.empty e of
    Left err -> putStrLn (showInterpError err)
    Right val -> print val
-}