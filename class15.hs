{-# LANGUAGE GADTs        #-}

import           Parsing2

import qualified Data.Map           as M
import           Text.Read          (readMaybe)
import           System.Environment (getArgs)

type Var = String

type Prog = [Stmt]

data Type where
  TyInt  :: Type
  TyBool :: Type
  deriving (Show, Eq)

data Stmt where
  Decl   :: Type -> Var -> Stmt           -- <type> <var>
  Assign :: Var  -> Expr -> Stmt          -- <var> ':=' <expr>
  Block  :: Prog -> Stmt                  -- '{' <prog> '}'
  If     :: Expr -> Stmt -> Stmt -> Stmt  -- 'if' <expr> 'then' <stmt> 'else' <stmt>
  Repeat :: Expr -> Stmt -> Stmt          -- 'repeat' <expr> <stmt>
  While  :: Expr -> Stmt -> Stmt          -- 'while' <expr> <stmt>
  Input  :: Var  -> Stmt                  -- 'input' <var>
  Output :: Expr -> Stmt                  -- 'output' <expr>
  deriving Show

data Expr where
  EInt  :: Integer -> Expr                -- <int>
  EBool :: Bool    -> Expr                -- 'False' | 'True'
  EVar  :: Var -> Expr                    -- <var>
  EUn   :: UOp -> Expr -> Expr            -- <uop> <expr>
  EBin  :: BOp -> Expr -> Expr -> Expr    -- <expr> <bop> <expr>
  deriving Show

data UOp = Neg | Not
  deriving (Show, Eq)

data BOp = Add | Sub | Mul | Div | And | Or | Equals | Less
  deriving (Show, Eq)

lexer :: TokenParser u
lexer = makeTokenParser $
  emptyDef
  { reservedNames   = [ "True", "False", "if", "then", "else", "begin", "end"
                        , "repeat", "while", "input", "output", "int", "bool" ]
  , reservedOpNames = [ ":=", "==", "<", "+", "-", "*", "!", "&&", "||"  ]
  }

parens :: Parser a -> Parser a
parens = getParens lexer

reserved, reservedOp :: String -> Parser ()
reserved   = getReserved lexer
reservedOp = getReservedOp lexer

symbol :: String -> Parser String
symbol = getSymbol lexer

ident :: Parser String
ident = getIdentifier lexer

integer :: Parser Integer
integer = getInteger lexer

whiteSpace :: Parser ()
whiteSpace = getWhiteSpace lexer

parseAtom :: Parser Expr
parseAtom
  =   EInt        <$> integer
  <|> EBool True  <$  reserved "True"
  <|> EBool False <$  reserved "False"
  <|> EVar        <$> ident
  <|> parens parseExpr

parseExpr :: Parser Expr
parseExpr = buildExpressionParser table parseAtom
  where
    table = [ [ unary  "!"  (EUn Not) ]
            , [ unary  "-"  (EUn Neg) ]
            , [ binary "*"  (EBin Mul)    AssocLeft
              , binary "/"  (EBin Div)    AssocLeft ]
            , [ binary "+"  (EBin Add)    AssocLeft
              , binary "-"  (EBin Sub)    AssocLeft
              ]
            , [ binary "==" (EBin Equals) AssocNone
              , binary "<"  (EBin Less)   AssocNone
              ]
            , [ binary "&&" (EBin And)    AssocRight ]
            , [ binary "||" (EBin Or)     AssocRight ]
            ]
    unary  name fun       = Prefix (fun <$ reservedOp name)
    binary name fun assoc = Infix  (fun <$ reservedOp name) assoc

parseProg :: Parser Prog
parseProg = parseStmt `sepBy` (reservedOp ";")

parseStmt :: Parser Stmt
parseStmt =
      parseBlock
  <|> If      <$> (reserved "if" *> parseExpr)
              <*> (reserved "then" *> parseStmt)
              <*> (reserved "else" *> parseStmt)
  <|> Repeat  <$> (reserved "repeat" *> parseExpr) <*> parseBlock
  <|> While   <$> (reserved "while" *> parseExpr)  <*> parseBlock
  <|> Input   <$> (reserved "input" *> ident)
  <|> Output  <$> (reserved "output" *> parseExpr)
  <|> Assign  <$> ident <*> (reservedOp ":=" *> parseExpr)
  <|> Decl    <$> parseType <*> ident

parseType :: Parser Type
parseType = (TyInt <$ reserved "int") <|> (TyBool <$ reserved "bool")

parseBlock :: Parser Stmt
parseBlock = Block  <$> (symbol "{" *> parseProg <* symbol "}")

impParser :: Parser Prog
impParser = whiteSpace *> parseProg <* eof

data TypeError where
  DuplicateVar :: Var -> TypeError
  UnboundVar   :: Var  -> TypeError
  Mismatch     :: Expr -> Type -> Type -> TypeError
  InputBool    :: Var  -> TypeError
  deriving Show

type Ctx = M.Map Var Type

infer :: Ctx -> Expr -> Either TypeError Type
infer _   (EInt _)        = Right TyInt
infer _   (EBool _)       = Right TyBool
infer ctx (EVar x)        =
  case M.lookup x ctx of
    Nothing -> Left $ UnboundVar x
    Just ty -> Right ty
infer ctx (EBin op e1 e2) = inferBin ctx op e1 e2
infer ctx (EUn op e)      = inferUn ctx op e

inferBin :: Ctx -> BOp -> Expr -> Expr -> Either TypeError Type
inferBin ctx op e1 e2 =
  case binTy op of
    (ty1, ty2, tyOut) ->
      check ctx e1 ty1 *>
      check ctx e2 ty2 *>
      Right tyOut

binTy :: BOp -> (Type, Type, Type)
binTy op
  | op `elem` [Add, Sub, Mul, Div] = (TyInt, TyInt, TyInt)
  | op `elem` [And, Or]            = (TyBool, TyBool, TyBool)
  | op `elem` [Equals, Less]       = (TyInt, TyInt, TyBool)
  | otherwise                      = error "Unhandled operator in binTy"

inferUn :: Ctx -> UOp -> Expr -> Either TypeError Type
inferUn ctx op e =
  case unTy op of
    (tyIn, tyOut) ->
      check ctx e tyIn *>
      Right tyOut

unTy :: UOp -> (Type, Type)
unTy Neg = (TyInt, TyInt)
unTy Not = (TyBool, TyBool)

check :: Ctx -> Expr -> Type -> Either TypeError ()
check ctx e ty =
  infer ctx e >>= \ty' ->
  case ty == ty' of
    False -> Left $ Mismatch e ty ty'
    True  -> Right ()

checkProg :: Ctx -> Prog -> Either TypeError Ctx
checkProg ctx []     = Right ctx
checkProg ctx (s:ss) = checkStmt ctx s >>= \ctx' -> checkProg ctx' ss

checkStmt :: Ctx -> Stmt -> Either TypeError Ctx
checkStmt ctx (Decl ty x)  =
    case M.lookup x ctx of
        Just _ -> Left $ DuplicateVar x
        Nothing -> Right $ M.insert x ty ctx
checkStmt ctx (Assign x e) =
    case M.lookup x ctx of
        Just t -> check ctx e t *> Right ctx
        Nothing -> Left $ UnboundVar x
checkStmt ctx (Block ss)   = checkProg ctx ss *> Right ctx
checkStmt ctx (If e s1 s2) =
    check ctx e TyBool *>
    checkStmt ctx s1 *>
    checkStmt ctx s2 *>
    Right ctx
checkStmt ctx (Repeat e body) =
    check ctx e TyBool *>
    checkStmt ctx body *>
    Right ctx
checkStmt ctx (While e body)  =
    check ctx e TyBool *>
    checkStmt ctx body *>
    Right ctx
checkStmt ctx (Input v)    =
    case M.lookup v ctx of
        Just TyInt -> Right ctx
        Just _ -> Left $ InputBool v
        Nothing -> Left $ UnboundVar v
checkStmt ctx (Output e)   =
    check ctx e TyInt *> Right ctx
