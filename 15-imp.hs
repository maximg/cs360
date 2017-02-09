{-# LANGUAGE GADTs        #-}

import           Parsing2

import qualified Data.Map           as M
import           Text.Read          (readMaybe)
import           System.Environment (getArgs)
import           System.IO

type Var = String

type Prog = [Stmt]

data Type where
  TyInt  :: Type
  TyBool :: Type
  deriving (Show, Eq)

data Stmt where
  Decl   :: Type -> Var -> Expr -> Stmt   -- <type> <var>
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
  <|> Decl    <$> parseType <*> ident <*> (reservedOp ":=" *> parseExpr)

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
checkStmt ctx (Decl ty x expr)  =
    case M.lookup x ctx of
        Just _ -> Left $ DuplicateVar x
        Nothing -> check ctx expr ty *> (Right $ M.insert x ty ctx)
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
    check ctx e TyInt *>
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


type Value = Integer
type Mem = M.Map Var Value

interpExpr :: Mem -> Expr -> Value
interpExpr _ (EInt i)       = i
interpExpr _ (EBool b)      = fromBool b
interpExpr m (EVar x)       =
  case M.lookup x m of
    Just v  -> v
    Nothing -> error $ "Impossible! Uninitialized variable " ++ x
interpExpr m (EBin b e1 e2) = interpBOp b (interpExpr m e1) (interpExpr m e2)
interpExpr m (EUn  u e)     = interpUOp u (interpExpr m e )

interpUOp :: UOp -> Value -> Value
interpUOp Neg v = -v
interpUOp Not v = 1-v

interpBOp :: BOp -> Value -> Value -> Value
interpBOp Add    = (+)
interpBOp Sub    = (-)
interpBOp Mul    = (*)
interpBOp Div    = div
interpBOp And    = (*)
interpBOp Or     = \v1 v2 -> min 1 (v1 + v2)
interpBOp Equals = \v1 v2 -> fromBool (v1 == v2)
interpBOp Less   = \v1 v2 -> fromBool (v1 <  v2)

fromBool :: Bool -> Value
fromBool False = 0
fromBool True  = 1

toBool :: Value -> Bool
toBool 0 = False
toBool _ = True

data World where
  W  :: Mem       -- Current state of memory
     -> [String]  -- Strings typed by the user, waiting to be read by 'input'
     -> [String]  -- Strings produced by 'output' (newest first)
     -> World
  Error :: World  -- Something went wrong
  deriving Show

-- An initial world state, given user input
initWorld :: String -> World
initWorld inp = W M.empty (words inp) []

interpStmt :: Stmt -> World -> World
interpStmt (Decl _ v expr) (W mem inp outp) = W (M.insert v init mem) inp outp
    where init = interpExpr mem expr
interpStmt (Assign v e) (W mem inp outp) = W (M.insert v (interpExpr mem e) mem) inp outp
interpStmt (Block bl) w = interpProg bl w
interpStmt (If c s1 s2) w@(W mem inp outp) =
    if toBool (interpExpr mem c) then interpStmt s1 w
                                 else interpStmt s2 w
interpStmt (Repeat cnt s1) w@(W mem _ _) = interpRepeat (interpExpr mem cnt) s1 w
interpStmt (While c s1) w  = interpWhile c s1 w
interpStmt (Input v) (W _ [] _) = Error
interpStmt (Input v) (W mem (y:ys) outp) =
    case readMaybe y :: Maybe Integer of
        Just x -> W (M.insert v x mem) ys outp
        Nothing -> Error
interpStmt (Output e) (W mem inp outp) = W mem inp ((show $ interpExpr mem e):outp)
interpStmt _ Error = Error


interpRepeat :: Integer -> Stmt -> World -> World
interpRepeat 0 _ w = w
interpRepeat n st w = interpRepeat (n-1) st (interpStmt st w)

interpWhile :: Expr -> Stmt -> World -> World
interpWhile cond st w@(W mem _ _) =
    if interpExpr mem cond == 0  then w
                                 else interpWhile cond st $
                                      interpStmt st w

interpProg :: Prog -> World -> World
interpProg prog w = foldl (flip interpStmt) w prog


formatWorld :: World -> String
formatWorld (W m _ o) = unlines $
    reverse o ++
    ["===================="] ++
    map formatVar (M.assocs m)
formatWorld Error = "Error"

formatVar :: (String, Value) -> String
formatVar (k,v) = k ++ " -> " ++ show v

run :: String -> IO ()
run fileName = do
    contents <- readFile fileName  
    case parse impParser contents of
        Left err -> print err
        Right prog -> case checkProg M.empty prog of
            Left err -> print err
            Right _ -> do
                let inp = "5 6"
                putStrLn $ formatWorld $ interpProg prog (initWorld inp)

run1 :: String -> IO ()
run1 fileName = do
    contents <- readFile fileName  
    print$  parse impParser contents

run2 :: String -> IO ()
run2 fileName = do
    contents <- readFile fileName  
    case parse impParser contents of
        Left err -> print err
        Right prog -> print $ checkProg M.empty prog


main :: IO ()
main = do
  args <- getArgs
  case args of
    []     -> putStrLn "Please provide a file name."
    (fn:_) -> run2 fn