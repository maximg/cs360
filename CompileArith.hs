{-# LANGUAGE GADTs #-}

import           Text.Parsec          hiding (Error, many, (<|>))
import           Text.Parsec.Expr
import           Text.Parsec.Language (emptyDef)
import           Text.Parsec.String   (Parser)
import qualified Text.Parsec.Token    as P
import           Control.Applicative

data Op where
  Plus  :: Op
  Minus :: Op
  Times :: Op
  deriving (Show, Eq)

data Arith where
  Lit :: Integer -> Arith
  Bin :: Op -> Arith -> Arith -> Arith
  deriving (Show)

interp :: Arith -> Integer
interp (Lit n)        = n
interp (Bin op a1 a2) = interpOp op (interp a1) (interp a2)

interpOp :: Op -> Integer -> Integer -> Integer
interpOp Plus  = (+)
interpOp Minus = (-)
interpOp Times = (*)
readArith :: String -> Arith
readArith s = case parse parseArith "" s of
  Left  err -> error (show err)
  Right a   -> a

lexer :: P.TokenParser u
lexer = P.makeTokenParser $
  emptyDef
  { P.opStart         = oneOf "+-*"
  , P.opLetter        = oneOf "+-*"
  , P.reservedOpNames = ["+", "-", "*"]
  }

parens :: Parser a -> Parser a
parens     = P.parens lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

integer :: Parser Integer
integer = P.integer lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

parseAtom :: Parser Arith
parseAtom = Lit <$> integer <|> parens parseExpr

parseExpr :: Parser Arith
parseExpr = buildExpressionParser table parseAtom <?> "expression"
  where
    -- Each list of operators in the table has the same precedence, and
    -- the lists are ordered from highest precedence to lowest.  So
    -- in this case '*' has the highest precedence, and then "+" and
    -- "-" have lower (but equal) precedence.
    table = [ [ binary "*" (Bin Times) AssocLeft ]
            , [ binary "+" (Bin Plus)  AssocLeft
              , binary "-" (Bin Minus) AssocLeft
              ]
            ]

    binary name fun assoc = Infix (reservedOp name >> return fun) assoc

parseArith :: Parser Arith
parseArith = whiteSpace *> parseExpr <* eof

data Instruction where
    Push :: Integer -> Instruction
    Add :: Instruction
    Sub :: Instruction
    Mul :: Instruction
    deriving (Show)

type Stack = [Integer]

data MachineState where
    Working :: [Instruction] -> Stack -> MachineState
    Done :: Stack -> MachineState
    Error :: MachineState
    deriving (Show)

step :: MachineState -> MachineState
step (Working [] st) = Done st
step (Working (x:xs) st) = case x of
    Push v -> Working xs (v:st)
    Add -> exec xs st (+)
    Sub -> exec xs st (-)
    Mul -> exec xs st (*)
    where exec prog (x:y:ys) op = Working prog ((op x y):ys)
          exec _ _ _ = Error

execute :: [Instruction] -> MachineState
execute prog = head $ dropWhile isWorking $ iterate step (Working prog [])
    where isWorking (Working _ _) = True
          isWorking _             = False

run :: [Instruction] -> Maybe Integer
run prog = case execute prog of
    Done (x:xs) -> Just x
    _ -> Nothing


compile :: Arith -> [Instruction]
compile (Lit x) = [Push x]
compile (Bin op e1 e2) = compile e1 ++ compile e2 ++ [encodeOp op]
    where
        encodeOp Plus = Add
        encodeOp Minus = Sub
        encodeOp Times = Mul

exec :: String -> IO ()
exec s =
    case parse parseArith "" s of
        Left err -> print err
        Right prog -> do
            putStrLn $ show $ run $ compile prog