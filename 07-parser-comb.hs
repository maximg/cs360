{-# LANGUAGE GADTs #-}

-- Hide some standard operators so we can use
-- variants with more specific types (for now)
import Prelude hiding ((<$>), (<$), (<*>), (<*), (*>))

-- Parsing is a module I have provided for you which wraps up some
-- functionality of parsec into a somewhat easier/simpler interface.
import Parsing

-- Our old friend Arith
data Arith where
  Lit :: Integer -> Arith
  Add :: Arith -> Arith -> Arith
  Sub :: Arith -> Arith -> Arith
  Mul :: Arith -> Arith -> Arith
  Exp :: Arith -> Arith -> Arith
  Neg :: Arith -> Arith
  deriving (Show)

interpArith :: Arith -> Integer
interpArith (Lit i) = i
interpArith (Add e1 e2) = interpArith e1 + interpArith e2
interpArith (Sub e1 e2) = interpArith e1 - interpArith e2
interpArith (Mul e1 e2) = interpArith e1 * interpArith e2
interpArith (Exp e1 e2) = interpArith e1 ^ interpArith e2
interpArith (Neg e1)    = -(interpArith e1)

lexer :: TokenParser u
lexer = makeTokenParser emptyDef

parens :: Parser a -> Parser a
parens     = getParens lexer

reservedOp :: String -> Parser ()
reservedOp = getReservedOp lexer

integer :: Parser Integer
integer    = getInteger lexer

whiteSpace :: Parser ()
whiteSpace = getWhiteSpace lexer

parseArithAtom :: Parser Arith
parseArithAtom = (Lit <$> integer) <|> parens parseArith

parseArith :: Parser Arith
parseArith = buildExpressionParser table parseArithAtom
  where
    table = [ [ Prefix (Neg <$ reservedOp "-") ]
            , [ Infix (Exp <$ reservedOp "^") AssocRight ]
            , [ Infix (Mul <$ reservedOp "*") AssocLeft ]
            , [ Infix (Add <$ reservedOp "+") AssocLeft
              , Infix (Sub <$ reservedOp "-") AssocLeft
              ]
            ]

arith :: Parser Arith
arith = whiteSpace *> parseArith <* eof

eval :: String -> Maybe Integer
eval s = case parse arith s of
  Left _  -> Nothing
  Right e -> Just (interpArith e)


parse2ndLit :: Parser Arith
parse2ndLit = integer *> (Lit <$> integer)

parseTildes :: Parser Integer
parseTildes = reservedOp "~" *> integer <* reservedOp "~"

adder :: Parser Arith
adder = add <$> integer <*> integer where
	add x y = Add (Lit x) (Lit y)
