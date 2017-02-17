{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs         #-}

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer

data Possibly a where
  Nope :: Possibly a
  Definitely :: a -> Possibly a
  deriving (Show, Functor)

instance Applicative Possibly where
  pure  = return
  (<*>) = ap

instance Monad Possibly where
    return a = Definitely a
    (>>=) Nope _ = Nope
    (>>=) (Definitely a) f = f a

tick :: State Int Int
tick = do
    x <- get
    put (x+1)
    return x

tick3 :: State Int Int
tick3 = do
    x <- tick
    tick
    tick
    return x

modify2 :: (s -> s) -> State s ()
modify2 f = do
    x <- get
    put $ f x


-- A 'StackProgI a' is a program which returns an 'a' and has access
-- to a mutable stack of Ints.
type StackProgI a = State [Int] a

-- Get the size of the stack.
sizeI :: StackProgI Int
sizeI = do
    xs <- get
    return $ length xs

-- Push an Int onto the stack.
pushI :: Int -> StackProgI ()
pushI x = modify (x:)

-- Pop the top Int from the stack and return it. (For now, fail by
-- calling 'error' the stack is empty.)
popI :: StackProgI Int
popI = do
    xs <- get
    case xs of
        [] -> error "empty stack"
        (x:xs') -> do
            put xs'
            return x

-- Look at the top Int on the stack without popping it.  (Fail with 'error'
-- if the stack is empty.)
peekI :: StackProgI Int
peekI = do
    xs <- get
    case xs of
        [] -> error "empty stack"
        (x:xs') -> do
            return x

-- Run a 'StackProgI a' starting with the empty stack, returning the
-- produced value of type 'a' along with the final stack state.
runStackProgI :: StackProgI a -> (a, [Int])
runStackProgI p = runState p []

opI :: (Int -> Int -> Int) -> StackProgI ()
opI f = do
    y <- popI
    x <- popI
    pushI $ f x y

pushListI :: [Int] -> StackProgI ()
pushListI = mapM_ pushI

crushI :: (Int -> Int -> Int) -> StackProgI ()
crushI f = do
    size <- sizeI
    when (size > 1) $ opI f *> crushI f



data StackError where
  Underflow :: StackError
  deriving Show

type StackProgE el a = ExceptT StackError (State [el]) a

sizeE :: StackProgE el Int
sizeE = do
    xs <- get
    return $ length xs

pushE :: el -> StackProgE el ()
pushE x = modify (x:)

popE :: StackProgE el el
popE = do
    xs <- get
    case xs of
        [] -> throwError Underflow
        (x:xs') -> do
            put xs'
            return x

peekE :: StackProgE el el
peekE = do
    xs <- get
    case xs of
        [] ->throwError Underflow
        (x:xs') -> do
            return x

runStackProgE :: StackProgE el a -> (Either StackError a, [el])
runStackProgE p = runState (runExceptT p) []

opE :: (el -> el -> el) -> StackProgE el ()
opE f = do
    y <- popE
    x <- popE
    pushE $ f x y

pushListE :: [el] -> StackProgE el ()
pushListE = mapM_ pushE

crushE :: (el -> el -> el) -> StackProgE el ()
crushE f = do
    size <- sizeE
    when (size > 1) $ opE f *> crushE f


data StackProgAST el a where

  -- A simple return value.
  Return :: a -> StackProgAST el a

  -- Push a value on the stack.  This instruction stores the value
  -- to push, and the rest of the program (i.e. it's a node with a
  -- single child node).
  Push :: el -> StackProgAST el a -> StackProgAST el a

  -- Pop a value from the stack.  Stores a function which, when
  -- given the element that is popped, determines the rest of the
  -- program.  Another way to think of it is that a Pop node is like
  -- an infinitely-branching tree node: there is one child AST node
  -- for every possible element that could be popped.
  Pop  :: (el -> StackProgAST el a) -> StackProgAST el a

  -- Peek at the value on the top of the stack.
  Peek :: (el -> StackProgAST el a) -> StackProgAST el a

  -- Get the size of the stack.
  Size :: (Int -> StackProgAST el a) -> StackProgAST el a

  deriving Functor

-- We get an Applicative instance for free from the Monad instance.
instance Applicative (StackProgAST el) where
  pure = return
  (<*>) = ap

instance Monad (StackProgAST el) where
    return = Return
    (Return x) >>= f = f x
    (Push el ast) >>= f = Push el (ast >>= f)
    (Pop fn) >>= f = Pop (\x -> fn x >>= f)
    (Peek fn) >>= f = Peek (\x -> fn x >>= f)
    (Size fn) >>= f = Size (\x -> fn x >>= f)

size :: StackProgAST el Int
size = Size (\x -> Return x)

push :: el -> StackProgAST el ()
push el = Push el (Return ())

pop :: StackProgAST el el
pop = Pop (\x -> Return x)

peek :: StackProgAST el el
peek = Peek (\x -> Return x)

op :: (el -> el -> el) -> StackProgAST el ()
op f = do
    y <- pop
    x <- pop
    push $ f x y

pushList :: [el] -> StackProgAST el ()
pushList = mapM_ push

crush :: (el -> el -> el) -> StackProgAST el ()
crush f =  do
    size <- size
    when (size > 1) $ op f *> crush f


interpStackProgE :: StackProgAST el a -> StackProgE el a
interpStackProgE (Return el) = pure el
interpStackProgE (Size fn) = do
    x <- sizeE
    interpStackProgE $ fn x
interpStackProgE (Push el rest) = pushE el *> interpStackProgE rest
interpStackProgE (Pop fn) = do
    x <- popE
    interpStackProgE $ fn x
interpStackProgE (Peek fn) = do
    x <- peekE
    interpStackProgE $ fn x


runAsStackProgE :: StackProgAST el a -> (Either StackError a, [el])
runAsStackProgE = runStackProgE . interpStackProgE


type StackProgW el a = ExceptT StackError (WriterT [String] (State [el])) a

interpStackProgW :: Show el => StackProgAST el a -> StackProgW el a
interpStackProgW (Return el) = pure el
interpStackProgW (Size fn) = do
    x <- sizeW
    interpStackProgW $ fn x
interpStackProgW (Push el rest) = pushW el *> interpStackProgW rest
interpStackProgW (Pop fn) = do
    x <- popW
    interpStackProgW $ fn x
interpStackProgW (Peek fn) = do
    x <- peekW
    interpStackProgW $ fn x


sizeW :: StackProgW el Int
sizeW =  do
    xs <- get
    return $ length xs

pushW :: Show el => el -> StackProgW el ()
pushW el = do
    tell ["Pushed " ++ (show el)]
    modify (el:)

popW :: Show el => StackProgW el el
popW = do
    xs <- get
    case xs of
        [] -> throwError Underflow
        (x:xs') -> do
            put xs'
            tell ["Popped " ++ (show x)]
            return x

peekW :: StackProgW el el
peekW = do
    xs <- get
    case xs of
        [] ->throwError Underflow
        (x:xs') -> do
            return x

runAsStackProgW :: Show el => StackProgAST el a -> ((Either StackError a, [String]), [el])
runAsStackProgW =  runStackProgW . interpStackProgW

runStackProgW :: StackProgW el a -> ((Either StackError a, [String]), [el])
runStackProgW p = runState (runWriterT $ runExceptT p) []
