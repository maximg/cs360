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
pushI x = do
    xs <- get
    put (x:xs)

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
