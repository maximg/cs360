module Main where

import           Calc

import           Data.List                (isPrefixOf)
import           System.Console.Haskeline

main :: IO ()
main = putStrLn description >> runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "> "
      case minput of
        Nothing      -> return ()
        Just s | s `isPrefixOf` ":quit" -> return ()
               | s `isPrefixOf` ":help" -> (outputStrLn $ helpMsg) >> loop
        Just input   -> do
          outputStrLn $ calc input
          loop
