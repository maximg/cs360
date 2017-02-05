module Main where

import           Quilt                    (Color, QuiltFun, evalQuilt)

import           Data.Char                (isDigit)
import           GHC.Word                 (Word8)

import           Codec.Picture
import           Control.Monad.State
import           System.Console.Haskeline

------------------------------------------------------------
-- Customize me!
------------------------------------------------------------

description :: String
description = "Welcome to The Quilterator!"

helpMsg :: String
helpMsg = unlines
  [ "Enter an expression to generate a 256x256 image and save it to quilt.png."
  , ":size        - report the current size used by the :save command."
  , ":size <int>  - set the size used by the :save command."
  , ":save <file> - save a :size x :size copy of the most recent expression to <file>.png and <file>.txt"
  , ":help        - print this message."
  , ":quit        - quit."
  , ""
  , "Your command history is automatically saved in a file named quilt_history.txt."
  ]

------------------------------------------------------------
-- Ignore the man behind the curtain (or peek if you like)
------------------------------------------------------------

data QuiltState = QS
  { quiltFun  :: Maybe QuiltFun
  , quiltSrc  :: Maybe String
  , quiltSize :: Int
  }

type QuiltM = StateT QuiltState (InputT IO)

defaultSize :: Int
defaultSize = 256

showSize :: Int -> String
showSize sz = show sz ++ "x" ++ show sz

quiltSettings :: Settings IO
quiltSettings = defaultSettings
  { historyFile = Just "quilt_history.txt" }

quiltREPL :: IO ()
quiltREPL = do
  putStrLn description
  runInputT quiltSettings . flip evalStateT (QS Nothing Nothing 256) $ loop
  where
    loop :: QuiltM ()
    loop = do
      minput <- lift $ getInputLine "> "
      case minput of
        Nothing      -> return ()
        Just s       -> do
          let (cmd:rest) = words s
          shouldLoop <- case cmd of
            ":q"    -> return False
            ":quit" -> return False
            ":help" -> (lift $ outputStrLn helpMsg) >> return True
            ":size" -> sizeCmd rest                 >> return True
            ":save" -> saveCmd rest                 >> return True
            _       -> eval s                       >> return True
          when shouldLoop $ loop

sizeCmd :: [String] -> QuiltM ()
sizeCmd [] = do
  curSz <- gets quiltSize
  lift $ outputStrLn $ "The current size is " ++ showSize curSz ++ "."
sizeCmd (sz:_)
  = case all isDigit sz of
      False -> lift $ outputStrLn "Please provide an integer size."
      True  -> do
        modify (\qs -> qs { quiltSize = read sz })
        lift $ outputStrLn $ "Size for :saved images is now "
                             ++ sz ++ "x" ++ sz ++ "."

saveCmd []     = lift $ outputStrLn "Please provide a file name to save in."
saveCmd (fn:_) = do
  sz <- gets quiltSize
  saveQFun True sz fn

saveQFun :: Bool -> Int -> FilePath -> QuiltM ()
saveQFun saveSrc qSize fn = do
  mq <- gets quiltFun
  ms <- gets quiltSrc
  case mq of
    Nothing -> lift $ outputStrLn "Nothing to save!"
    Just q  -> do
      let imgFn = fn ?<.> "png"
          srcFn = fn -<.> "txt"
      liftIO $ saveQFunRaw qSize imgFn q
      lift $ outputStrLn $ showSize qSize ++ " -> " ++ imgFn
      when saveSrc $ do
        liftIO $ maybe (return ()) (writeFile srcFn) ms
        lift   $ maybe (return ()) (\_ -> outputStrLn $ "source -> " ++ srcFn) ms

base ?<.> ext = if ('.' `elem` base) then base else base ++ "." ++ ext
base -<.> ext = takeWhile (/= '.') base ++ "." ++ ext

saveQFunRaw :: Int -> FilePath -> QuiltFun -> IO ()
saveQFunRaw qSize fn q = do
  let q' r c = q (2*(fromIntegral r / fromIntegral qSize) - 1)
                 (-(2*(fromIntegral c / fromIntegral qSize) - 1))
      img    = ImageRGB8 $ generateImage (\r c -> toPixel $ q' r c) qSize qSize
  savePngImage fn img

toPixel :: Color -> PixelRGB8
toPixel [r,g,b] = PixelRGB8 (conv r) (conv g) (conv b)
  where
    conv :: Double -> Word8
    conv v = fromIntegral . clamp $ floor (v * 256)
    clamp :: Int -> Int
    clamp v
      | v > 255   = 255
      | v < 0     = 0
      | otherwise = v
toPixel c = error $ "toPixel called on list of length /= 3: " ++ show c

eval :: String -> QuiltM ()
eval s = case evalQuilt s of
  Left  err  -> lift $ outputStrLn err
  Right qfun -> do
    modify (\qs -> qs { quiltFun = Just qfun
                      , quiltSrc = Just s
                      }
           )
    saveQFun False defaultSize "quilt.png"

main :: IO ()
main = quiltREPL
