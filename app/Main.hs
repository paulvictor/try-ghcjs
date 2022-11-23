{-# LANGUAGE JavaScriptFFI, CPP #-}
module Main where

import qualified MyLib (someFunc)
import Data.Foldable

#ifdef __GHCJS__
foreign import javascript unsafe        "console.log($1);" writeNumber :: Int -> IO ()
-- foreign import javascript interruptible "setTimeout($c, $1);"         delay       :: Int -> IO ()
foreign import javascript interruptible
  "\
  var P = require(\"bluebird\");\
  var p = new P(function(res, rej){\
    setTimeout(function(){res(128)}, $1);\
  });\
  p.then(function(v){$c(v)});"         delay       :: Int -> IO Int
#else
writeNumber = error "writeNumber: only available from JavaScript"
delay = error "delay: only available from JavaScript"
#endif

main :: IO ()
main = do
  delay 2000 >>= print
--   mapM_ (\x -> writeNumber x >> delay 50) [1..1000]
  for_ [ 1,2,3,4,5] print

  putStrLn "Hello, Haskell!"
  MyLib.someFunc
