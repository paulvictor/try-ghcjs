{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI, CPP #-}
module Main (main) where

main :: IO ()
main = putStrLn "Dummy main"
-- module Main(main) where

-- import qualified MyLib (someFunc)
-- import Data.Foldable

-- import qualified Data.ByteString.Lazy as BS
-- import Data.ByteString.Lazy (ByteString)
-- import qualified Codec.Compression.GZip as Z
-- import Control.Concurrent (threadDelay)

-- #ifdef __GHCJS__
-- import GHCJS.Types (JSVal)

-- add :: Int -> Int -> IO Int
-- add x y =
--   let
--     sum = x + y
--   in
--     do
--       threadDelay (1000 * 1000 * 10)
--       putStrLn (show x <> " + " <> show y <> " = " <> show sum)
--       pure sum

-- foreign export javascript "hs_add" add :: Int -> Int -> IO Int
-- foreign import javascript unsafe
--   "require($1)" require :: JSVal -> JSVal
-- foreign import javascript unsafe        "console.log($1);" writeNumber :: Int -> IO ()
-- -- foreign import javascript interruptible "setTimeout($c, $1);"         delay       :: Int -> IO ()
-- foreign import javascript interruptible
--   "\
--   var P = require(\"bluebird\");\
--   var p = new P(function(res, rej){\
--     setTimeout(function(){res(128)}, $1);\
--   });\
--   p.then(function(v){$c(v)});"         delay       :: Int -> IO Int
-- #else
-- writeNumber = error "writeNumber: only available from JavaScript"
-- delay = error "delay: only available from JavaScript"
-- #endif

-- main :: IO ()
-- main = do
--   let
--     input :: ByteString
--     input = fold $ replicate 10000 "foobarbaz"
-- --   mapM_ (\x -> writeNumber x >> delay 50) [1..1000]
--   for_ [ 1,2,3,4,5] print

--   add 3 4

--   putStrLn "Hello, Haskell!"
--   MyLib.someFunc
