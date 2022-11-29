module Internal where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

add :: Int -> Int -> IO Int
add x y =
  let
    sum = x + y
  in
    do
      putStrLn (show x <> " + " <> show y <> " = " <> show sum)
      pure sum

foreign export javascript "hs_add" add :: Int -> Int -> IO Int
