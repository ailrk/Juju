module Main where

main :: IO ()
main = do
  putStrLn "CPS transformation has little to do with the programmings style with cb"
  putStrLn "Instead it's a very good format of the code to be used as an IR"
  putStrLn "All direct style code can be converted to cps form"
  putStrLn "With all intermediate values be named"
  putStrLn "and evaluation order be explicited"
  putStrLn "The code is still in cps form, but when we processing cps transformed ir"
  putStrLn "We just treat it as a data format that preserve the original program's semantics"
  putStrLn "But cps is still hard to read, a simpler form ANF can also be used"
  putStrLn "Which simply use let to expose intermediate values"
