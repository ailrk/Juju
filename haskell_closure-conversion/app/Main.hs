module Main where

main :: IO ()
main = do
  putStrLn "How do we compile closure?"
  putStrLn "Use. Closure conversion"
  putStrLn "Closure is not a function. It's a function with it's environment"
  putStrLn "C function poiner really only represents a computation, and it doesn't capture lexical env"
  putStrLn "But if a langauge has closure, it needs some way to compile a fuction with it's environment so it knows where to lookup for it's free variables"
  putStrLn "Natural way of doing it is to pass the environment as a extra paramter to the function"
  putStrLn "Like dict passing for type class. Yes because everything is the same in computer science."
  putStrLn "Each function takes an extra parameter as it's argument, it's caller pass the right environment"
  putStrLn "But what about the top level closure?"
  putStrLn "Well there is only one set of enviroment to pass, which are top level definitions"
  putStrLn "So the 'indution' ends with a base case."
  putStrLn "There is also differences between topdown conversion, bottomup conversion, create a chain of dictonary or simply copying dictionaries across layers... All of these sutble differences have their performance implication"
