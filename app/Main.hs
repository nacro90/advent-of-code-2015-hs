module Main where

import AdventFiveSolutions
import Text.Printf

solutions :: [(String, String -> String)]
solutions = [("notQuiteLisp", show . notQuiteLisp)]

main :: IO ()
main = do
  putStrLn "Select solution:"
  line <- getLine
  let selection = (round . read) line :: Int
  let solutionTuple = solutions !! selection
  input <- getLine
  putStrLn $ snd solutionTuple input
