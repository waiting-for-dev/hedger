module Main where

import Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "No args provided"
    x:xs -> case x of
            "-h" -> putStrLn "Help option provided"
            _ -> putStrLn "Optionn not supported"
