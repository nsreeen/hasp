module Main where

import Interpreter

main :: IO ()
main = do
    program <- getLine
    let result = interpret program
    putStrLn $ show result
