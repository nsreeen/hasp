module Main where

import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map
import System.IO


import Expression
import Interpreter

initialEnvironment :: Environment
initialEnvironment = Map.empty

main :: IO Environment
main = do
    putStrLn "-----------------------------------------------------------------"
    execStateT runRepl initialEnvironment

runRepl :: StateT Environment IO ()
runRepl = forever $ do
    lift $ putStr ">>"
    lift $ hFlush stdout -- this allow a prompt on the same line as the cursor
    program <- lift getLine
    environment <- get
    let (newEnvironment, result) = interpret environment program
    put newEnvironment
    lift $ putStrLn $ show result
