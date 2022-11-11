module Evaluator where

import qualified Data.Map as Map
import Data.Foldable

import Expression
import Lexer
import Parser



{-
TODO∷
- lambdas
- cons
- error handling
  - wrong num args
  - div by 0
-}

eval :: Expression -> Expression
eval expression = snd $ eval' Map.empty expression

unwrapNumber (Number x) = x
unwrapBoolean (Boolean x) = x

numericFold env f acc args = let evaluatedArgs = map (\x -> unwrapNumber $ snd $ eval' env x) args
                                 result = foldl' f acc evaluatedArgs
                             in (env, Number result)

applyToList env arg emptyCase nonEmptyCase = let list = snd $ eval' env arg
                                             in case list of
                                                (DataList (x:xs)) -> nonEmptyCase x xs
                                                (DataList []) -> emptyCase
                                                _ -> Error "Cannot apply list function to an expression that isn't a list"

applyBinary :: (Map.Map String Expression) -> (a -> a -> b) -> (Expression -> a) -> (b -> Expression) -> Expression -> Expression -> ((Map.Map String Expression), Expression)
applyBinary env f unwrap wrap x y = let evaluatedX = unwrap $ snd $ eval' env x
                                        evaluatedY = unwrap $ snd $ eval' env y
                                        result = f evaluatedX evaluatedY
                                    in (env, wrap result)

eval' :: (Map.Map String Expression) -> Expression -> ((Map.Map String Expression), Expression)
eval' env (Number x) = (env, Number x)
eval' env (Boolean x) = (env, Boolean x)

eval' env (List (Atom "+":args)) = numericFold env (+) 0 args
eval' env (List (Atom "*":args)) = numericFold env (*) 1 args
eval' env (List (Atom "-":x:y:[])) = applyBinary env (-) unwrapNumber (Number) x y
eval' env (List (Atom ">":x:y:[])) = applyBinary env (>) unwrapNumber (Boolean) x y
eval' env (List (Atom "<":x:y:[])) = applyBinary env (<) unwrapNumber (Boolean) x y
eval' env (List (Atom "and":x:y:[])) = applyBinary env (&&) unwrapBoolean (Boolean) x y
eval' env (List (Atom "or":x:y:[])) = applyBinary env (||) unwrapBoolean (Boolean) x y
eval' env (List (Atom "==":x:y:[])) = applyBinary env (==) (\x -> x) (Boolean) x y

eval' env (List (Atom "list":args)) = (env, DataList args)
eval' env  (List (Atom "empty?":arg:[])) = (env, applyToList env arg (Boolean True) (\_ _ -> Boolean False))
eval' env  (List (Atom "car":arg:[])) = (env, applyToList env arg (Error "Cannot apply `car` to an empty list") (\x _ -> x))
eval' env  (List (Atom "cdr":arg:[])) = (env, applyToList env arg (Error "Cannot apply `cdr` to an empty list") (\_ xs -> DataList xs))
eval' env  (List (Atom "cons":arg1:arg2:[])) = let evaluatedArg1 = snd $ eval' env arg1
                                               in (env, applyToList env arg2 (DataList [evaluatedArg1]) (\x xs -> DataList ([evaluatedArg1,x] ++ xs)))

eval' env (List ((Atom "if"):condition:expression1:expression2:[])) = if unwrapBoolean $ snd $ eval' env condition
                                                                      then (eval' env expression1)
                                                                      else (eval' env expression2)

eval' env (List ((Atom "define"):(Atom name):expression:[])) = let value = snd $ eval' env expression
                                                                   updatedEnv = Map.insert name value env
                                                               in (updatedEnv, value)

eval' env (List ((Atom "define"):(Atom name):(List parameters):body:[])) = let function = Function body parameters env
                                                                               updatedEnv = Map.insert name function env
                                                                           in (updatedEnv, function)

eval' env (List ((Atom "let"):(List vars):body:[])) = let argPairs = pair vars
                                                          updatedEnv = addPairsToHashMap env argPairs
                                                      in eval' updatedEnv body

eval' env (List ((Atom "begin"):expressions)) = executeSequentially env expressions where
                          executeSequentially env (expression:[]) = eval' env expression
                          executeSequentially env (expression:rest) = let updatedEnv = fst $ eval' env expression
                                                                      in executeSequentially updatedEnv rest

eval' env (List (List ((Atom "lambda"):(List parameters):body:[]):(List arguments):[])) = let function = Function body parameters env
                                                                                              result = callFunction env function arguments
                                                                                          in (env, result)

eval' env (List (List ((Atom "lambda"):(List parameters):body:[]):argument:[])) = let function = Function body parameters env
                                                                                      result = callFunction env function [argument]
                                                                                  in (env, result)

eval' env (List ((Atom x):arguments)) = case (snd $ eval' env (Atom x)) of
                                      f@(Function _ _ _) -> ((Map.insert "here" (Number 8) env), (callFunction env f arguments))
                                      x -> (env, Error $ "oops " ++ (show x))

eval' env (Atom x) = case (Map.lookup x env) of
          (Just f@(Function body args functionEnv)) -> (env, f)
          (Just expression) -> (env, expression)
          Nothing -> (env, Error $ "Variable '" ++ x ++ "' not found in environment")

eval' env _ = (env, Error "Could not evaluate program")

pair list = pair' list [] where
      pair' [] pairs = pairs
      pair' (x:y:rest) pairs = pair' rest (pairs++[(x,y)])

addPairsToHashMap hashMap pairs = addPairsToHashMap' hashMap pairs where -- TODO should not unwrap atoms
    addPairsToHashMap' hashMap [] = hashMap
    addPairsToHashMap' hashMap (((Atom name),expression):rest) = addPairsToHashMap' (Map.insert name expression hashMap) rest
    addPairsToHashMap' hashMap _ = Map.fromList [("NOT PAIRS", Number 0)]

callFunction :: Environment -> Expression -> [Expression] -> Expression
callFunction callingEnv f@(Function body parameters localEnv) arguments =
                let paramArgPairs = zip parameters (map (\a -> (snd $ eval' callingEnv a)) arguments)
                    functionEnvWithArgs = addPairsToHashMap localEnv paramArgPairs
                    evaluatedExpression = snd $ eval' functionEnvWithArgs body
                in evaluatedExpression
