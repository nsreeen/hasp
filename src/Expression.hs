module Expression where

import qualified Data.Map as Map

data Expression = Atom String
                | Number Int
                | Boolean Bool
                | List [Expression]
                | Error String
                | DataList [Expression]
                | Function Body Parameters Environment

type Body = Expression
type Parameters = [Expression]
type Environment = Map.Map String Expression

instance Show Expression where
  show (Atom x) = x
  show (Number x) = (show x)
  show (Boolean x) = (show x)
  show (List x) = (show x)
  show (Error x) = (show x)
  show (DataList x) = (show x)
  show (Function body args env) = "Function body:" ++ (show body) ++ " args:" ++ (show args) ++ " env:" ++ (show env)

instance Eq Expression where
  (==) (Atom x) (Atom y) = x == y
  (==) (Number x) (Number y) = x == y
  (==) (Boolean x) (Boolean y) = x == y
  (==) (List x) (List y) = x == y
  (==) (Error x) (Error y) = x == y
  (==) (DataList x) (DataList y) = x == y
  (==) (Function body1 args1 env1) (Function body2 args2 env2) = body1 == body2 && args2 == args2 && env1 == env2
  (==) _ _ = False
