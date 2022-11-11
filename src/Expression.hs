module Expression where

import qualified Data.Map as Map
import qualified Text.Read as TextRead

-- data Expression = Atom String
--                 | Number Int
--                 | Boolean Bool
--                 | List [Expression]
--                 | Error String
--                 | DataList [Expression]
--                 | UserDefinedFunction {_name::String, _body::Expression, _args::Expression, _env::(Map.Map String Expression)}
--                 | Variable {_name::String, _value::Expression}
--
-- instance Show Expression where
--   show (Atom x) = "Atom " ++ x
--   show (Number x) = "Number " ++ (show x)
--   show (Boolean x) = "Boolean " ++ (show x)
--   show (List x) = "List " ++ (show x)
--   show (Error x) = "Error " ++ (show x)
--   show (DataList x) = "DataList " ++ (show x)
--   show UserDefinedFunction{_name=n, _body=b, _args=a, _env=e} = "UserDefinedFunction name:" ++ n ++ " body:" ++ (show b) ++ " args:" ++ (show a) ++ " env:" ++ (show e)
--   show Variable{_name=n, _value=v} = "Variable " ++ n ++ " " ++ (show v)
--
-- instance Eq Expression where
--   (==) (Atom x) (Atom y) = x == y
--   (==) (Number x) (Number y) = x == y
--   (==) (Boolean x) (Boolean y) = x == y
--   (==) (List x) (List y) = x == y
--   (==) (Error x) (Error y) = x == y
--   (==) (DataList x) (DataList y) = x == y
--   (==) UserDefinedFunction{_name=n1, _body=_, _args=_, _env=_} UserDefinedFunction{_name=n2, _body=_, _args=_, _env=_} = n1 == n2
--   (==) Variable{_name=n1, _value=_} Variable{_name=n2, _value=_} = n1 == n2
--   (==) _ _ = False

type Body = Expression
type Parameters = [Expression]
type Environment = Map.Map String Expression
data Expression = Atom String
                | Number Int
                | Boolean Bool
                | List [Expression]
                | Error String
                | DataList [Expression]
                | Function Body Parameters Environment

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

-- instance Functor Expression where
--   fmap f (Number x) = Number (f x)
--   fmap f (Boolean x) = Boolean (f x)
--   fmap f expression = Error "Cannot apply " ++ (show f) ++ " to " ++ (show expression)
-- --
-- instance Applicative Expression where
--     pure True = Boolean True
--     pure False = Boolean False
--     pure [] = List []
--     pure l@(x:xs) = List l
--     pure x = case (TextRead.readMaybe x :: Maybe Int) of
--                 (Just number) -> Number number
--                 Nothing -> Error "Cannot convert " ++ (show x) ++ " to Expression"
--     (<*>) (Boolean f) something = fmap f something
--     (<*>) (Number f) something = fmap f something
--     (<*>) expression something = Error "Cannot apply " ++ (show expression) ++ " to " ++ (show something)
