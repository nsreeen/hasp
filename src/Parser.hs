module Parser where

import qualified Text.Read as TextRead

import Expression


tryParseNumber tokens@(x:xs) = case (TextRead.readMaybe x :: Maybe Int) of
                                 (Just number) -> Left (Number number, xs)
                                 Nothing -> Right tokens
tryParseBoolean tokens@(x:xs) = case x of
                                "true" -> Left (Boolean True, xs)
                                "false" -> Left (Boolean False, xs)
                                _ -> Right tokens
tryParseAtom tokens@(x:xs) = let isAtom = ((take 1 x) /= "(") && ((take 1 x) /= ")")
                             in if isAtom
                                then Left (Atom x, xs)
                                else Right tokens
tryParseList tokens@(x:xs) = case x of
                                 "(" -> tryParseTilEndOfList xs (List [])
                                 _ -> (Right tokens)

tryParseTilEndOfList tokens@(x:xs) (List l) = if x == ")"
                                              then (Left (List l, xs))
                                              else case (parse' tokens) of
                                                  (Right rest) -> (Right rest)
                                                  (Left (nextSymbol,rest)) -> (tryParseTilEndOfList rest (List (l ++ [nextSymbol])))

parse' tokens = tryParseNumber tokens >>= tryParseBoolean >>= tryParseAtom >>= tryParseList

parse tokens = case (parse' tokens) of
                        (Left (tree,tokens)) -> tree
                        (Right _) -> List []
