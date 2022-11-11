module Lexer where

import Control.Monad.State

-- tokenise inputString = tokeniseInner inputString [] "" where
--   tokeniseInner :: String -> [String] -> [Char] -> [String]
--   tokeniseInner ('(':xs) output current = tokeniseInner xs (output ++ ["("]) []
--   tokeniseInner (')':xs) output [] = tokeniseInner xs (output ++ [")"]) []
--   tokeniseInner (')':xs) output current = tokeniseInner xs (output ++ [current, ")"]) []
--   tokeniseInner (' ':xs) output [] = tokeniseInner xs output []
--   tokeniseInner (' ':xs) output current = tokeniseInner xs (output ++ [current]) []
--   tokeniseInner (x:xs) output current = tokeniseInner xs output (current ++ [x])
--   tokeniseInner [] output [] = output
--   tokeniseInner [] output current = output++[current]

type InputStream = String
type CurrentToken = [Char]
type Tokens = [String]
data TokeniseState = TokeniseState InputStream CurrentToken Tokens deriving (Show)

tokenise :: InputStream -> Tokens
tokenise input = let (TokeniseState _ _ tokens) = execState runTokenise (TokeniseState input [] [])
                       in tokens

runTokenise :: State TokeniseState ()
runTokenise = do
             state <- get
             let nextChar = readNextChar state
             modify (dropNextChar)
             case nextChar of
                  Nothing     -> modify completeCurrentToken
                  (Just ' ')  -> do
                                  modify completeCurrentToken
                                  runTokenise
                  (Just '(')  -> do
                                  modify completeCurrentToken
                                  modify (appendToken ['('])
                                  runTokenise
                  (Just ')')  -> do
                                  modify completeCurrentToken
                                  modify (appendToken [')'])
                                  runTokenise
                  (Just char) -> do
                                  modify (appendToCurrentToken char)
                                  runTokenise

completeCurrentToken :: TokeniseState -> TokeniseState
completeCurrentToken (TokeniseState inputStream currentToken tokens) = let newTokens = case currentToken of
                                                                                [] -> tokens
                                                                                _ -> tokens++[currentToken]
                                                                       in TokeniseState inputStream [] newTokens

appendToCurrentToken :: Char -> TokeniseState -> TokeniseState
appendToCurrentToken char (TokeniseState inputStream currentToken tokens) = TokeniseState inputStream (currentToken++[char]) tokens

appendToken :: String -> TokeniseState -> TokeniseState
appendToken token (TokeniseState inputStream currentToken tokens) = (TokeniseState inputStream currentToken (tokens++[token]))

dropNextChar :: TokeniseState -> TokeniseState
dropNextChar (TokeniseState inputStream currentToken tokens) = case inputStream of
                                                                          [] -> (TokeniseState inputStream currentToken tokens)
                                                                          (nextChar:rest) -> (TokeniseState rest currentToken tokens)

readNextChar :: TokeniseState -> Maybe Char
readNextChar (TokeniseState inputStream currentToken tokens) = case inputStream of
                                                                          [] -> Nothing
                                                                          (nextChar:_) -> Just nextChar
