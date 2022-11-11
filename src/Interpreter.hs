module Interpreter where

import Evaluator
import Expression
import Lexer
import Parser

interpret environment input = eval' environment $ parse $ tokenise input
  
