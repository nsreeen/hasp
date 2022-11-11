module Interpreter where

import Evaluator
import Expression
import Lexer
import Parser

interpret input = eval $ parse $ tokenise input
