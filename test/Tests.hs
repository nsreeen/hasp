import Test.HUnit

import Interpreter
import Evaluator
import Expression
import Lexer
import Parser

lexerTest_emptyString = TestCase (assertEqual "tokenise ," ([]) (tokenise ""))
lexerTest_expression = TestCase (assertEqual "tokenise (+ 1 (- 4 2))," (["(","+","1","(","-", "4", "2", ")", ")"]) (tokenise "(+ 1 (- 4 2))"))
lexerTests = TestList [TestLabel "lexer_emptyString" lexerTest_emptyString,
                       TestLabel "lexerTest_expression" lexerTest_expression]

parserTest_expression = TestCase (assertEqual "parse ['1']" (Number 1) (parse ["1"]))
parserTest_nestedExpression = TestCase (assertEqual "parse ['(','+','2','(','-','7','1',')',')']" (List [Atom "+",Number 2,List [Atom "-",Number 7,Number 1]]) (parse ["(","+","2","(","-","7","1",")",")"]))
parserTest_booleans = TestCase (assertEqual "parse ['(','==','true','false', ')', ]" (List [Atom "==",Boolean True,Boolean False]) (parse ["(","==","true","false",")"]))
parserTest_list = TestCase (assertEqual "parse ['(','1','2','3', ')']" (List [Number 1, Number 2, Number 3]) (parse ["(","1","2","3",")"]))
parserTests = TestList [TestLabel "parserTest_expression" parserTest_expression,
                        TestLabel "parserTest_nestedExpression" parserTest_nestedExpression,
                        TestLabel "parserTest_booleans" parserTest_booleans,
                        TestLabel "parserTest_list" parserTest_list]

interpreter_add = TestCase (assertEqual "interpret (+ 1 2)" (Number 3) (interpret "(+ 1 2)"))
interpreter_nestedSub = TestCase (assertEqual "interpret (+ (- 7 3) 2)" (Number 6) (interpret "(+ (- 7 3) 2)"))
interpreter_and = TestCase (assertEqual "interpret (and true true)" (Boolean True) (interpret "(and true true)"))
interpreter_or = TestCase (assertEqual "interpret (or false true)" (Boolean True) (interpret "(or false true)"))
interpreter_eq = TestCase (assertEqual "interpret (== true false)" (Boolean False) (interpret "(== true false)"))
interpreter_greaterThan = TestCase (assertEqual "interpret (> 6 2)" (Boolean True) (interpret "(> 6 2)"))
interpreter_lessThan = TestCase (assertEqual "interpret (< 4 2)" (Boolean False) (interpret "(< 4 2)"))
interpreter_mul = TestCase (assertEqual "interpret (* 1 2 3)" (Number 6) (interpret "(* 1 2 3)"))

interpreter_list = TestCase (assertEqual "interpret (list 1 2 3)" (DataList [Number 1, Number 2, Number 3]) (interpret "(list 1 2 3)"))
interpreter_emptyList = TestCase (assertEqual "interpret (list)" (DataList []) (interpret "(list)"))
interpreter_empty = TestCase (assertEqual "interpret (empty? (list 1 2 3))" (Boolean False) (interpret "(empty? (list 1 2 3))"))
interpreter_car = TestCase (assertEqual "interpret (car (list 1 2 3))" (Number (1)) (interpret "(car (list 1 2 3))"))
interpreter_cdr = TestCase (assertEqual "interpret (cdr (list 1 2 3))" (DataList [Number 2, Number 3]) (interpret "(cdr (list 1 2 3))"))
interpreter_cons = TestCase (assertEqual "interpret (cons 1 (list 2 3))" (DataList [Number 1, Number 2, Number 3]) (interpret "(cons 1 (list 2 3))"))
interpreter_consEmptyList = TestCase (assertEqual "interpret (cons 1 (list))" (DataList [Number 1]) (interpret "(cons 1 (list))"))

interpreter_if = TestCase (assertEqual "interpret (if true 1 2)" (Number 1) (interpret "(if true 1 2)"))
interpreter_let = TestCase (assertEqual "interpret (let (x 1 y 2) (+ 1 2))" (Number 3) (interpret "(let (x 1 y 2) (+ 1 2))"))
interpreter_begin_define = TestCase (assertEqual "interpret (begin (define foo (x) (+ x 1)) (foo 3))" (Number 4) (interpret "(begin (define foo (x) (+ x 1)) (foo 3))"))
interpreter_begin_defineVariable = TestCase (assertEqual "interpret (begin (define x 5) x)" (Number 5) (interpret "(begin (define x 5) x)"))
interpreter_lambda = TestCase (assertEqual "interpret ((lambda (y) (+ y 2)) 40)" (Number 42) (interpret "((lambda (y) (+ y 2)) 40)"))
interpreter_multiArgLambda = TestCase (assertEqual "interpret ((lambda (x y) (+ x y)) (1 2))" (Number 3) (interpret "((lambda (x y) (+ x y)) (1 2))"))

interpreterTests = TestList [TestLabel "interpreter_add " interpreter_add,
                             TestLabel "interpreter_nestedSub" interpreter_nestedSub,
                             TestLabel "interpreter_and" interpreter_and,
                             TestLabel "interpreter_or" interpreter_or,
                             TestLabel "interpreter_eq" interpreter_eq,
                             TestLabel "interpreter_greaterThan" interpreter_greaterThan,
                             TestLabel "interpreter_lessThan" interpreter_lessThan,
                             TestLabel "interpreter_mul" interpreter_mul,
                             TestLabel "interpreter_list" interpreter_list,
                             TestLabel "interpreter_emptyList" interpreter_emptyList,
                             TestLabel "interpreter_empty" interpreter_empty,
                             TestLabel "interpreter_car" interpreter_car,
                             TestLabel "interpreter_cdr" interpreter_cdr,
                             TestLabel "interpreter_cons" interpreter_cons,
                             TestLabel "interpreter_consEmptyList" interpreter_consEmptyList,
                             TestLabel "interpreter_if" interpreter_if,
                             TestLabel "interpreter_let" interpreter_let,
                             TestLabel "interpreter_begin_define" interpreter_begin_define,
                             TestLabel "interpreter_begin_defineVariable" interpreter_begin_defineVariable,
                             TestLabel "interpreter_lambda" interpreter_lambda,
                             TestLabel "interpreter_multiArgLambda" interpreter_multiArgLambda]

main :: IO Counts
main = do
  runTestTT lexerTests
  runTestTT parserTests
  runTestTT interpreterTests
