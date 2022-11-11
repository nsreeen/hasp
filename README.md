# hasp

## How to run
You need (Stack)[https://docs.haskellstack.org/en/stable/] to run this project.

You can then start a repl with by running the following command in the root directory:
`stack build && stack exec hasp`

## Functionality
Hasp has the following builtin words:

- basic operators: `+`, `-`, `*`
- boolean operators: `and`, `or`
- mathematical comparison operators: `<`, `>`, `==`
- list functions: `list`, `empty?`, `cons`, `car`, `cdr`
- program control functions: `if`, `begin`, `define`, `let`, `lambda`

Hasp support integers, strings, boolean, functions, and lists.  

Here are some valid programs and their output:

| program                                    | output       |
| ------------------------------------------ | ------------ |
| `(+ 1 2)`                                  | 3            |
| `(+ 1 2 3)`                                | 6            |
| `(* 1 2)`                                  | 2            |
| `(* 1 2 3)`                                | 6            |
| `(- 2 2)`                                  | 0            |
| `(list 1 2 3)`                             | [1,2,3]      |
| `(empty? (list 1 2 3))`                    | False        |
| `(car (list 1 2 3))`                       | 1            |
| `(cdr (list 1 2 3))`                       | [2,3]        |
| `(cons 1 (list 2 3))`                      | [1,2,3]      |
| `(if true 1 2)`                            | 1            |
| `(begin (+ 1 2) (== false false))`         | True         |
| `(begin (define foo (x) (+ x 1)) (foo 5)`  | 6            |
| `(begin (define foo 3) foo`                | 3            |
| `(let (x 1 y 2) (+ x y))`                  | 3            |
| `((lambda (x) (+ 1 x)) 10)`                | 11           |
