#lang racket #| * CSC324H5 Fall 2023: Assignment 4 * |#
#|
Module:        a4
Description:   Assignment 4
Copyright: (c) University of Toronto Mississsauga
               CSC324 Principles of Programming Languages, Fall 2023
|#

; Import the testing library
(module+ test
  (require rackunit))

; Import 
(require "a4.rkt")

; This file provides several suggestions for testing your A4 code.
; Please read through each part carefully.


; =============== Part 1: Tracing Execution =====================

; One way to test your code during development is to have Racket trace
; and print out calls to the functions cps-expr and cps-insert in the
; REPL. Uncomment the code below to do so. However, you may want to keep
; these commented when running unit tests to avoid the verbosity.
(require racket/trace)
;(trace cps-expr)    ; tell racket to trace calls to cps-expr
;(trace cps-insert)  ; ditto for cps-insert
; (cps-define '(define x (+ (f 1) 2)))


; =============== Part 2: Using an Interpreter =====================

; Another way to ensure that your CPSed code is correct is to ensure that
;   (1) If the CPSed version of an expression evaluates to a number or boolean,
;       that it evaluates to the same number/boolean as the unCPSed version.
;       This can be checked using an *interpreter*, written for you below.
;   (2) We can check that the CPSed expression follows the grammar provided
;       to you in the handout. We will use such a checker to test your code,
;       but this code is not provided to you. (You can write a checker
;       yourself using the BNF grammar provided to you in the handout.)
; This section contains the intepreter used in the automatic tests

#|
(interp expr env) -> (or number? boolean? list?)
  expr: (or number? symbol? list?)
    A definition that follows the grammar for <expr> or <c-expr> from the handout
  env: hash?

  Returns the value of the expression.
|#
(define/match (interp expr env)
  [((list 'lambda params body) env)
   (list 'closure params body env)]
  [((list '+ e1 e2) env)
   (+ (interp e1 env) (interp e2 env))]
  [((list '- e1 e2) env)
   (- (interp e1 env) (interp e2 env))]
  [((list '= e1 e2) env)
   (= (interp e1 env) (interp e2 env))]
  [((list 'if condition then alt) env)
   (if (interp condition env)
       (interp then env)
       (interp alt env)) ]
  [((list fnexpr argexpr) env) ; 1 argument function application
   (let* ([fnval   (interp fnexpr env)] ; closure
          [fnparam (first (second fnval))]
          [fnbody  (third fnval)]
          [fnenv   (fourth fnval)]
          [argval  (interp argexpr env)]
          [newenv  (hash-set fnenv fnparam argval)])
     (interp fnbody newenv))]
  [((list fnexpr arg1expr arg2expr) env) ; 2 arg function application
   (let* ([fnval   (interp fnexpr env)] ; closure
          [fnparam (first (second fnval))]
          [kparam  (second (second fnval))]
          [fnbody  (third fnval)]
          [fnenv   (fourth fnval)]
          [arg1val (interp arg1expr env)]
          [arg2val (interp arg2expr env)]
          [newenv  (hash-set (hash-set fnenv fnparam arg1val) kparam arg2val)])
     (interp fnbody newenv))]
  [(expr env)
   (if (symbol? expr) (hash-ref env expr) expr)])



; You can write helper functions freely
(module+ test
  ; We can check the CPSed output directly.
  ; However, be careful that our use of gensym means that the
  ; output will not be deterministic.
  ; Note that this is NOT the suggested way to test your code:
  ; instead we suggest that you think about testing more systematically
  ; and build tools to help you test.
  (test-equal? "CPSing (+ (f 1) 2)"
             (let* ([cexpr (cps-expr '(+ (f 1) 2) '(_))])
               ; cexpr ~= '(f 1 (lambda (r) (+ r 2)))
               (and (equal? (first cexpr) 'f)
                    (equal? (second cexpr) 1)
                    (equal? (first (third cexpr)) 'lambda)
                    (equal? (first (third (third cexpr))) '+)
                    (equal? (third (third (third cexpr))) '2)))
             #t)

  ; The interpreter provided to you is an example of a tool that we can
  ; build to help test our code.
  (test-equal? "CPSing (+ ((lambda (x) (+ x 1)) 1) 2) yields the same value"
               (interp '(+ ((lambda (x) (+ x 1)) 1) 2) (hash))
               (interp (cps-expr '(+ ((lambda (x) (+ x 1)) 1) 2) '(_))
                       (hash)))

  ; TODO: Write more tests. Testing is an important part of programming,
  ; so you and your partner must write your own tests. Do not share your
  ; tests with anyone else.
)

