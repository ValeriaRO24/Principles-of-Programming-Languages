#lang racket #| CSC324 Fall 2022: Week 11 Lab |#

; Do not add additional imports
(require "mk.rkt")
(require "numbers.rkt")
(require "evalo.rkt")

; This specifies which functions this module exports. Don't change this!
(provide split-even-oddo changeo)

; Import the testing library
(module+ test
  (require rackunit))


;-------------------------------------------------------------------------------
; * Task 1: The relation split-even-oddo*
;-------------------------------------------------------------------------------

#|
(split-even-oddo xsys xs ys)
  xsys: A list.
  xs: A list.
  ys: A list.

  Returns a pair of lists (xs . ys), where
  `xs` contains elements of `xsys` in the even indices and
  `ys` contains elements of `xsys` in the odd indices.

  At this point in the course, you should be able to write
  this function without any help or starter code. However,
  we're providing structured starter code to make the
  transition from the function to the relation more clear.
|#
(define (split xsys)
  (cond [(void)  ; base case condition todo
         (void)] ; base case body todo
        [else
         (let* ([elem    (first xsys)]
                [result  (split (void))] ; todo: replace "(void)"
                [xs      (car result)]  ; access the first element of the pair
                [ys      (cdr result)]) ; access the second element of the pair
           (void))]))

#|
(split-even-oddo xsys xs ys)
  xsys: A list.
  xs: A list.
  ys: A list.

  Succeeds if `xs` contains elements of `xsys` in the even indices
  and `ys` contains elements of `xsys` in the odd indices.
|#
(define (split-even-oddo xsys xs ys)
  (void)) ; todo

(module+ test
  (require rackunit)
  (test-equal? "split-even-oddo - simple"
               (first (run 1 (xs ys) (split-even-oddo '(1 2 3) xs ys)))
               '((1 3) (2))))

;-------------------------------------------------------------------------------
; * Task 2: Fixing Changeo *
;-------------------------------------------------------------------------------

#|
(changeo coins total denoms)
  coins: A list of miniKanren numbers representing the list of coins
         that should make up the total value.
  total: A miniKanren number representing the total worth of the coins
  denoms: A list of miniKanren numbers representing the allowed denominations
          of the coins.

  The relation holds if `coins` is a valid way to give change with value
  `total` using coins in allowed in `denoms`.

  This current implementation produces duplicate results.
|#
(define (changeo coins total denoms)
  (conde ((== coins '())
          (zeroo total))
         ((fresh (c coins^ subtotal)
            (== coins (cons c coins^))
            (membero c denoms)
            (pluso subtotal c total)
            (changeo coins^ subtotal denoms)))))

#|
(membero elem lst)
  elem: A term
  lst:  A list of terms

  The relation holds if `elem` is element of the list `lst`.

  This is a helper function for the `changeo` function. You may or may not
  choose to use this function in your implementation of `changeo`.
|#
(define (membero elem lst)
  (fresh (first rest)
    (== lst (cons first rest))
      (conde
        ((== first elem))
        ((membero elem rest)))))

#;(module+ test
  (test-equal? "changeo-simple"
               (let*
                 ([result (run 2
                               (coins)
                               (changeo coins
                                        (build-num 2)
                                        (list (build-num 5) (build-num 1))))]
                  [resultnum  (map (lambda (coins) (map toint coins))
                                   result)])
                 resultnum)
               '((1 1))))

;-------------------------------------------------------------------------------
; * Task 3: Program Synthesis: OPTIONAL *
;-------------------------------------------------------------------------------

#|
(pbe (<input> <output>) ...)
  A macro used to fill in the body a function:
    (lambda (arg) _______________)
  So that the function is consistent with the input-output examples provided.

  Return a list with a single element containing one answer.

  We recommend writing `pbe` as a macro, but you can optionally implement `pbe`
  as a function.
|# 
#;(define-syntax pbe
  (syntax-rules ()
    ))


#;(module+ test
  (test-equal? "pbe identity function"
               (pbe ('x 'x) ('y 'y))
               '(arg))
  (test-equal? "pbe constant function"
               (pbe ('x 'x) ('y 'x))
               '('x))
  (test-equal? "pbe list function"
               (pbe ('x '(x)) ('y '(y)))
               '((list arg))))


