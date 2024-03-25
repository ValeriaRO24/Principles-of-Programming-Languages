#lang racket #| * CSC324H5 Fall 2023: Week 4 Lab * |#
#|
Module:        w04lab
Description:   Week 4 Lab: Functional Data Structures
Copyright: (c) University of Toronto Mississsauga
               CSC324 Principles of Programming Languages, Fall 2023
|#

; This specifies which functions this module exports. Don't change this!
(provide execute)

; NOTE: As is common to testing frameworks, by default DrRacket only displays
; output for *failing* tests. If you run the module with the tests uncommented
; but don't see any output, that's good---the tests all passed! (If you want
; to double-check this, you can try breaking test cases and seeing the "fail"
; output yourself.)
(module+ test
  ; Import the testing library
  (require rackunit))

;-------------------------------------------------------------------------------

#|
(execute cmds) -> list? 
  cmds: list?
    A list of "get" and "set" commands

  Returns a list of resulting numbers from the "get" commands
|#
(define (execute lst)
  set = (hash '() '())


  
  (helper lst (hash) '())


  ) 

(define/match (helper lst hash ret)

  [((cons x xs) hash ret)
   (cond [(equal? (first x) 'set)(helper (rest lst) (hash-set hash (second x) (third x)) ret)]
  [else (cond [(hash-has-key? hash (second x))
               (helper (rest lst) hash (append ret (list(hash-ref hash (second x)))))]
  [else (helper (rest lst) hash (append ret (list 'error)))])])]
                                 
  [('() hash ret) ret])



(module+ test
  (test-equal? "execute: set and get a value"
               (execute '((set a 3) (get a)))
               '(3))
  (test-equal? "execute: set a value multiple times"
               (execute '((set a 3) (set a 4) (get a)))
               '(4))
  (test-equal? "execute: test from the handout"
               (execute '((set a 3) (set b 4) (get a) (set b 5) (get b) (get c)))
               '(3 5 error))
)

