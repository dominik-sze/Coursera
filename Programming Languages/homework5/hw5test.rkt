#lang racket
;; Programming Languages Homework5 Simple Test
;; Save this file to the same directory as your homework file
;; These are basic tests. Passing these tests does not guarantee that your code will pass the actual homework grader

;; Be sure to put your homework file in the same folder as this test file.
;; Uncomment the line below and change HOMEWORK_FILE to the name of your homework file.
(require "hw5.rkt")

(require rackunit)

(define tests
  (test-suite
   "Sample tests for Assignment 5"
   (check-equal? (eval-exp (aunit)) (aunit) "aunit test")
   (check-equal? (eval-exp (int 3)) (int 3) "int test")
   (check-equal? (eval-exp (apair (int 3) (int 4))) (apair (int 3) (int 4)) "apair test")
   (check-equal? (eval-exp (apair (add (int 3) (int 4)) (int 4))) (apair (int 7) (int 4)) "apair test")
   (check-equal? (eval-exp (apair (add (int 3) (int 4)) (add (int 1) (int 4)))) (apair (int 7) (int 5)) "apair test")
   ;; check racketlist to mupllist with normal list
   (check-equal? (racketlist->mupllist (list)) (aunit) "racketlist->mupllist test 1")
   (check-equal? (racketlist->mupllist (list (int 3))) (apair (int 3) (aunit)) "racketlist->mupllist test 2")
   (check-equal? (racketlist->mupllist (list (int 3) (int 4))) (apair (int 3) (apair (int 4) (aunit))) "racketlist->mupllist test 3")
   (check-equal? (racketlist->mupllist (list 1 2 3 4 5)) (apair 1 (apair 2 (apair 3 (apair 4 (apair 5 (aunit)))))) "racketlist->mupllist test 4")
   
   ;; check mupllist to racketlist with normal list
   (check-equal? (mupllist->racketlist (aunit)) (list) "mupllist->racketlist 1")
   (check-equal? (mupllist->racketlist (apair (int 3) (aunit))) (list (int 3)) "mupllist->racketlist 2")
   (check-equal? (mupllist->racketlist (apair (int 3) (apair (int 4) (aunit)))) (list (int 3) (int 4)) "mupllist->racketlist 3")
   (check-equal? (mupllist->racketlist (apair (int 3) (apair (int 4) (apair 5 (aunit))))) (list (int 3) (int 4) 5) "mupllist->racketlist 4")

   (check-equal? (eval-exp (add (int 3) (int 4))) (int 7) "ifgreater test")
   
   ;; tests if ifgreater returns (int 2)
   (check-equal? (eval-exp (ifgreater (int 3) (int 4) (int 3) (int 2))) (int 2) "ifgreater test 1")
   (check-equal? (eval-exp (ifgreater (int 3) (int 2) (int 3) (int 2))) (int 3) "ifgreater test 2")
   (check-equal? (eval-exp (ifgreater (int 3) (int 3) (int 3) (int 2))) (int 2) "ifgreater test 2")
   
   ;; mlet test
   (check-equal? (eval-exp (mlet "x" (int 1) (add (int 5) (var "x")))) (int 6) "mlet test")
   (check-equal? (eval-exp (mlet "x" (int 1) (aunit))) (aunit) "mlet test")
   
   ;; call test
   (check-equal? (eval-exp (call (fun #f "x" (add (var "x") (int 7))) (int 1))) (int 8) "call test")
   (check-equal? (eval-exp (call (closure '() (fun #f "x" (apair (var "x") (int 7)))) (add (int 1) (int 2)))) (apair (int 3) (int 7)) "call test")
   
   ;;snd test
   (check-equal? (eval-exp (fst (apair (int 1) (int 2)))) (int 1) "snd test")
   (check-equal? (eval-exp (fst (apair (int 1) (apair (int 2) (int 3))))) (int 1) "snd test")
   (check-equal? (eval-exp (snd (apair (int 1) (int 2)))) (int 2) "snd test")
   (check-equal? (eval-exp (snd (apair (int 1) (apair (int 2) (int 3))))) (apair (int 2) (int 3)) "snd test")
   (check-exn (regexp "MUPL snd applied to non-pair") (lambda () (eval-exp (snd (int 1)))) "snd test")
   
   ;; isaunit test
   (check-equal? (eval-exp (isaunit (closure '() (fun #f "x" (aunit))))) (int 0) "isaunit test")
   (check-equal? (eval-exp (isaunit (aunit))) (int 1) "isaunit test")
   
   ;; ifaunit test
   (check-equal? (eval-exp (ifaunit (int 1) (int 2) (int 3))) (int 3) "ifaunit test")
   (check-equal? (eval-exp (ifaunit (aunit) (add (int 2) (int 3)) (int 3))) (int 5) "ifaunit test")
   
   ;; mlet* test
   (check-equal? (eval-exp (mlet* (list) (int 10))) (int 10) "mlet* test")
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 10))) (var "x"))) (int 10) "mlet* test")
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 10)) (cons "y" (add (int 5) (var "x")))) (var "y"))) (int 15) "mlet* test")
   
   ;; ifeq test
   (check-equal? (eval-exp (ifeq (int 1) (int 2) (int 3) (int 4))) (int 4) "ifeq test")
   (check-equal? (eval-exp (ifeq (int 1) (int 1) (int 3) (int 4))) (int 3) "ifeq test")
   
   ;; mupl-map test
   (check-equal? (eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 1) (aunit)))) 
                 (apair (int 8) (aunit)) "mupl-map test")
   
   ;; problems 1, 2, and 4 combined test
   (check-equal? (mupllist->racketlist
   (eval-exp (call (call mupl-mapAddN (int 7))
                  (racketlist->mupllist 
                    (list (int 3) (int 4) (int 9)))))) (list (int 10) (int 11) (int 16)) "combined test")
   
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
