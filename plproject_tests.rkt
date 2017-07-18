;; Rafael J. Colon & Senal Kularatne
;;Cs4003 | Final Project
;;Test file

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file
(require "plproject.rkt")
(require rackunit)

;; ---- Part 1 tests --
;racketlist->mupllist:
(racketlist->mupllist '(1 2 3 4 5)) ;;;;(list* (int 1) (int 2) (int 3) (int 4) (int 5) (aunit))
(racketlist->mupllist '("H" "ello" "ma" "n"))  ;;;;(list* (var "H") (var "ello") (var "ma") (var "n") (aunit))
(racketlist->mupllist '()) ;;;;(aunit)

;mupllist->racket:
(mupllist->racketlist (list* (int 1) (int 2) (int 3) (int 4) (int 5) (aunit))) ;;;;'(1 2 3 4 5)
(mupllist->racketlist (list* (var "H") (var "ello") (var "ma") (var "n") (aunit))) ;;;;'("H" "ello" "ma" "n")
(mupllist->racketlist (aunit)) ;;; ()

;; ---- Part 2 tests ----
; ifgreater 
(eval-exp (ifgreater (int 1) (int 8) (int 2) (int 0)))
(eval-exp (ifgreater (int 5) (int 4) (int 5) (int 4))) 
(eval-exp (ifgreater (int 7) (int 7) (int 7) (int 6))) 

; add
(eval-exp (add (int 100) (int 50)))
(eval-exp (add (int -100) (int -50)))
(eval-exp (add (int 0) (int -100)))

; snd
(eval-exp (snd (apair (int 1) (int 2))))

;mlet
(eval-exp (mlet "x" (int 1) (add (int 5) (var "x"))))
(eval-exp (mlet "x" (int 1) (aunit)))

;isaunit
(eval-exp (isaunit (closure '() (fun #f "x" (aunit)))))

;; ---- Part 3 tests --
;ifaunit
(eval-exp (ifaunit (int 1) (int 2) (int 3)))
(eval-exp (ifaunit (aunit) (add (int 1) (int 2)) (int 2)))

;ifeq
(eval-exp (ifeq (int 4) (int 5) (int 6) (int 7)))
(eval-exp (ifeq (int 2) (int 2) (int 4) (int 5)))
(eval-exp (ifeq (int 1) (int 6) (int 3) (int -3)))
(eval-exp (ifeq (int 10) (int 5) (int 7) (int 3)))
(eval-exp (ifeq (int 7) (int 7) (int 4) (int 0)))

;mlet*
(eval-exp (mlet* (list (cons "a" (int 1)) (cons "b" (add (int 1) (int 5)))) (add (var "a") (var "b"))))
(eval-exp (mlet* (list (cons "a" (int 1)) (cons "b" (add (int 1) (int 5))) (cons "c" (int 1))) (add (var "c") (add (var "a") (var "b")))))
(eval-exp (mlet* (list (cons "a" (int 1))) (add (var "a") (int 0))))

;; ---- Part 4 tests --
;;(define test1
  ;;(mupllist->racketlist
   ;;(eval-exp (call (call mupl-mapAddN (int 7))
                   ;(racketlist->mupllist 
                    ;(list (int 3) (int 4) (int 9)))))))