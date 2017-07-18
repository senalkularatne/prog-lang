;; Rafael J. Colon & Senal Kularatne
;;Cs4003 | Final Project
;;Source code file

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Part 1 - Warm-up

;; CHANGE (put your solutions here)
(define (racketlist->mupllist ls) ;;translate RACKET lists into MUPL lists 
  (cond [(null? ls) (aunit)]
        [(number? (car ls)) (cons (int (car ls)) (racketlist->mupllist (cdr ls)))]
        [(string? (car ls)) (cons (var (car ls)) (racketlist->mupllist (cdr ls)))]))

(define (mupllist->racketlist ls) ;;translate MUPL lists into RACKET lists
  (cond [(aunit? ls) null]
        [(int? (car ls)) (cons (int-num (car ls)) (mupllist->racketlist (cdr ls)))]
        [(var? (car ls)) (cons (var-string (car ls)) (mupllist->racketlist (cdr ls)))])) 
;; Part 2 - Implementing the language

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e) e]  ;;int
        [(closure? e) e]  ;;closure
        [(aunit? e) e]  ;;aunit
        [(apair? e) ;;apair
         (let ([e1 (eval-exp (apair-e1 e))]
               [e2 (eval-exp (apair-e2 e))])
           (apair e1 e2))]
        [(fun? e)
         (cond ([string? (fun-nameopt e) (eval-exp (closure (e env)))]
                [#t (eval-exp (closure (lambda() env)))]))]
        [(ifgreater? e) ;; ifgreater
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-exp (ifgreater-e3 e))
                   (eval-exp (ifgreater-e4 e)))             
               (error "MUPL ifgreater applied to non-number")))]
        [(mlet? e)  ;;mlet
         (let ([v (eval-exp (mlet-e e))])
           (eval-under-env (mlet-body e) (cons (cons (mlet-var e) v) env)))]
        [(isaunit? e) ;;isaunit
         (let ([sub (eval-exp(isaunit-e e))])
           (if (aunit? sub) (int 1) (int 0)))]
        [(call? e) ;;call
         (let ([e1 (eval-under-env (call-funexp e) env)]
               [e2 (eval-under-env (call-actual e) env)])
           (if (closure? e1) (eval-under-env ((closure-fun e1) (cons (closure-env e1) (cons (e1 (eval-exp e2)))))) (error "bad MUPL call expression")))]
        [(fst? e) ;;fst
         (let ([res (eval-exp (fst-e e))])
           (if (pair? res) (apair-e1 res) (apair-e2 res)))]
        [(snd? e) ;;snd
         (let ([res (eval-exp (snd-e e))])
           (if (pair? res) (apair-e2 res) (apair-e1 res)))]
        [#t (error "bad MUPL expression")]))


;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Part 3 - Expanding the Language
(define (ifaunit e1 e2 e3)
  (if (= 1 (int-num (eval-exp (isaunit e1)))) (eval-exp e2) (eval-exp e3)))

(define (mlet* lst fin)
  (define (helper evals l)
    (cond [(null? l) (eval-under-env fin evals)]
          [#t (helper (cons (cons (car (car l)) (eval-exp (cdr (car l)))) evals) (cdr l))]))
  (helper null lst))

(define (ifeq e1 e2 e3 e4) 
  (if (= (int-num e1) (int-num e2))
      (eval-exp e3) (eval-exp e4)))


;; Part 4 - Using the language


;Bind to the Racket variable mupl-map a MUPL function that acts like map (as we used extensively in ML). Your function should be curried: it should take a MUPL function and return a MUPL function that takes a MUPL list and applies the function to every element of the list returning a new MUPL list. Recall a MUPL list is aunit or a pair where the second component is a MUPL list. 
;;(define mupl-map
  ;;(fun "f1" "f" (fun "f2" "l" (cond [(isaunit? (var "l")) aunit]
    ;;                                [#t (cons (((var "f") (car (var "l")))))]))))

                               
;Bind to the Racket variable mupl-mapAddN a MUPL function that takes an MUPL integer i and returns a MUPL function that takes a MUPL list of MUPL integers and returns a new MUPL list of MUPL integers that adds i to every element of the list. Use mupl-map (a use of mlet is given to you to make this easier).
;;(define mupl-mapAddN 
  ;;(mlet "map" mupl-map
    ;;    (fun #f "i" ((var "map") (add (var "i"))))))