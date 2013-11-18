;; Programming Languages, Homework 5

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

;; Problem 1

(define (racketlist->mupllist xs)
  (if (null? xs) 
      (aunit)
      (apair (car xs) (racketlist->mupllist (cdr xs)))))

(define (mupllist->racketlist e)
  (if (aunit? e)
      null
      (cons (apair-e1 e) (mupllist->racketlist (apair-e2 e)))))


;; Problem 2

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
  (cond
    ;; int
    [(int? e) e]
    ;; unit
    [(aunit? e) e]
    ;; closure
    [(closure? e) e]
    ;; variable use
    [(var? e) (envlookup env (var-string e))]
    ;; addition
    [(add? e) 
     (let ([v1 (eval-under-env (add-e1 e) env)]
           [v2 (eval-under-env (add-e2 e) env)])
       (if (and (int? v1)
                (int? v2))
           (int (+ (int-num v1) 
                   (int-num v2)))
           (error "MUPL addition applied to non-number")))]
    ;; function definition
    [(fun? e)
     (let ([name (fun-nameopt e)])
       (if name
           (closure e (cons (cons name e) env))
           (closure e env)))]
    ;; ifgreater
    [(ifgreater? e)
     (let ([e1 (eval-under-env (ifgreater-e1 e) env)]
           [e2 (eval-under-env (ifgreater-e2 e) env)])
       (if (and (int? e1) (int? e2))
           (if (> (int-num e1) (int-num e2)) 
               (eval-under-env (ifgreater-e3 e) env)
               (eval-under-env (ifgreater-e4 e) env))
           (error "ifgreater requires first two params to be ints")))]
    ;; function call
    [(call? e)
     (let ([funexp (eval-under-env (call-funexp e) env)]
           [arg (eval-under-env (call-actual e) env)])
       (if (closure? funexp)
           (let* ([env (closure-env funexp)]
                  [fn (closure-fun funexp)]
                  [param (fun-formal fn)]
                  [body (fun-body fn)])
             (eval-under-env body (cons (cons param arg) env)))
           (error "call requires a first argument of a function")))]
    ;; mlet
    [(mlet? e)
     (let ([var (mlet-var e)]
           [e (mlet-e e)]
           [body (mlet-body e)])
       (eval-under-env body (cons (cons var (eval-under-env e env)) env)))]
    ;; pair creation
    [(apair? e) (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
    ;; fst
    [(fst? e)
     (let ([pr (eval-under-env e env)])
       (if (apair? pr)
           (apair-e1 pr)
           (error "fst requires an argument of type apair")))]
    ;; snd
    [(snd? e)
     (let ([pr (eval-under-env e env)])
       (if (apair? pr)
           (apair-e2 pr)
           (error "fst requires an argument of type apair")))]
    ;; isaunit
    [(isaunit? e)
     (let ([e (eval-under-env (isaunit-e e) env)])
       (if (aunit? e) 
           (int 1) 
           (int 0)))]
    [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3

(define (ifaunit e1 e2 e3) (mlet "x" (isaunit e1) (ifgreater (var "x") (int 0) e2 e3)))

(define (mlet* lstlst e2) 
  (if (null? lstlst)
      e2
      (mlet (caar lstlst) (cdar lstlst) (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4) 
  (mlet* 
   (list (cons "_x" e1) (cons "_y" e2))
   (ifgreater 
    (add 
     (ifgreater (var "_x") (var "_y") (int 1) (int 0)) 
     (ifgreater (var "_y") (var "_x") (int 1) (int 0))) 
    (int 0)
    e4
    e3)))


;; Problem 4

(define mupl-map 
  (fun #f "fn" 
       (fun "map" "xs" 
            (ifeq (isaunit (var "xs")) (int 1) 
                  (aunit) 
                  (apair 
                   (call (var "fn") (fst (var "xs"))) 
                   (call (var "map") (snd (var "xs"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i" 
             (fun "mapi" "xs"
                  (call 
                   (call (var "map") 
                         (fun #f "x" (add (var "x") (var "i")))) 
                   (var "xs"))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
