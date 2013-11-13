#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (< high low) 
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix) 
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([pr (s)]) (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1))))))

(define (stream-gen index generator)
  (cons (generator index) (lambda () (stream-gen (+ index 1) generator))))

(define (funny-number-stream)
  (stream-gen 1 (lambda (index) (if (= (remainder index 5) 0) 
                                    (- 0 index) 
                                    index))))

(define (dan-then-dog)
  (stream-gen 1 (lambda (index) (if (= (remainder index 2) 0) 
                                    "dog.jpg" 
                                    "dan.jpg"))))

(define (stream-add-zero s)
  (lambda () (let ([pr (s)]) (cons (cons 0 (car pr)) (stream-add-zero (cdr pr))))))

(define (cycle-lists xs ys)
  (letrec ([aux (lambda (n) (cons 
                             (cons (list-nth-mod xs n) (list-nth-mod ys n)) 
                             (lambda () (aux (+ n 1)))))])
    (lambda () (aux 0))))

(define (vector-assoc v vec)
  (letrec 
      ([l (vector-length vec)]
       [aux (lambda (n) (if (>= n l) #f 
                            (let 
                                ([elem (vector-ref vec n)]) 
                              (if (and (pair? elem) (equal? (car elem) v)) 
                               elem 
                               (aux (+ n 1))))))])
    (aux 0)))

(define (cached-assoc xs n)
  (let 
      ([cache (make-vector n #f)]
       [pos 0])
    (lambda (v) (let ([cache-result (vector-assoc v cache)])
                  (if cache-result 
                      cache-result 
                      (let 
                          ([assoc-result (assoc v xs)]) 
                        (if assoc-result 
                            (begin
                              (vector-set! cache pos assoc-result)
                              (set! pos (if (< pos (- n 1)) (+ pos 1) 0))
                              assoc-result)
                            assoc-result)))))))