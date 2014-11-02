
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
(define (sequence low high stride) 
  (cond [(< high low) null]
        [#t (cons low (sequence (+ low stride) high stride))]))

(define (string-append-map xs suffix)
  (map (lambda (x) 
         (string-append x suffix))
       xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(= 0 (length xs)) (error "list-nth-mod: empty list")]
        [#t (let ([i (modulo n (length xs))])
            (car (list-tail xs i)))]))

(define (stream-for-n-steps s n)
  (cond [(= n 0) null]
        [#t (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))]))

(define funny-number-stream
  (letrec ([f (lambda (x) 
                (let ([z (cond [(= 0 (modulo x 5)) (- 0 x)]
                               [#t x])])
                  (cons z (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([dan (lambda () (cons "dan.jpg" dog))]
           [dog (lambda () (cons "dog.jpg" dan))])
    (lambda () (dan))))

(define (stream-add-zero s)
  (letrec ([f (lambda() (cons (cons 0 (car (s))) f))])
    (lambda () (f))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda(x) 
                (cons (cons (list-nth-mod xs x) (list-nth-mod ys x)) 
                      (lambda() (f (+ 1 x)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([f (lambda(x) 
                (cond [(= x (vector-length vec)) #f]
                      [(pair? (vector-ref vec x)) 
                       (if (equal? (car (vector-ref vec x)) v) 
                           (vector-ref vec x)
                           (f(+ 1 x)))]
                      [#t (f (+ 1 x))]))])
          (f 0)))