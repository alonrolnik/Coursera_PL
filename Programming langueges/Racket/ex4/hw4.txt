
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high) null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs ))

(define (list-nth-mod xs n)
  (cond [(null? xs) (error "list-nth-mod: empty list") ]
        [(< n 0) (error "list-nth-mod: negative number")]
        [#t (list-ref xs (remainder n (length xs)))]))

(define (stream-for-n-steps s n)
  (if (= n 0) null (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define (funny-number-stream)
  (letrec ([f (lambda(n)
                (if (= (remainder n 5) 0) (cons (* n -1) (lambda () (f (+ n 1))))
                    (cons n (lambda() (f (+ n 1))))))])
    (f 1)))

(define (dan-then-dog)
  (letrec ([f (lambda(b) (cons
                          (if b "dan.jpg" "dog.jpg")  
                          (lambda() (f (not b)))))])
    (f #t)))

(define (stream-add-zero s)
  (letrec ([f (lambda(s1)
                (cons (cons 0 (car (s1))) (lambda () (f (cdr (s1))))))])
    (lambda() (f s))))

(define (cycle-lists xs ys)
  (letrec ([xs-len (length xs)]
           [ys-len (length ys)]
           [f (lambda(n)
                (cons (cons (list-nth-mod xs (+ n xs-len)) (list-nth-mod ys (+ n ys-len)))
                      (lambda() (f (+ n 1)))))])
    (lambda() (f 0))))

(define (vector-assoc v vec)
  (letrec (
           [vec-len (vector-length vec)]
           [f (lambda(n) (cond
                           [(= n vec-len) #f]
                           [(not (pair? (vector-ref vec n))) (f (+ n 1))] 
                           [(equal? (car (vector-ref vec n)) v) (vector-ref vec n)]
                           [#t (f (+ n 1))]))])
    (f 0)))

(define (cached-assoc xs n)
  (letrec (
           [memo (make-vector n #f)]
           [index 0]
           [f (lambda(v) (let ([p-memo (vector-assoc v memo)])
                           (if (not p-memo) (let ([p-xs (assoc v xs)])
                                              (begin (vector-set! memo index p-xs)
                                                     (set! index (remainder (+ index 1) n)))
                                              p-xs)
                               p-memo)))])
    (lambda(v1) (f v1))))
                                              
    
                            