#lang racket

(define (divisor? x y)
  (= (remainder x y) 0))

(define (range-list bgn end)
  (if (>= bgn end)
    '()
    (cons bgn (range-list (+ bgn 1) end))))

(define (solver num)
  (apply + (filter
             (lambda (x) (or (divisor? x 3) (divisor? x 5)))
             (range-list 2 num))))

(solver 1000)
