#lang racket

(define (divisor? x y)
  (= (remainder x y) 0))

(define (solver num)
  (apply + (filter
             (lambda (x) (or (divisor? x 3) (divisor? x 5)))
             (range 2 num))))

(solver 1000)
