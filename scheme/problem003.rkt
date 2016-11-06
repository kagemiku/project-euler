#lang racket

(define (divisor? x y)
  (= (remainder x y) 0))

(define (solver num)
  (let loop((x num) (factor 2))
    (cond
      ((= x 1) factor)
      ((divisor? x factor) (loop (/ x factor) factor))
      (else (loop x (+ factor 1))))))

(solver 600851475143)
