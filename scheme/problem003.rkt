#lang racket

(define (divisor? x y)
  (= (remainder x y) 0))

(define (prime? value)
    (let loop((val (truncate (sqrt value))))
      (cond
        ((= val 1) #t)
        ((divisor? value val) #f)
        (else (loop (- val 1))))))

(define (solver value)
  (let loop((val (truncate (sqrt value))))
    (if (and (prime? val) (divisor? value val))
      val
      (loop (- val 1)))))

(solver 600851475143)

