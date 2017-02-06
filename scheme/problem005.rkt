#lang racket

(define (solver max-num)
  (do ((i max-num (- i 1))
       (ret 1 (lcm ret i)))
    ((zero? i) ret)))

(solver 20)
