#lang racket

(define (range-list bgn end)
  (if (>= bgn end)
    '()
    (cons bgn (range-list (+ bgn 1) end))))

(define (divisible? num ls)
  (cond
    ((null? ls) #t)
    ((= (remainder num (car ls)) 0) (divisible? num (cdr ls)))
    (else #f)))

(define (solver max-elem)
  (let loop((ls (range-list 2 (+ max-elem 1))) (ret max-elem))
    (if (divisible? ret ls)
      ret
      (loop ls (+ ret max-elem)))))

(solver 20)
