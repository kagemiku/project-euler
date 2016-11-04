#lang racket

(define (number->list number)
  (string->list (number->string number)))

(define (palindromic? num)
  (let loop((ls0 (number->list num)) (ls1 (reverse (number->list num))) (cnt (truncate (/ (length (number->list num)) 2))))
    (cond
      ((<= cnt 0) #t)
      ((eqv? (car ls0) (car ls1)) (loop (cdr ls0) (cdr ls1) (- cnt 1)))
      (else #f))))

(define (gen-muls max-num)
  (let loop((x max-num) (y max-num) (ret '()))
    (cond
      ((= y 0) ret)
      ((= x 0) (loop max-num (- y 1) ret))
      (else (loop (- x 1) y (cons (* x y) ret))))))

(define (solver max-num)
  (apply max (filter palindromic? (gen-muls max-num))))

(solver 999)

