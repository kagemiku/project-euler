#lang racket

(define (integer-reverse num)
  (do ((num num (quotient num 10))
       (r 0 (+ (* 10 r) (modulo num 10))))
    ((zero? num) r)))

(define (palindromic? num)
  (= num (integer-reverse num)))

(define (gen-muls max-num min-num)
  (let loop((x max-num) (y max-num) (ret '()))
    (cond
      ((< y min-num) ret)
      ((< x y) (loop max-num (- y 1) ret))
      ((palindromic? (* x y)) (loop max-num (- y 1) (cons (* x y) ret)))
      (else (loop (- x 1) y ret)))))

(define (solver max-num min-num)
  (apply max (gen-muls max-num min-num)))

(solver 999 100)

