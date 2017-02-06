#lang racket

(define (solver num)
  (let loop((n1 1) (n2 2) (ret 2))
    (let ((n3 (+ n1 n2)))
      (cond
        ((> n3 num) ret)
        ((even? n3) (loop n2 n3 (+ ret n3)))
        (else (loop n2 n3 ret))))))

(solver 4000000)
