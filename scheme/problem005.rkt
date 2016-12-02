#lang racket

(require scheme/mpair)

(define (divisor? x y)
  (= (remainder x y) 0))

(define (mrange-list bgn end)
  (if (>= bgn end)
    '()
    (mcons bgn (mrange-list (+ bgn 1) end))))

(define (change-elem! mls idx val)
  (if (<= idx 0)
    (set-mcar! mls val)
    (change-elem! (mcdr mls) (- idx 1) val)))

(define (change-elems! mls bgn step val)
  (when (and (< bgn (mlength mls)) (>= step 0))
    (change-elem! mls bgn val)
    (change-elems! mls (+ bgn step) step val)))

(define (gen-primes max-prime)
  (let loop((prime 2) (ret (mrange-list 2 (+ max-prime 1))))
    (cond
      ((>= prime max-prime) (filter number? (mlist->list ret)))
      ((number? (mlist-ref ret (- prime 2))) (change-elems! ret (+ (- prime 2) prime) prime #f) (loop (+ prime 1) ret))
      (else (loop (+ prime 1) ret)))))

(define (gen-factors num primes)
  (let loop((n num) (factors primes) (ret '(1)))
    (cond
      ((= n 1) (reverse ret))
      ((divisor? n (car factors)) (loop (/ n (car factors)) factors (cons (car factors) ret)))
      (else (loop n (cdr factors) ret)))))

(define (count-vals ls val)
  (count (lambda (x) (= x val)) ls))

(define (factors->powers factors primes)
  (if (null? primes)
    '()
    (cons (count-vals factors (car primes)) (factors->powers factors (cdr primes)))))

(define (solver max-num)
  (let ((primes (gen-primes max-num)))
    (let loop((num 2) (ret (make-list (length primes) 0)))
      (if (> num max-num)
        (apply * (map expt primes ret))
        (loop (+ num 1) (map max ret (factors->powers (gen-factors num primes) primes)))))))

(solver 20)
