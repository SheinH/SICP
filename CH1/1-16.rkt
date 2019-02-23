#lang sicp
(define (expt b n)
  (if (= n 0)
      1
      (iter-expt n b 1)))
(define (iter-expt n b a)
  (cond ((= n 1) (* a b))
        ((odd? n) (iter-expt (- n 1) b (* a b)))
        (else (iter-expt (/ n 2) (* b b) a))))
