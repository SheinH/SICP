#lang sicp

(define (double x)
  (* x 2))
(define (halve x)
  (/ x 2))
(define (mult x y)
  (cond ((= y 0) 0)
        ((= y 1) x)
        ((odd? y) (+ x (mult x (- y 1))))
        (else (mult (* x 2) (/ y 2)))))