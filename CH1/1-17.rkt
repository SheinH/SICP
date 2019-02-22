#lang racket
(define (helper x y s)
  (cond ((= y 1) (+ s x))
        ((even? y) (helper (* x 2) (/ y 2) s))
        (else (helper x (- y 1) (+ s x)))))
(define (mult x y)
  (if (> y x)
      (helper y x 0)
      (helper x y 0)))