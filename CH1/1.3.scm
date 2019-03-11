#lang sicp

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))


(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b)
     dx))



(define (simpsons f a b n)
  (define h (/ (- b a) (exact->inexact n)))
  (define (const c)
    (cond ((or (= c 0) (= c n)) 1)
          ((odd? c ) 4)
          (else 2)))
  (define (term c)
    (* (const c) (f (* c h))))
  (define (sum c curr)
    (if (= c n)
        curr
        (sum (+ c 1) (+ curr (term c)))))
  (* (/ h 3) (sum 0 0)))