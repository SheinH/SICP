#lang sicp

(define (square x) (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (next x)
  (if (= x 2)
      3
      (+ x 2)))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n 3 (runtime)))

(define (start-prime-test n n2 start-time)
  (cond ((= n2 0) (report-prime (- (runtime) start-time)))
        ((prime? n) (start-prime-test (+ n 1) (- n2 1) start-time))
        (else (start-prime-test (+ n 1) n2 start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (test n)
  (if (< n 1000000000)
      (begin
       (timed-prime-test n)
       (test (* n 10)))))