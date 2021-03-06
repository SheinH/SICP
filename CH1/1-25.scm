#lang sicp

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (square x) (* x x))

(define (expmod base exp m)
   (cond ((= exp 0) 1)
         ((even? exp)
          (remainder (square (expmod base (/ exp 2) m))
                     m))
         (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
(else false)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n 3 (runtime)))

(define (start-prime-test n n2 start-time)
  (cond ((= n2 0) (report-prime (- (runtime) start-time)))
        ((fast-prime? n 10) (start-prime-test (+ n 1) (- n2 1) start-time))
        (else (start-prime-test (+ n 1) n2 start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (test n)
  (if (< n 1000000000)
      (begin
       (timed-prime-test n)
       (test (* n 10)))))

(test 10)