#lang simply-scheme

(define (inc n) (+ n 1))

(define (identity n) n)

(define (square n) (* n n))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (if (member? n '(2 3))
    #t
    (if (or (< n 2) (equal? (remainder n 2) 0) (equal? (remainder n 3) 0))
      #f
      (= n (smallest-divisor n)))))

(define (filtered-accumulate filter combiner null-value term a next b)
    (if (> a b)
        null-value
        (if (filter a)
          (combiner (term a) (filtered-accumulate filter combiner null-value term (next a) next b))
          (combiner 0 (filtered-accumulate filter combiner null-value term (next a) next b)))))

(define (sum-squares-prime-numbers a b)
    (filtered-accumulate prime? + 0 square a inc b))

(define (gcd i n)
  (if (equal? n 0)
    i
    (gcd n (remainder i n))))

(define (relative-prime n)
  (lambda(i)
    (equal? (gcd i n) 1)))

(define (product-of-all-positive-integers n)
  (filtered-accumulate (relative-prime n) * 1 identity 1 inc (- n 1)))

(sum-squares-prime-numbers 1 10)

(product-of-all-positive-integers 13)