#lang simply-scheme

(define (inc n) (+ n 1))

(define (identity n) n)

(define (accumulate combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (product term a next b)
    (accumulate * 1 term a inc b))

(define (sum term a next b)
    (accumulate + 0 term a inc b))

(define (product-integers a b)
    (product identity a inc b))

(define (sum-integers a b)
    (sum identity a inc b))

(product-integers 1 5)

(sum-integers 1 5)