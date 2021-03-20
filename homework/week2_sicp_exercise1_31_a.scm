#lang simply-scheme

(define (identity n) n)

(define (inc n) (+ n 1))

(define (inc2 n) (+ n 2))

(define (squared n) (* n n))

(define (squared-except exceptions)
    (lambda(n) 
        (if (member? n exceptions)
            (identity n)
            (squared n))))

(define (product term a next b)
    (if (> a b)
        1
        (* (term a)
            (product term (next a) next b))))

(define (product-integers a b)
    (product identity a inc b))

(define (factorial n)
    (product identity 1 inc n))

(define (pi-approx)
    (define n 8)
    (* 4
        (/ 
            (product (squared-except (list 2 n)) 2 inc2 n)
            (* 1.0 (product squared 1 inc2 (- n 1))))))

(product-integers 1 5)

(factorial 7)

(pi-approx)