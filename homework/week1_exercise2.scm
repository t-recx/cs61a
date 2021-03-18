#lang simply-scheme

(define (square nums)
  (if (empty? nums)
      '()
       (se (* (first nums) (first nums)) (square (bf nums)))))