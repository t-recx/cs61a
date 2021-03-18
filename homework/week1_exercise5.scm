#lang simply-scheme

(define (ends-e s)
  (if (empty? s)
      '()
      (let ( (firstword (first s)) (restofwords (bf s)) )
            (if (equal? (last firstword) 'e)
                (se firstword (ends-e restofwords))
                (se (ends-e restofwords)))) ))