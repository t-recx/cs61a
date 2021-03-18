#lang simply-scheme

(define (ordered? nums)
  (if (empty? nums)
      #t
      (let ((restofnums (bf nums)))
        (if (empty? restofnums)
            #t
            (if (> (first nums) (first restofnums))
                #f
                (ordered? restofnums))))))