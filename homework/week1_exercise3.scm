#lang simply-scheme

(define (switch st)
  (define (innerswitch s)
    (if (empty? s)
      '()
      (let ((firstword (first s)))
        (se (cond
              ((equal? firstword 'you) 'me)
              ((equal? firstword 'I) 'you)
              ((equal? firstword 'me) 'you)
              (else firstword)) (innerswitch (bf s))))))
  
  (if (empty? st)
      '()
      (let ((firstword (first st)))
        (se (if (equal? firstword 'You)
              'I
              firstword) (innerswitch (bf st))))))
