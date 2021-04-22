#lang simply-scheme

; For our purposes, the rules of twenty-one (“blackjack”) are as follows. 
; There are two players: the “customer” and the “dealer”. 
; The object of the game is to be dealt a set of cards that comes as close to 21 as possible without going over 21 (“busting”).
; A card is represented as a word, such as 10s for the ten of spades. 
; (Ace, jack, queen, and king are a, j, q, and k.)
; Picture cards are worth 10 points; an ace is worth either 1 or 11 at the player’s option. 

(define (twenty-one strategy)
  (define (play-dealer customer-hand dealer-hand-so-far rest-of-deck)
    (cond ((> (best-total dealer-hand-so-far) 21) 1)
	  ((< (best-total dealer-hand-so-far) 17)
	   (play-dealer customer-hand
			(se dealer-hand-so-far (first rest-of-deck))
			(bf rest-of-deck)))
	  ((< (best-total customer-hand) (best-total dealer-hand-so-far)) -1)
	  ((= (best-total customer-hand) (best-total dealer-hand-so-far)) 0)
	  (else 1)))

  (define (play-customer customer-hand-so-far dealer-up-card rest-of-deck)
    (cond ((> (best-total customer-hand-so-far) 21) -1)
	  ((strategy customer-hand-so-far dealer-up-card)
	   (play-customer (se customer-hand-so-far (first rest-of-deck))
			  dealer-up-card
			  (bf rest-of-deck)))
	  (else
	   (play-dealer customer-hand-so-far
			(se dealer-up-card (first rest-of-deck))
			(bf rest-of-deck)))))

  (let ((deck (make-deck)))
    (play-customer (se (first deck) (first (bf deck)))
		   (first (bf (bf deck)))
		   (bf (bf (bf deck))))) )

(define (make-ordered-deck)
  (define (make-suit s)
    (every (lambda (rank) (word rank s)) '(A 2 3 4 5 6 7 8 9 10 J Q K)) )
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C)) )

(define (make-deck)
  (define (shuffle deck size)
    (define (move-card in out which)
      (if (= which 0)
	  (se (first in) (shuffle (se (bf in) out) (- size 1)))
	  (move-card (bf in) (se (first in) out) (- which 1)) ))
    (if (= size 0)
	deck
    	(move-card deck '() (random size)) ))
  (shuffle (make-ordered-deck) 52) )

(define (rank card)
    (bl card))

(define (picture? card)
    (member? (rank card) '(J K Q A)))

(define (ace? card)
    (equal? (rank card) 'A))

(define (all-aces? s)
    (define (all-aces-not-empty s)
        (cond 
            ((empty? s) #t)
            ((ace? (first s))
                (all-aces-not-empty (bf s)))
            (else #f)))

    (if (empty? s) 
        #f
        (all-aces-not-empty s)))

(define (best-total s)
    (define (best-total-iter s points)
        (cond ((empty? s) points)
            ((all-aces? s) 
                (if (<= (+ (+ points 11) (- (length s) 1)) 21)
                    (best-total-iter (bf s) (+ 11 points))
                    (best-total-iter (bf s) (+ 1 points))))
            ((picture? (first s)) 
                (if (ace? (first s))
                    (best-total-iter (se (bf s) (first s)) points)
                    (best-total-iter (bf s) (+ 10 points))))
            (else (best-total-iter (bf s) (+ points (rank (first s)))))))

    (best-total-iter s 0))

(best-total '(AD 8S))
(best-total '(AD 8S 2S))
(best-total '(AD 8S 5H))

(best-total '(AD 8S AC 2S))
(best-total '(AD AS 9H))
(best-total '(AD AS AH AC))

(define (stop-at-17 customer-hand dealer-up-card)
    (< (best-total (se customer-hand dealer-up-card)) 17))

(stop-at-17 '(AD 4S) 'AS) ; should return true
(stop-at-17 '(AD 5S) 'AS) ; should return false