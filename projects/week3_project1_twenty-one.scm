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

(define (play-n strategy n)
    (if (= n 0)
        0
        (+ (twenty-one strategy) (play-n strategy (- n 1)))))

(play-n stop-at-17 3)

(define (dealer-sensitive customer-hand dealer-up-card)
    (let (
            (is-picture (picture? dealer-up-card)) 
            (dealer-card-rank (rank dealer-up-card)) 
            (best-total-customer (best-total customer-hand)))
        (or (and (< best-total-customer 17) (or is-picture (> dealer-card-rank 6)))
            (and (< best-total-customer 12) (member? dealer-card-rank '(2 3 4 5 6))))
    ))

(define (stop-at n)
    (lambda (customer-hand dealer-up-card)
        (< (best-total (se customer-hand dealer-up-card)) n)))

((stop-at 17) '(AD 4S) 'AS) ; should return true
((stop-at 17) '(AD 5S) 'AS) ; should return false

(define (suit card)
    (last card))

(define (has-suit hand suit-filter)
    (cond 
        ((empty? hand)
            #f)
        ((equal? (suit (first hand)) suit-filter)
            #t)
        (else (has-suit (bf hand) suit-filter))))

(has-suit '(AD 4S) 'H)
(has-suit '(AD 2H 4S) 'H)

(define (valentine customer-hand dealer-up-card)
    (if (has-suit customer-hand 'H)
        ((stop-at 19) customer-hand dealer-up-card)
        ((stop-at 17) customer-hand dealer-up-card)))

(define (suit-strategy s strategy-if-not-suit strategy-if-suit)
    (lambda (customer-hand dealer-up-card)
        (if (has-suit customer-hand s)
            (strategy-if-suit customer-hand dealer-up-card)
            (strategy-if-not-suit customer-hand dealer-up-card))))

(define alt-valentine (suit-strategy 'H (stop-at 17) (stop-at 19)))

(define (majority s1 s2 s3)
    (lambda (customer-hand dealer-up-card)
        (let (
            (s1-result (s1 customer-hand dealer-up-card))
            (s2-result (s2 customer-hand dealer-up-card))
            (s3-result (s3 customer-hand dealer-up-card)))
            (or (and s1-result s2-result) (and s2-result s3-result) (and s1-result s3-result)))))

(play-n (majority valentine stop-at-17 dealer-sensitive) 3)

(define (reckless strategy)
    (lambda (customer-hand dealer-up-card)
        (strategy (bl customer-hand) dealer-up-card)))

((reckless (stop-at 17)) '(AD 5S) 'AS) ; should return true
((reckless (stop-at 17)) '(AD 5S 1S) '1S) ; should return false