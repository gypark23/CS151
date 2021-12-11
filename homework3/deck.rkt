#lang typed/racket

(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)

;; ------- type definitions ------------
;;; Cards come in one of four suits
(define-type Suit (U 'diamond 'heart 'club 'spade))

;;; Cards have both a suit and a rank
;;; The rank is either a number 2-10, or J, Q, K, A
;;; We represent A=1, and J,Q,K = 11, 12, 13
(define-struct Card
  ([suit : Suit]
   [rank : Integer]))

(define-type Deck (Listof Card))

;; -------- function definitions ---------

(: red? : Card -> Boolean)
(define (red? card)
  (or
   (symbol=? (Card-suit card) 'heart)
   (symbol=? (Card-suit card) 'diamond)))


(: black? : Card -> Boolean)
(define (black? card)
  (or
   (symbol=? (Card-suit card) 'club)
   (symbol=? (Card-suit card) 'spade)))

;;(define (black? card)
  ;;(not (red? card)))

;;(: face? : Card -> Boolean)

(: card=? : Card Card -> Boolean)
(define (card=? card-1 card-2)
  (and
   (symbol=? (Card-suit card-1) (Card-suit card-2)) ;;check if suits are equal
   (= (Card-rank card-1) (Card-rank card-2)))) ;; check if rank is equal

;;(: card->string : Card -> String)


(: cards-in-suit-helper : Suit Integer -> Deck)
;; outputs all Cards of the given Suit up to the given rank
(define (cards-in-suit-helper suit rank)
  (cond
    [(= rank 1) (list (Card suit 1))]
    [else (cons (Card suit rank) (cards-in-suit-helper suit (- rank 1)))]))


(: cards-in-suit : Suit -> Deck)
;; outputs the Cards of all ranks in a given Suit
(define (cards-in-suit suit)
  (cards-in-suit-helper suit 13))

(: complete-deck : Deck)
(define complete-deck
  (append (cards-in-suit 'club)
          (cards-in-suit 'heart)
          (cards-in-suit 'spade)
          (cards-in-suit 'diamond)))


;;(: select : Deck Integer -> Card)

;;(: complete? : Deck -> Boolean)

;;(: shuffle : Deck -> Deck)

;;(: deck=? : Deck Deck -> Boolean)

;;(: deck->string : Deck -> String)