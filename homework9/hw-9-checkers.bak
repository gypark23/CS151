#lang typed/racket

(require "../include/cs151-core.rkt")
(require "../include/cs151-universe.rkt")
(require "../include/cs151-image.rkt")
(require typed/test-engine/racket-tests)


;;;;; ==== Data definitions for checkers =====
(define-type Color (U 'red 'black))

(define-struct Loc
  ([row : Integer]
   [col : Integer]))

(define-struct Piece
  ([color : Color]
   [loc : Loc]))

(define-struct Checkers
  ([pieces : (Listof Piece)]
   [turn : Color]
   [clicked-piece : (U 'none Piece)]))

(: pieces-in-row : Integer Color Integer -> (Listof Piece))
;; Output a list of 4 pieces of the given color in the given row
;; This is a helper function to define starting-board
(define (pieces-in-row row c offset)
  (list
   (Piece c (Loc row (+ 1 offset)))
   (Piece c (Loc row (+ 3 offset)))
   (Piece c (Loc row (+ 5 offset)))
   (Piece c (Loc row (+ 7 offset)))))

(: starting-pieces : (Listof Piece))
(define starting-pieces
  (append
   (pieces-in-row 1 'red 1)
   (pieces-in-row 2 'red 0)
   (pieces-in-row 3 'red 1)
   (pieces-in-row 6 'black 0)
   (pieces-in-row 7 'black 1)
   (pieces-in-row 8 'black 0)))

(: starting-board : Checkers)
(define starting-board (Checkers starting-pieces 'red 'none))

;;; some general-purpose helper functions for the given datatypes
(: flip : Color -> Color)
(define (flip c)
  (match c
    ['red 'black]
    ['black 'red]))

(: loc=? : Loc Loc -> Boolean)
(define (loc=? l-one l-two)
  (and (= (Loc-row l-one) (Loc-row l-two))
       (= (Loc-col l-one) (Loc-col l-two))))

(: piece=? : Piece Piece -> Boolean)
(define (piece=? p-one p-two)
  (and
   (loc=? (Piece-loc p-one) (Piece-loc p-two))
   (symbol=? (Piece-color p-one) (Piece-color p-two))))

(: xy->loc : Integer Integer -> Loc)
;; convert (x, y) coordinates to a grid location
(define (xy->loc x y)
  (Loc (+ 1 (exact-floor (/ y 50))) (+ 1 (exact-floor (/ x 50)))))

;;;;; ==== Code for checkers logic ===
(: click-piece : Piece Checkers -> Checkers)
;; update the game state when the given piece is clicked
;; This requires adding the piece to the special clicked-piece field,
;; and also removing the piece from the list of pieces
(define (click-piece p game)
  (local
    {
     (: remove-p : (Listof Piece) -> (Listof Piece))
     ;; removes the first occurrence of p from the list
     (define (remove-p pieces)
       (cond
         [(empty? pieces) (error "click-piece: piece not found")]
         [else (if (piece=? (first pieces) p)
                   (rest pieces)
                   (cons (first pieces) (remove-p (rest pieces))))]))}
    (Checkers (remove-p (Checkers-pieces game)) (Checkers-turn game) p)))


(: place-piece : Loc Checkers -> Checkers)
;; place the clicked piece at the given location and update the game state
(define (place-piece loc game)
  (match (Checkers-clicked-piece game)
    [(Piece c _) (Checkers (cons (Piece c loc) (Checkers-pieces game))
                           (Checkers-turn game)
                           'none)]
    ['none (error "place-piece: no piece to place")]))

(: get-piece-or-none : Loc Checkers -> (U 'none Piece))
;; checks if there is a piece at the given location,
;; and if there is, that piece is returned
(define (get-piece-or-none loc game)
  (local
    {
     (: piece-from-list : (Listof Piece) -> (U Piece 'none))
     ;; return a Piece matching loc from the given list
     ;; or 'none if there aren't any
     (define (piece-from-list pieces)
       (cond
         [(empty? pieces) 'none]
         [else (if (loc=? loc (Piece-loc (first pieces)))
                   (first pieces)
                   (piece-from-list (rest pieces)))]))}
    (piece-from-list (Checkers-pieces game))))


(: click-board : Checkers Integer Integer Mouse-Event -> Checkers)
;; this function is called whenever the mouse does something
;; Currently, it adds a piece to the board at the clicked location
(define (click-board game x y event)
  (match event
    ["button-up" (match (Checkers-clicked-piece game)
                   [(Piece _ _) (place-piece (xy->loc x y) game)]
                   ['none (match (get-piece-or-none (xy->loc x y) game)
                            [(Piece c loc) (click-piece (Piece c loc) game)]
                            ['none game])])]
    [_ game]))


(: game-over? : Checkers -> Boolean)
(define (game-over? game) #f)

;;; ==== Code for interactivity ====
;; Code past this point is used for rendering the game board

;;;;; functions we need from Lab 1
(: draw-square : Integer Image-Color -> Image)
(define (draw-square size color)
  (overlay
   (square size "outline" "black")
   (square size "solid" color)))

(: alt-shaded-row : Integer Integer Image-Color Image-Color -> Image)
(define (alt-shaded-row n size color-one color-two)
  (cond
    [(= 0 n) empty-image]
    [else (beside
           (draw-square size color-one)
           (alt-shaded-row (- n 1) size color-two color-one))]))

(: alt-shaded-rows : Integer Integer Integer Image-Color Image-Color -> Image)
(define (alt-shaded-rows m n size color-one color-two)
  (cond
    [(= 0 m) empty-image]
    [else (above
           (alt-shaded-row n size color-one color-two)
           (alt-shaded-rows (- m 1) n size color-two color-one))]))

(: draw-board : Checkers -> Image)
;; draws the given game state
(define (draw-board game)
  (local
    {(: sq-len : Integer)
     (define sq-len 50)

     (: color-one : Image-Color)
     (define color-one "maroon")
     (: color-two : Image-Color)
     (define color-two "ivory")
     
     (: board : Image)
     (define board (alt-shaded-rows 8 8 sq-len color-one color-two))
     
     (: draw-piece : Color -> Image)
     (define (draw-piece color)
       (circle (round (/ (- sq-len 10) 2))
               "solid"
               (match color
                 ['red  "red"]
                 ['black "black"])))

     (: draw-pieces : (Listof Piece) Image -> Image)
     (define (draw-pieces pieces background)
       (match pieces
         ['() background]
         [(cons (Piece color (Loc r c)) rest)
          (overlay/xy (draw-piece color)
                      (- (* -1 sq-len (- c 1)) 5)
                      (- (* -1 sq-len (- r 1)) 5)
                      (draw-pieces rest background))]))

     (: draw-clicked-piece : Image -> Image)
     (define (draw-clicked-piece background)
       (match (Checkers-clicked-piece game)
         [(Piece color (Loc r c))
          (overlay/xy (overlay (draw-piece color)
                               (circle (- (/ sq-len 2) 3)
                                       "solid"
                                       (if (= (remainder (+ r c) 2) 0) color-one color-two))
                               (circle (- (/ sq-len 2) 1) "solid" "green"))
                      (- (* -1 sq-len (- c 1)) 1)
                      (- (* -1 sq-len (- r 1)) 1)
                      background)]
         ['none background]))}
    (draw-clicked-piece (draw-pieces (Checkers-pieces game) board))))

(: start-game : -> Checkers)
;; renders an interactive game board in the start state
;; the clauses inside [] below are the handlers for different interactions
;; To draw the game state, we call the function draw-board
;; On a mouse action, we call the function click-board
(define (start-game)
  (big-bang starting-board : Checkers
    [to-draw draw-board]
    [on-mouse click-board]
    [stop-when game-over?]))