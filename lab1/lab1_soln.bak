#lang typed/racket
(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require typed/test-engine/racket-tests)

(: num-text : Integer -> Image)
;; produce an image of the given number
(define (num-text n)
  (text (number->string n) 14 "black"))

(: row-of-squares (Integer Integer -> Image))
(define (row-of-squares num len)
  (cond
    [(<= num 0) empty-image]
    [else (beside (square len "outline" "black") (row-of-squares (- num 1) len))]))


(: alt-shaded-row : Integer Integer Image-Color Image-Color -> Image)
(define (alt-shaded-row num size color1 color2)
  (cond
    [(<= num 0) empty-image]
    [else (beside (square size "solid" color1) (alt-shaded-row (- num 1) size color2 color1))]))

(: alt-shaded-rows : Integer Integer Integer  Image-Color Image-Color -> Image)
(define (alt-shaded-rows height width size color1 color2)
  (cond
    [(<= height 0) empty-image]
    {else (above (alt-shaded-row width size color1 color2) (alt-shaded-rows (- height 1) width size color2 color1))}))

(: num-alt-shaded-row : Integer Integer Integer Image-Color Image-Color -> Image)
(define (num-alt-shaded-row start num size color1 color2)
  (cond
    [(<= num 0) empty-image]
    [else (beside (overlay  (num-text start) (square size "solid" color1) ) (num-alt-shaded-row (+ start 1) (- num 1) size color2 color1))]))

(: num-alt-shaded-rows : Integer Integer Integer Integer Image-Color Image-Color -> Image)
(define (num-alt-shaded-rows start height width size color1 color2)
  (cond
    [(<= height 0) empty-image]
    [else (above (num-alt-shaded-row start width size color1 color2) (num-alt-shaded-rows (+ start width) (- height 1) width size color2 color1))]))
