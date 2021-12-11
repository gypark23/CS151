#lang typed/racket

(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)

(define-struct Point
  ([x : Real]
   [y : Real]))

(: p : Point)
(define p (Point 3 2))

(: add-point : Point Point -> Point) 
(define (add-point point1 point2)
  (Point (+ (Point-x point1) (Point-x point2)) (+ (Point-y point1) (Point-y point2))))

(: check-greater-axis : Point Point -> Boolean )
;; Checks if the first point is above x-axis
;; and second point is below x-axis
( define (check-greater-axis point1 point2)
     (and ( > (Point-y point1) 0) (< (Point-y point2) 0)))



(check-expect (Point-y p) 2)


(check-expect (add-point (Point 1 2) (Point 3 4)) (Point 4 6))
(check-expect (check-greater-axis (Point 1 2) (Point 1 -2)) #t)
(test)