#lang typed/racket
(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)
(require "../include/cs151-image.rkt")

;; Homework 3
;;
;; Kyu Park
;; 12198215



;; Problem 1

(define-struct Point
  ([x : Real]
   [y : Real]))

(: ryerson : Point)
(define ryerson (Point 0.25 0.4))

(: cathey : Point)
(define cathey (Point -0.5 -2.8))

(: regenstein : Point)
(define regenstein (Point -0.3 1.6))

(: crerar : Point)
(define crerar (Point -1.5 0.5))

(: ratner : Point)
(define ratner (Point -1 3))

(: in-quad? : Point -> Boolean)
;; checks if the given point is within the quad
;; checks if x and y cordinates are within +- 0.75 and +- 1
;; returns true if all the conditions are met
;; returns false if any of the coordinates is outside of the boundary
(define (in-quad? mypoint)
  (and (< (Point-x mypoint) 0.75) (< (Point-y mypoint) 1) (> (Point-x mypoint) -0.75) (> (Point-y mypoint) -1)))


(: dist : Point Point -> Real)
;; calculates the distance between two points
;; returns real value of the distance
(define (dist firstpoint secondpoint)
  (sqrt (+ (expt (- (Point-x firstpoint) (Point-x secondpoint)) 2) (expt (- (Point-y firstpoint) (Point-y secondpoint)) 2))))


(: route-dist : (Listof Point) -> Real)
;; calculates the total distance travelled
;; by summing up distances between points
;; after taking a list of points
(define (route-dist route)
  (cond
    [(empty? (rest route)) 0]
    [else (+ (dist (first route) (second route)) (route-dist (rest route)))])) 


;; Problem 2

(define-type Color (U 'gray 'red 'white 'blue))
(define-type Size (U 'small 'medium 'large))

(define-struct Shirt
  ([short-sleeves? : Boolean]
   [color : Color]
   [size : Size]))
(define-struct Pants
  ([shorts? : Boolean]
   [color : Color]
   [size : Size]))

(define-type Clothing (U Shirt Pants))



(: clothing-color : Clothing -> Color)
;; returns the color of the clothing
;; after determining whether it is shirt or pants
(define (clothing-color cloth)
  (cond
    [(Shirt? cloth) (Shirt-color cloth)]
    [(Pants? cloth) (Pants-color cloth)]))

(: same-size? : Clothing Clothing -> Boolean)
;; checks if the two given clothes are same sized
;; checks the symbol values of the sizes
;; returns true if they are same sized
;; returns false if they are not same sized
(define (same-size? firstclothing secondclothing)
  (cond
    [(Shirt? firstclothing)
     (cond
       [(Shirt? secondclothing) (symbol=? (Shirt-size firstclothing) (Shirt-size secondclothing))]
       [else (symbol=? (Shirt-size firstclothing) (Pants-size secondclothing))])]
    [else
     (cond
       [(Shirt? secondclothing) (symbol=? (Pants-size firstclothing) (Shirt-size secondclothing))]
       [else (symbol=? (Pants-size firstclothing) (Pants-size secondclothing))])]))
                               
(check-expect (same-size? (Shirt true 'red 'small) (Pants true 'white 'small)) #t)
(check-expect (same-size? (Shirt true 'red 'small) (Shirt false 'white 'medium)) #f)
(check-expect (same-size? (Pants true 'red 'small) (Pants true 'white 'small)) #t)


(: cool? : Clothing -> Boolean)
;; checks if the given clothes is short-sleeved or shorts 
;; checks the boolean value of the clothing
;; returns true if it is short-sleeved or shorts
;; returns false if it isn't short-sleeved or shorts
(define (cool? myclothing)
  (cond
    [(Shirt? myclothing) (Shirt-short-sleeves? myclothing)]
    [else (Pants-shorts? myclothing)]))
    
                               
(check-expect (cool? (Shirt true 'red 'small)) #t)
(check-expect (cool? (Shirt false 'red 'small)) #f)
(check-expect (cool? (Pants true 'red 'small)) #t)

