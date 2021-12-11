#lang typed/racket
(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)
(require "../include/cs151-image.rkt")

;; Homework 4
;;
;; Kyu Park
;; 12198215



;; Problem 1

;; description should be String since it will contain multiple words describing what the activity is
(define-type Description String)

;; Day-of-Week can be Symbol since all of them  can be described in a single word and there are only 7 possible Day-of-Week
(define-type Day-of-Week (U 'monday 'tuesday 'wednesday 'thursday 'friday 'saturday 'sunday))

;; Location should be String since location might need multiple words
(define-type Location String)

;; Activity is a structure since it contains description, day-of-week and location
(define-struct Activity
  ([description : Description]
   [day-of-week : Day-of-Week]
   [location : Location]))

;; Calendar is a list of activity, so it must be a type of activity list
(define-type Calendar (Listof Activity))


(: firstlecture : Activity)
(define firstlecture (Activity "regular lectures" 'monday "Zoom")) 

(: secondlecture : Activity)
(define secondlecture (Activity "regular lectures" 'wednesday "Zoom")) 

(: thirdlecture : Activity)
(define thirdlecture (Activity "regular lectures" 'friday "Zoom"))

(: lab : Activity)
(define lab (Activity "lab session" 'wednesday "Zoom")) 




(: weekend? : Activity -> Boolean)
;; checks if the activity occurs on weekends
;; returns true if the activity's day-of-week is either saturday or sunday
;; returns false if the activity's day-of-week is not saturday nor sunday
(define (weekend? act)
  (match act
    [(Activity _ 'saturday _) #t]
    [(Activity _ 'sunday _) #t]
    [_ #f]))


(check-expect (weekend? firstlecture) #f)
(check-expect (weekend? (Activity "just checkin" 'sunday "shud be true")) #t)



;; Problem 2
(define-struct Pine-Tree
  ([seed : (U 'nothing Pine-Cone)]
   [height : Real]))

(define-struct Pine-Cone
  ([grows-into : (U 'nothing Pine-Tree)]
   [number-of-bristles : Integer]))


(: seed-bristles : Pine-Tree -> Integer)
;; checks how many seed-bristles a given pine tree has
;; returns an integer value of the number of seed bristles
;; returns 0 if there is no seed
(define (seed-bristles tree)
  (match (Pine-Tree-seed tree)
    ['nothing 0]
    [(Pine-Cone _ _) (Pine-Cone-number-of-bristles (Pine-Tree-seed tree))]))


(check-expect (seed-bristles (Pine-Tree (Pine-Cone 'nothing 30) 120)) 30)
(check-expect (seed-bristles (Pine-Tree (Pine-Cone 'nothing 50) 160)) 50)
(check-expect (seed-bristles (Pine-Tree 'nothing 120)) 0)


(: germinate : Pine-Tree -> Pine-Tree)
;; germinates the given pine tree
;; takes the pine tree and returns the new pine tree with new seed
;; if the pine tree already had a pine cone returns the pine tree itself unmodified
(define (germinate tree)
  (match (Pine-Tree-seed tree)
    [(Pine-Cone _ _) tree]
    ['nothing (Pine-Tree (Pine-Cone 'nothing 40) (Pine-Tree-height tree))]))

(check-expect (germinate (Pine-Tree 'nothing 30)) (Pine-Tree (Pine-Cone 'nothing 40) 30))
(check-expect (germinate (Pine-Tree (Pine-Cone 'nothing 30) 30)) (Pine-Tree (Pine-Cone 'nothing 30) 30))


(: max-tree-descendant-height : Pine-Tree -> Real)
;; calculates the max height of the descendants of the given pine tree
;; returns the real value of max height of descendants of the pine tree
(define (max-tree-descendant-height tree)
  (match (Pine-Tree-seed tree)
    ['nothing (Pine-Tree-height tree)]
    [(Pine-Cone _ _) (max (Pine-Tree-height tree) (max-cone-descendant-height (Pine-Tree-seed tree)))]))

(: max-cone-descendant-height : Pine-Cone -> Real)
;; calculates the max height of the descendants of the given pine cone
;; returns the real value of max height of descendants of the pine cone
(define (max-cone-descendant-height cone)
  (match (Pine-Cone-grows-into cone)
    ['nothing 0]
    [(Pine-Tree _ _) (max-tree-descendant-height (Pine-Cone-grows-into cone))]))

(check-expect (max-tree-descendant-height (Pine-Tree (Pine-Cone (Pine-Tree 'nothing 20) 50) 160)) 160)
(check-expect (max-tree-descendant-height (Pine-Tree (Pine-Cone (Pine-Tree 'nothing 1000) 50) 160)) 1000)



;; Problem 3

(define-struct Succ
  ([nat :  Nat]))
(define-type Nat (U 'zero Succ))

(: my-gte : Nat Nat -> Boolean)
;; checks if the first given Nat is greater or equal to the second given Nat
;; returns true if the first given Nat is greater or equal
;; returns false otherwise
(define (my-gte first second)
  (match second
    ['zero #t]
    [(Succ m) (match first
                ['zero #f]
                [(Succ n) (my-gte n m)])]))


(check-expect (my-gte (Succ (Succ (Succ (Succ 'zero)))) (Succ (Succ (Succ 'zero)))) #t)
(check-expect (my-gte (Succ (Succ (Succ (Succ 'zero)))) 'zero) #t)
(check-expect (my-gte 'zero (Succ (Succ (Succ 'zero)))) #f)
(check-expect (my-gte (Succ (Succ (Succ (Succ 'zero)))) (Succ (Succ (Succ (Succ 'zero))))) #t)
(check-expect (my-gte (Succ (Succ (Succ (Succ 'zero)))) (Succ (Succ (Succ (Succ (Succ (Succ (Succ 'zero)))))))) #f)
(check-expect (my-gte 'zero 'zero) #t)



(: my-odd? : Nat -> Boolean)
;; checks if the given Nat is odd number
;; considers zero as even number
;; returns true if is odd number
;; returns false if it is even number
(define (my-odd? num)
  (match num
    ['zero #f]
    [(Succ 'zero) #t]
    [(Succ (Succ m)) (my-odd? m)]))


(check-expect (my-odd? 'zero) #f)
(check-expect (my-odd? (Succ 'zero)) #t)
(check-expect (my-odd? (Succ (Succ 'zero))) #f)
(check-expect (my-odd? (Succ (Succ (Succ (Succ (Succ (Succ (Succ 'zero)))))))) #t)
(check-expect (my-odd? (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ 'zero))))))))) #f)



(:  pred :  Nat -> Nat)
;; returns a precedent nat
(define (pred nat)
  (match nat
    ['zero (error "pred:  input is zero")]
    [(Succ m) m]))

(: sub : Nat Nat -> Nat)
;; subtracts two nats
(define (sub nat-left nat-right)
  (match nat-right
    ['zero nat-left]
    [(Succ m) (sub (pred nat-left) m)]))


(: my-quotient : Nat Nat -> Nat)
;; calculates the quotient from given two Nat
;; error if the divisor is zero
;; returns quotient nat and drops remainder
(define (my-quotient dividend divisor)
  (cond
    [(symbol? divisor) (error "divisor cannot be zero")]
    [(my-gte dividend divisor) (Succ (my-quotient (sub dividend divisor) divisor))]
    [else 'zero]))


(check-expect (my-quotient (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ 'zero)))))))) (Succ (Succ (Succ 'zero)))) (Succ (Succ 'zero)))
(check-expect (my-quotient (Succ (Succ (Succ (Succ (Succ (Succ 'zero)))))) (Succ (Succ (Succ 'zero))))(Succ (Succ 'zero)))
(check-expect (my-quotient (Succ 'zero) (Succ 'zero)) (Succ 'zero))
