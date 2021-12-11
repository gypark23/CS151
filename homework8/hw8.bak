#lang typed/racket
(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)
(require "../include/cs151-image.rkt")

;; Homework 6
;;
;; Kyu Park
;; 12198215



;; Problem 1

(define-type TV (Integer -> String))

(: friday-night : TV)
(define (friday-night num)
  (cond
    [(= num 5) "NBC Evening News"]
    [(= num 7) "Game of Throne"]
    [(= num 33) "ESPN SportsCenter"]
    [else (error "No channel")]))


(: change-program : TV Integer String -> TV)
;; changes program in a given tv
;; takes three inputs TV, integer channel number and string program name
;; returns a new tv with channel updated to program
(define (change-program tv chan program)
  (local
    {(: new-tv : TV)
     (define (new-tv new-chan)
       (if (= new-chan chan)
           program
           (tv new-chan)))}
    new-tv))


(check-expect ((change-program friday-night 5 "New 5 Channel") 5) "New 5 Channel")


;; Problem 2

(: duplicate-string : String Integer -> String)
;; takes a string and an integer number
;; returns a new string that has the given string repeated for the given amount
(define (duplicate-string word num)
  (local
    {(: num-to-words : Integer -> String)
     (define (num-to-words n)
       word)}
     (foldr string-append "" (build-list num num-to-words))))


(check-expect (duplicate-string "hi" 3) "hihihi")


;; Problem 3

(: number : Integer -> Integer)
;; returns an integer that is given
(define (number n) n)

(check-expect (number 5) 5)

(: list-0-to-n : Integer -> (Listof Integer))
;; builds a list from 0 to a given positive integer n
(define (list-0-to-n n)
  (build-list (+ 1 n) number)) 

(check-expect (list-0-to-n 5) (list 0 1 2 3 4 5))


(: my-sqrt : Integer -> Integer)
;; computes the largest integer less or equal than square root of given integer
;; the input integer must be greater or equal than 0
(define (my-sqrt n)
  (local
    {(: lte-sqrt : Integer -> Boolean)
     (define (lte-sqrt num)
       (<= (* num num) n))}
    (foldr max 0 (filter lte-sqrt (list-0-to-n n))))) 

(check-expect (my-sqrt 0) 0)
(check-expect (my-sqrt 1) 1)
(check-expect (my-sqrt 2) 1)
(check-expect (my-sqrt 3) 1)
(check-expect (my-sqrt 4) 2)

(: my-expt : Real Integer -> Real)
;; computes x^y for nonnegative y
(define (my-expt x y)
  (cond
    [(< y 0) (error "my-exp: the exponents cannot be negative")]
    [else (expt x y)]))

(: my-root : Integer Integer -> Integer)
;; computes the largest integer less or equal to k-th root of given integer n
;; the input n and k must be greater or equal to 0
(define (my-root n k)
  (local
    {(: lte-root : Integer -> Boolean)
     (define (lte-root num)
       (<= (my-expt num k) n))}
    (foldr max 0 (filter lte-root (list-0-to-n n)))))

(check-expect (my-root 64 3) 4)
(check-expect (my-root 121 3) 4)
  


;; Problem 4

(define-struct (Tree A)
  ([value : A]
   [left-child : (U 'none (Tree A))]
   [right-child : (U 'none (Tree A))]))

(: valid-bracket? : (Tree String) -> Boolean)
;; checks if the given tree bracket is valid
;; returns true if the tree bracket is valid with all left and right children valid as well
;; returns false otherwise
(define (valid-bracket? node)
  (local
    {(: left : (U (Tree String) 'none))
     (define left (Tree-left-child node))
     (: right : (U (Tree String) 'none))
     (define right (Tree-right-child node))}
    (cond
      [(and (symbol? left)(symbol? right)) #t]
      [(or (not (Tree? left)) (not (Tree? right))) #f]
      [(or (string=? (Tree-value node) (Tree-value right))
           (string=? (Tree-value node) (Tree-value left)))
       (and (valid-bracket? left) (valid-bracket? right))]
      [else #f])))




(: correct-tree : (Tree String))
(define correct-tree (Tree "Brazil" (Tree "Brazil" 'none 'none) (Tree "Argentina" 'none 'none)))

(: incorrect-tree : (Tree String))
(define incorrect-tree (Tree "Brazil" (Tree "Spain" 'none 'none) (Tree "Argentina" 'none 'none)))

(check-expect (valid-bracket? correct-tree) #t)
(check-expect (valid-bracket? incorrect-tree) #f)
