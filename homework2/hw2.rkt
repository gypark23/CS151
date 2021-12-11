#lang typed/racket
(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)
;; Homework 2
;;
;; Kyu Park
;; 12198215



;; Problem 2
;; converts an integer value of score to a symbol letter grade
;; the score must be a positive integer and 0
;; otherwise yields error
(: score->letter : Exact-Rational -> Symbol)
(define (score->letter score)
  (cond
    [(>= score 90) 'A]
    [(>= score 80) 'B]
    [(>= score 70) 'C]
    [(>= score 60) 'D]
    [(>= score 0) 'F]
    [else (error "score->letter: the score cannot be negative")]))


(check-expect (score->letter 93) 'A)
(check-expect (score->letter 80) 'B)
(check-expect (score->letter 57) 'F)
(check-error (score->letter -35) "score->letter: the score cannot be negative")



;; Problem 3
;; prints out a gramatically correct string sentence of how many animals there are
;; after taking positive integer values of the number of cats and dogs
;; prints out error if the integer values are negative
(: how-many-animals : Integer Integer -> String)
   (define (how-many-animals numcats numdogs)
     (cond
       [(or (< numcats 0) (< numdogs 0)) (error "how-many-animals: input cannot be negative")]
       [(and (= numcats 0) (= numdogs 0)) "there are no animals"]
       [(= numcats 0)
        (if (= numdogs 1)
            "there is 1 dog"
            (string-append "there are " (number->string numdogs) " dogs"))]
       [(= numdogs 0)
        (if (= numcats 1)
            "there is 1 cat"
            (string-append "there are " (number->string numcats) " cats"))]
       [else
        (if (= numcats 1)
            (if (= numdogs 1)
                "there is 1 cat and there is 1 dog"
                (string-append "there is 1 cat and there are " (number->string numdogs) " dogs"))
            (if (= numdogs 1)
                (string-append "there are " (number->string numcats) " cats and there is 1 dog")
                (string-append "there are " (number->string numcats) " cats and there are " (number->string numdogs) " dogs")))]))
            


(check-error (how-many-animals -1 0) "how-many-animals: input cannot be negative")
(check-expect (how-many-animals 0 0) "there are no animals")
(check-expect (how-many-animals 1 0) "there is 1 cat")
(check-expect (how-many-animals 1 1) "there is 1 cat and there is 1 dog")
(check-expect (how-many-animals 2 0) "there are 2 cats")
(check-expect (how-many-animals 2 1) "there are 2 cats and there is 1 dog")
(check-expect (how-many-animals 2 2) "there are 2 cats and there are 2 dogs")
(check-expect (how-many-animals 0 1) "there is 1 dog")
(check-expect (how-many-animals 0 2) "there are 2 dogs")
(check-expect (how-many-animals 1 2) "there is 1 cat and there are 2 dogs")




   
;; Problem 4
;; calculates the exponential x^y
;; after taking real number x and positive integer y
;; if y is negative, throws error
(: my-expt : Real Integer -> Real)
(define (my-expt x y)
  (cond
    [(< y 0) (error "my-exp: the exponents cannot be negative")]
    [else (expt x y)]))
        

(check-within (my-expt 4.1 2) 16.81 0.0001)
(check-within (my-expt -3.6784 3) -49.7711 0.0001)
(check-error (my-expt 2 -1) "my-exp: the exponents cannot be negative")

;; sums up the first n numbers raised to the power k
;; after taking integer values n and k
;; throws error if k is negative
(: sum-of-powers : Integer Integer -> Real)
(define (sum-of-powers n k)
  (cond 
  [(= n 1) my-expt n 1]
  [(+ (sum-of-powers (- n 1) k) (my-expt n k))]))


(check-expect (sum-of-powers 5 2) 55)
(check-expect (sum-of-powers 5 3) 225)
(check-error (sum-of-powers 5 -1) "my-exp: the exponents cannot be negative")



;; Problem 5
;; counts how many numbers are in a given list through recursive function
;; returns 0 when the list is empty
;; returns an integer value of the number of numbers in a given list
(: my-length : (Listof Number) -> Integer)
(define (my-length numlist)
  (cond
    [(empty? numlist) 0]
    [else (+ 1 (my-length (rest numlist)))]))


(check-expect (my-length (list 1 2 3 4 5)) 5)
(check-expect (my-length (list )) 0)



;; Problem 6
;; counts how many cards the second person has but not the first person
;; in two given same-lengthed lists of booleans
;; if the lists don't have identical length then prints out error
;; returns the integer value of how many cases
;; the first list has false when the second list has true
(: theirs-not-mine : (Listof Boolean) (Listof Boolean) -> Integer)
(define (theirs-not-mine mylist theirlist)
  (cond
    [(and (empty? mylist) (empty? theirlist)) 0]
    [(or (empty? mylist) (empty? theirlist)) (error "theirs-not-mine: the lengths of the lists are not identical")]
    [else (if (and (first theirlist) (not (first mylist)))
              (+ 1 (theirs-not-mine (rest mylist) (rest theirlist)))
              (theirs-not-mine (rest mylist) (rest theirlist)))]))


(check-expect (theirs-not-mine (list #f #f #f #f #f)(list #t #t #t #t #t)) 5)
(check-expect (theirs-not-mine (list #t #f #t #f #t)(list #f #f #t #t #t)) 1)
(check-expect (theirs-not-mine (list #t #f #f #f #t)(list #f #t #t #t #f)) 3)
(check-error (theirs-not-mine (list #t #f #f #f #t #t #f #t)(list #f #t #t #t #f)) "theirs-not-mine: the lengths of the lists are not identical")



