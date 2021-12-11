#lang typed/racket
(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)
(require "../include/cs151-image.rkt")

;; Homework 5
;;
;; Kyu Park
;; 12198215

;; Problem 2

(: third-moment : (Listof Real) -> Real)
(define (third-moment numbers)
  (foldr (lambda ([num : Real]) (expt num 3))