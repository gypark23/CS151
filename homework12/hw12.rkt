#lang typed/racket
(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)
(require "../include/cs151-image.rkt")
(require "../include/BST.rkt")

;; Homework 12
;;
;; Kyu Park
;; 12198215

(define-type Vertex Integer)

(define-struct Graph
  ([n : Integer]
   [adj : (Vectorof (Listof Vertex))]))

(: get-nbrs : Graph Vertex -> (Listof Vertex))
(define (get-nbrs g v)
  (vector-ref (Graph-adj g) v))

(: dfs! : Graph Vertex (Vectorof Boolean) -> Void)
(define (dfs! g v visited)
  (begin
    (vector-set! visited v #t)
    (local
      {
       (: explore-list! : (Listof Vertex) -> Void)
       (define (explore-list! nbrs)
         (cond
           [(empty? nbrs) (void)]
           [else (begin
                   (if (vector-ref visited (first nbrs))
                       (void)
                       (dfs! g (first nbrs) visited))
                   (explore-list! (rest nbrs)))]))}
      (explore-list! (get-nbrs g v)))))

;; Problem 1

(: richest-connection : Graph (Vectorof Exact-Rational) Vertex -> Vertex)
;; finds, given a person (vertex) of the graph, which reachable person (vertex) in the graph has the most money
;; returns a vertex of person who has the most money who is also reachable from the given vertex
(define (richest-connection g money v)
  (local
    {(: n : Integer)
     (define n (Graph-n g))
     (: visited : (Vectorof Boolean))
     (define visited (make-vector n #f))
     (: find-max-helper : Integer Integer -> Integer)
     (define (find-max-helper curr-max i)
       (cond
         [(= i n) curr-max]
         [else (find-max-helper (if (> (vector-ref money i) (vector-ref money curr-max)) i curr-max) (+ 1 i))]))}
     (begin (dfs! g v visited) (find-max-helper v 0))))
    
         
(: g : Graph)
(define g (Graph 5 (vector (list 1 2 3) (list 0) (list 0 4) (list 0 4) (list 2 3))))
;; 0 : George 1 : Max 2 : Sam 3: Dog 4 : Mitch Everybody is reachable to each other, so Dog should be the output all the time.
(: money : (Vectorof Exact-Rational))
(define money (vector 1002 2060373/100 36949 1020608026/100 467778))

(check-expect (richest-connection g money 0) 3)
(check-expect (richest-connection g money 1) 3)
(check-expect (richest-connection g money 2) 3)
(check-expect (richest-connection g money 4) 3)




;; Problem 2


(: my-andmap : (All (A) (A -> Boolean) (Listof A) -> Boolean))
;; applies the function to the given list
;; and computes "and" function to all boolean results of the function to the given list
(define (my-andmap f lists)
  (local
    {(: my-and : (Listof Boolean) -> Boolean)
     (define (my-and bools)
       (cond
         [(empty? (rest bools)) (first bools)]
         [else (and (first bools) (my-and (rest bools)))]))}
  (my-and (map f lists)))) 
    
(check-expect (my-andmap positive? (list 1 2 3 4 5)) true)
(check-expect (my-andmap negative? (list 1 -2 3 -4 5)) false)



;; Problem 3



(: count-tiles : Integer -> Integer)
;; counts how many ways are there to cover a floor ofsize 2×n with tiles of size 2×2 and 1×2.
;; n must be a nonnegative integer
(define (count-tiles n)
  (match n
    [0 0]
    [1 1]
    [2 3]
    [else (+ (count-tiles (- n 1)) (* 2 (count-tiles (- n 2))))]))



(check-expect (count-tiles 1) 1)
(check-expect (count-tiles 2) 3)
(check-expect (count-tiles 3) 5)
(check-expect (count-tiles 4) 11)


(test)