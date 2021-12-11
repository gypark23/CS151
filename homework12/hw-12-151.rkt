#lang racket/base
(require "../Desktop/CS-151-HW/include/cs151-core.rkt")
(require typed/test-engine/racket-tests)
;; Homework 11
;;
;; Savannah Pinedo
;; savpinedo

(: dfs! : Graph Vertex (Vectorof Boolean) -> Void)
;; initially, reachable should be all false
(define (dfs! g v visited)
  (begin
    (vector-set! visited v #t)
    (local
      {
       (: explore-list! : (Listof Vertex) -> Void)
       (define (explore-list! nbrs)
         (cond
           [(empty? nbrs) (void)]
           [else (begin (if (vector-ref visited (first nbrs))
                            (void)
                            (dfs! g (first nbrs) visited))
                        (explore-list! (rest nbrs)) )]))}
      (explore-list! (get-nbrs g v)))))

(: richest-connection : Graph (Vector Exact-Rational) Vertex -> Vertex)
(define (richest-connection g money v)
  (local
    {
     (: n : Integer)
     (define n (Graph-n g))
     (: visited : (Vectorof Boolean))
     (define visited (make-vector n #f))
     (: find-max-helper : Integer Integer -> Integer)
     (define (find-max-helper cur-max i)
       (cond
         [(= i n) cur-max]
         [else (find-max-helper (+ i 1))]))}
    (begin
      (dfs! g v visited)
      (find-max-helper v 0))))
       