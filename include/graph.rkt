#lang typed/racket

(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)

(define-type Vertex Integer)
(define-struct Graph
  ([n : Integer]
   [adj : (Vectorof (Listof Vertex))]))


(: get-nbrs : Graph Vertex -> (Listof Vertex))
(define (get-nbrs g v)
  (vector-ref (Graph-adj g) v))

(: in-list? : (Listof Vertex) Vertex -> Boolean)
(define (in-list? vtxs v)
  (cond
    [(empty? vtxs) #f]
    [else (or (= v (first vtxs)) (in-list? (rest vtxs) v))]))

(: adjacent? : Graph Vertex Vertex -> Boolean)
(define (adjacent? g u v)
  (in-list? (get-nbrs g u) v))

(: degree : Graph Vertex -> Integer)
(define (degree g v)
  (length (get-nbrs g v)))

(: num-edges : Graph -> Integer)
(define (num-edges g)
  (quotient (foldr + 0 (vector->list (vector-map (inst length Vertex) (Graph-adj g)))) 2))

(: all-vtxs : Graph -> (Listof Vertex))
(define (all-vtxs g)
  (build-list (Graph-n g) (lambda ([x : Integer]) x)))

(: simple? : Graph -> Boolean)
(define (simple? g)
  (local
    {
     (: no-self-loops? : Graph -> Boolean)
     (define (no-self-loops? g)
       (andmap (lambda ([v : Vertex]) (not (in-list? (get-nbrs g v) v))) (all-vtxs g)))

     ;; check whether a list of Integers is unique by sorting first
     (: sorted-unique? : (Listof Vertex) -> Boolean)
     (define (sorted-unique? vtxs)
       (cond
         [(empty? vtxs) #t]
         [(empty? (rest vtxs)) #t]
         [else (and (not (= (first vtxs) (second vtxs)))
                    (sorted-unique? (rest vtxs)))]))
     (: unique? : (Listof Vertex) -> Boolean)
     (define (unique? vtxs)
       (sorted-unique? (sort vtxs <)))
     
     (: no-multiedges? : Graph -> Boolean)
     (define (no-multiedges? g)
       (andmap unique? (vector->list (Graph-adj g))))}
    (and (no-self-loops? g)
         (no-multiedges? g))))

(: undirected? : Graph -> Boolean)
(define (undirected? g)
  (local
    {
     (: v-opps-exist? : Vertex -> Boolean)
     (define (v-opps-exist? v)
       (andmap (lambda ([u : Vertex]) (adjacent? g u v)) (get-nbrs g v)))} 
  (andmap v-opps-exist? (all-vtxs g))))
    

(: connected? : Graph -> Boolean)
(define (connected? g)
  (local
    {
     (: reachable : (Vectorof Boolean))
     (define reachable (make-vector (Graph-n g) #f))}
    (begin (dfs! g 0 reachable)
           (andmap (lambda ([b : Boolean]) b) (vector->list reachable)))))

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


















#|
(: bfs! : Graph Vertex (Vectorof Boolean) -> Void)
(define (bfs! g v visited)
   (local
     {
      (: explore! : (Listof Vertex) -> (Listof Vertex))
      (define (explore! to-do)
        (cond
          [(empty? to-do) empty]
          [else (if (not (vector-ref visited (first to-do
     |#

(: within-two : Graph Vertex -> Integer)
(define (within-two g v)
  (local
    {
     (: seen? : (Vectorof Boolean))
     (define seen? (make-vector (Graph-n g) #f))

     (: true? : Boolean -> Boolean)
     (define (true? bool) bool)
     
     (: set-all! : (Listof Vertex) -> Void)
     (define (set-all! far-nbrs)
       (cond
         [(empty? far-nbrs) (void)]
         [else (begin (vector-set! seen? (first far-nbrs) #t)
                      (set-all! (rest far-nbrs)))]))
     (: set-nbrs! : (Listof Vertex) -> Void)
     (define (set-nbrs! nbrs)
       (cond
         [(empty? nbrs) (void)]
         [else (begin (set-all! (vector-ref (Graph-adj g) (first nbrs)))
                      (set-nbrs! (rest nbrs)))]))}
    (begin (set-all! (vector-ref (Graph-adj g) v))
           (set-nbrs! (vector-ref (Graph-adj g) v))
           (vector-count true? seen?))))


(: count-ccs : Graph -> Integer)
(define (count-ccs g)
  (local
    {
     (: visited : (Vectorof Boolean))
     (define visited (make-vector (Graph-n g) #f))
     (: visit : Vertex Integer -> Integer)
     (define (visit v num-ccs)
       (if (not (vector-ref visited v))
           (begin (dfs! g v visited)
                  (+ num-ccs 1))
           num-ccs))}
    (foldr visit 0 (build-list (Graph-n g) (lambda ([x : Integer]) x)))))

(: acyclic? : Graph -> Boolean)
(define (acyclic? g) #f)



(: tri : Graph)
(define tri (Graph 3 (vector (list 1 2) (list 0 2) (list 0 1))))

(: tri-tri : Graph)
(define tri-tri (Graph 6 (vector (list 1 2) (list 0 2) (list 0 1) (list 4 5) (list 3 5) (list 3 4))))

