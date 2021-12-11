#lang typed/racket

(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)

(provide BST
         BST-bst
         BST-lte?
         BST?
         Node
         Node-value
         Node-left-child
         Node-right-child
         Node?
         BST-valid?
         BST-search
         BST-insert
         BST-delete
         BST->list)

;; Type definition for a binary search tree
(define-struct (BST A)
  ([lte? : (A A -> Boolean)]
   [bst : (U 'none (Node A))]))

(define-struct (Node A)
  ([value : A]
   [left-child : (U 'none (Node A))]
   [right-child : (U 'none (Node A))]))

(: BST-min-val : (All (A) (BST A) -> A))
;; returns the smallest value in the tree
(define (BST-min-val t)
  (match t
    [(BST _ 'none) (error "BST-min-val: empty tree")]
    [(BST lte? (Node value lc rc))
     (cond
       [(and (Node? lc) (Node? rc))
        (local
          {
           (: left-min : A)
           (define left-min (BST-min-val (BST lte? lc)))
           (: right-min : A)
           (define right-min (BST-min-val (BST lte? rc)))
           (: child-min : A)
           (define child-min
             (if (lte? left-min right-min)
                 left-min
                 right-min))}
          (if (lte? child-min value)
              child-min
              value))]
       [(Node? lc)
        (if (lte? (BST-min-val (BST lte? lc)) value)
            (BST-min-val (BST lte? lc))
            value)]
       [(Node? rc)
        (if (lte? (BST-min-val (BST lte? rc)) value)
            (BST-min-val (BST lte? rc))
            value)]
       [else value])]))


(: BST-max-val : (All (A) (BST A) -> A))
;; returns the largest value in the tree
(define (BST-max-val t)
  (BST-min-val (BST (lambda ([v1 : A] [v2 : A]) ((BST-lte? t) v2 v1))
                    (BST-bst t))))


(: BST-valid? : (All (A) (BST A) -> Boolean))
;; checks if the given tree satisfies the BST sortedness conditions:
;; the nodes in the left subtree must have smaller values,
;; and the nodes in the right subtree must have larger values.
(define (BST-valid? t)
  (local
    {
     (: valid-BST-in-interval? : (BST A) A A -> Boolean)
     (define (valid-BST-in-interval? sub-t lo hi)
       (match t
         [(BST _ 'none) #t]
         [(BST lte? (Node my-val lc rc))
          (and (lte? lo my-val)
               (lte? my-val hi)
               (valid-BST-in-interval? (BST lte? lc) lo my-val)
               (valid-BST-in-interval? (BST lte? rc) my-val hi))]))}
    (or (not (Node? (BST-bst t)))
        (valid-BST-in-interval? t (BST-min-val t) (BST-max-val t)))))


(: BST-search : (All (A) (BST A) A -> (U 'none (BST A))))
;; search the given BST for a key
;; the entire subtree is returned if found
;; If not found, 'none is returned
(define (BST-search t target)
  (match t
    [(BST _ 'none) 'none]
    [(BST lte? (Node my-val lc rc))
     (cond
       [(and (lte? target my-val) (lte? my-val target)) t]
       [(lte? target my-val) (BST-search (BST lte? lc) target)]
       [(lte? my-val target) (BST-search (BST lte? rc) target)]
       [else (error "BST-search: bad cond case")])]))

(: BST-insert : (All (A) A (BST A) -> (BST A)))
;; insert the given value into the tree
(define (BST-insert value t)
  (local
    {
     (: lte? : A A -> Boolean)
     (define lte? (BST-lte? t))
     
     (: node-insert : (U 'none (Node A)) A -> (Node A))
     (define (node-insert node value)
       (match node
         ['none (Node value 'none 'none)]
         [(Node my-val lc rc)
          (cond
            [(lte? value my-val)
             (Node my-val
                  (node-insert lc value)
                  rc)]
            [(lte? my-val value)
             (Node my-val
                   lc
                   (node-insert rc value))]
            [else (error "BST-insert: bad cond case")])]))}
    (BST lte? (node-insert (BST-bst t) value))))

(: BST-delete : (All (A) A (BST A) -> (BST A)))
;; deletes the node with the given key from the BST
;; If the key is not in the given BST, the tree is not modified
;; ONE occurrence of the given key is removed
;; If the tree would become empty, 'none is returned
(define (BST-delete value t)
  (local
    {
     (: lte? : A A -> Boolean)
     (define lte? (BST-lte? t))

     (: node-min : (Node A) -> A)
     (define (node-min node)
       (match node
         [(Node v 'none r) v]
         [(Node v (Node lv ll lr) r) (node-min (Node lv ll lr) )]))
     
     (: root-delete : (Node A) -> (U 'none (Node A))) 
      (define (root-delete node)
        (match node
          [(Node _ 'none r) r]
          [(Node _ l 'none) l]
          [(Node v l (Node rv rl rr))
           (local{(define r (Node rv rl rr)) }
             (Node (node-min r) l (node-delete (node-min r) r)))]))

     (: node-delete : A (U 'none (Node A)) -> (U 'none (Node A)))
     (define (node-delete value node)
       (match node
         ['none 'none]
         [(Node my-val lc rc)
          (cond
            [(and (lte? value my-val) (lte? my-val value)) (root-delete node)]
            [(lte? value my-val)
             (Node my-val
                   (node-delete value lc)
                   rc)]
            [(lte? my-val value)
             (Node my-val
                   lc
                   (node-delete value rc))]
            [else (error "BST-delete: bad cond case")])]))}
    (BST lte? (node-delete value (BST-bst t)))))
        

(: BST->list : (All (A) (BST A) -> (Listof A)))
;; get all the keys in this tree as a list
;; Provided the BST is valid, this list is sorted in increasing order
(define (BST->list t)
  (match t
    [(BST _ 'none) empty]
    [(BST lte? (Node value lc rc))
     (append
      (BST->list (BST lte? lc)) 
      (cons value (BST->list (BST lte? rc))))]))
