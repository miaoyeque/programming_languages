;; Helper functions that return the value of the root, left subtree and right subtree
(define root-value (lambda (bst) (list-ref bst 0)))
(define left-subtree (lambda (bst) (list-ref bst 1)))
(define right-subtree (lambda (bst) (list-ref bst 2)))

;; Helper function that checks whether an input is in the correct tree format
(define tree?
  (lambda (bst)
    (cond
      [(null? bst) #f]
      [(not (= (length bst) 3)) #f]
      [(not (list? (left-subtree bst))) #f]
      [(not (list? (right-subtree bst))) #f]
      [else #t])))

;; A function that returns the node of bst
(define entry
  (lambda (bst)
    (if (equal? (tree? bst) #t) (root-value bst) #f)))

;; A function that returns the left subtree of bst
(define left
  (lambda (bst)
    (if (equal? (tree? bst) #t) (left-subtree bst) #f)))

;; A function that returns the right subtree of bst
(define right
  (lambda (bst)
    (if (equal? (tree? bst) #t) (right-subtree bst) #f)))

(entry '(5 (2) (1)))
(left '(5 (2) (1)))
(right '(5 (2) (1)))

;; Returns a new tree whose root node is elt, left subtree is left,
;;and right subtree is right.
(define make-bst
  (lambda (elt left right)
    (if (not (number? elt)) #f
        (cond
          [(equal? (tree? left) #f) #f]
          [(equal? (tree? left) #f) #f]
          [else (list elt left right)]))))

(make-bst 5 '(2 () ()) '(6 () ()))

;; Returns a list containing all values in bst from a preorder traversal
(define preorder
  (lambda (bst)
    (cond
      ((null? bst) '())
      (else (append
              (list (root-value bst))
              (preorder (left-subtree bst))
              (preorder (right-subtree bst)))))))

;; Returns a list containing all values in bst from an inorder traversal
(define inorder
  (lambda (bst)
    (cond
      ((null? bst) '())
      (else (append
              (inorder (left-subtree bst))
              (list (root-value bst))
              (inorder (right-subtree bst)))))))

;; Returns a list containing all values in bst from a postorder traversal
(define postorder
  (lambda (bst)
    (cond
      ((null? bst) '())
      (else (append
              (postorder (left-subtree bst))
              (postorder (right-subtree bst))
              (list (root-value bst)))))))

(preorder '(5 (3 () (4 () ()) ) (4 (1 () ()) (2 () ()))))
(inorder '(5 (3 () (4 () ()) ) (4 (1 () ()) (2 () ()))))
(postorder '(5 (3 () (4 () ()) ) (4 (1 () ()) (2 () ()))))

;; Return a new binary search tree with integer v appearing in proper location
(define insert
  (lambda (v bst)
    (cond
      [(null? bst) (list v () ())]
      [(= (car bst) v) bst]
      [(> (car bst) v) (list (car bst) (insert v (left-subtree bst)) (right-subtree bst))]
      [(< (car bst) v) (list (car bst) (left-subtree bst) (insert v (right-subtree bst)))]
    )))

(insert 6 '(5 (3 () ()) (7 () ())))

;; Bonus point - returns #t if the tree is well-formed, and #f otherwise
(define proper-tree?
  (lambda (bst)
    (if (null? bst) #t
     (and 
      (and
       (< (abs (- (tree-height (left-subtree bst)) (tree-height (right-subtree bst)))) 1)
      (proper-tree? (left-subtree bst)))
     (proper-tree? (right-subtree bst))))))
        

;; Helper function to find the height of bst
(define tree-height
  (lambda (bst)
    (if (null? bst) 0
    (if (and (null? (left-subtree bst)) (null? (right-subtree bst))) 1
      (+ 1 (max (tree-height (left-subtree bst))
                      (tree-height (right-subtree bst))))))))

(proper-tree? '(5 (3 () ()) (7 () ())))
(proper-tree? '(5 (3 () (4 () ()) ) (4 (1 () ()) (2 () ()))))