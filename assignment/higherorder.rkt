(define test
  (lambda (a b c)
    (+ a (+ b c))))

(test 1 2 3)

;; (a) Returns a curried version of a three-argument function
(define curry3
  (lambda (f)
    (lambda (x)
      (lambda (y)
        (lambda (z)
          (f x y z))))))

((((curry3 test) 1) 2) 3)

;; (b) Takes a curried three-argument function and returns a normal Scheme
;; uncurried version of that function
(define uncurry3
  (lambda (f)
    (lambda (x y z)
      (((f x) y) z))))

((uncurry3 (curry3 test)) 1 2 3)

;; (c) An implementation of Scheme's filter function
(define my-filter
  (lambda (pred lst)
    (cond
      [(null? lst) '()]
      [(pred (car lst)) (cons (car lst)
                              (my-filter pred (cdr lst)))]
      [(my-filter pred (cdr lst))])))

(filter positive? '(1 -2 3 4 -5))
(my-filter positive? '(1 -2 3 4 -5))

;; (d) An implementation of Scheme's union function
(define my-union
  (lambda (lst1 lst2)
    (cond
      [(null? lst2) lst1]
      [(member (car lst2) lst1) (my-union lst1 (cdr lst2))]
      [else (my-union (cons (car lst2) lst1) (cdr lst2))])))

(my-union '(1 2 3) '(3 4 5))

;; (d) An implementation of Scheme's intersect function
(define my-intersect
  (lambda (lst1 lst2)
    (cond
      [(null? lst2) null]
      [(null? lst1) null]
      [(member (car lst2) lst1) (append (list (car lst2)) (my-intersect lst1 (cdr lst2)))]
      [else (my-intersect lst1 (cdr lst2))])))

(my-intersect '(1 2 3) '(4 5 6))
(my-intersect '(1 2 3) '(3 2 1))
(my-intersect '(1 2 3) '(2 3 4 5 6))

;; (e) Returns #t if at least one item in a list satisfies a predicate
(define exists
  (lambda (pred lst)
    (if (equal? (my-filter pred lst) '()) #f
    #t)))

(exists (lambda (x) (eq? x 0)) '(-1 0 1))
(exists (lambda (x) (eq? x 2)) '(-1 0 1))