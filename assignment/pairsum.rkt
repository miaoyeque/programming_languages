;; (a) Generates a list of consecutive integers from start to stop
(define gen-list
  (lambda (start stop)
    (if (> start stop) '()
        (cons start (gen-list (+ 1 start) stop)))))

(gen-list 1 20)

;; (a) Tests whether any two adjacent values in lst sum to val
(define pair-sum?
  (lambda (lst val)
    (if (< (length lst) 2) #f
    (if (= (+ (car lst) (car (cdr lst))) val) #t
        (pair-sum? (cdr lst) val)))))

(pair-sum? '(1 2 3) 3)

;; (b) gen-lazy-list as defined in the assignment
(define gen-lazy-list
  (lambda (start stop)
    (if (> start stop)
        #f
        (cons start
            (lambda () (gen-lazy-list (+ start 1) stop))))))

(gen-lazy-list 1 3)

;; (b) Takes a lazy integer sequence as defined by gen-lazy-list
;; and tests whether any two adjacent values in the sequence sum to val
(define pair-sum-lazy?
  (lambda (seq val)
    (if (eq? ((cdr seq)) #f)
        #f
    (if (= (+ (car seq) (car ((cdr seq)))) val) #t
        (pair-sum-lazy? ((cdr seq)) val)))))

(pair-sum-lazy? (gen-lazy-list 1 4) 6)

;; (c) Returns a lazy version of a traditional Scheme list
(define make-lazy
  (lambda (lst)
    (if (null? lst) '()
        (cons (car lst)
              (lambda () (make-lazy (cdr lst)))))))

(make-lazy '())

;; Helper function that checks whether a number adds to any number in a lazy list
;; to sum val
(define add-each-sum?
  (lambda (x lst val)
    (if (null? lst) #f
    (if (= (+ x (car lst)) val) #t
    (if (eq? ((cdr lst)) #f) #f
        (if (= (+ x (car ((cdr lst)))) val) #t
            (add-each-sum? x ((cdr lst)) val)))))))

(add-each-sum? 3 (make-lazy '(1 2 3 5)) 8)

;; (d) Given a lazy list, returns if any two integers add to the value provided
(define any-sum-lazy?
  (lambda (lst val)
    (if (null? lst) #f
    (if (eq? ((cdr lst)) #f) #f
        (or (add-each-sum? (car lst) ((cdr lst)) val)
        (any-sum-lazy? ((cdr lst)) val))))))

(any-sum-lazy? (make-lazy '(12 323 21 2 3)) 15)