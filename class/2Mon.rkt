(define subst
  (lambda (old new input)
    (cond [(null? input) '()]
          [(list? input) (cons (subst old new (car input)) (subst old new (cdr input)))]
          [(equal? old input) new]
          [else input])))

(subst 'b 'a '((b c) (b () d)))
(subst 'b 'a 'b)

(define cons-each
  (lambda (ele input)
    (if (null? input)
         '()
         (cons (cons ele (car input)) (cons-each ele (cdr input))))))

(cons-each 'a '((b) (c) (d)))

(define subsets
  (lambda (lst)
    (if (null? lst) '(())
        (append
         (subsets (cdr lst))
         (cons-each (car lst) (subsets (cdr lst)))))))

(subsets '(a b c d))
          