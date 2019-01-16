
;; A recursive function, keep-first-n, that returns a list of the first n elements in a list.
(define keep-first-n
    (lambda (L x)
      (if (< (length L) x)
          #f
      (if (< x 0)
          #f
      (if (= x 0)
          '()
          (cons (car L)
                (keep-first-n (cdr L) (- x 1))))))))

(keep-first-n '(1 2 3) -1)
(keep-first-n '(1 2 3) 4)
(keep-first-n '(1 2 3) 2)

;; A recursive function, sum, that adds up all the elements of a list.
(define sum
    (lambda (L)
        (if (null? L)
            0
            (+ (car L) (sum (cdr L))))))

(sum '())
(sum '(1 2 4))

(or (values 1 2))