;; (a) Generates an integer lazy list from first to last
(define seq
  (lambda (first last)
    (if (> first last)
        #f
        (cons first
            (lambda () (seq (+ first 1) last))))))

(seq 1 5)

;; (a) Returns an integer lazy list containing the infinite sequence from first
(define inf-seq
  (lambda (first)
    (cons first
          (lambda () (inf-seq (+ first 1))))))

;; (a) Returns an ordinary Scheme list containing the first n values in
;; lazy-list
(define first-n
  (lambda (lazy-list n)
    (cond
      [(null? lazy-list) '()]
      [(= n 0) '()]
      [(boolean? ((cdr lazy-list))) (append (list (car lazy-list)))]
      [else (cons (car lazy-list) (first-n ((cdr lazy-list)) (- n 1)))])))

(first-n (seq 1 5) 7)

;; (a) Takes a lazy list and an integer and returns the n-th value in the
;; lazy-list
(define nth
  (lambda (lazy-list n)
    (cond
      [(boolean? ((cdr lazy-list))) #f]
      [(= n 1) (car lazy-list)]
      [else (nth ((cdr lazy-list)) (- n 1))])))

(nth (seq 1 5) 2)

;; (a) Helper function that decides whether val is multiples of n
(define is-multiples?
  (lambda (val n)
    (if (= (modulo val n) 0)
        #t
        #f)))

(is-multiples? 9 3)

;; (a) Returns a new lazy list that has n and all integer multiples of n
;; removed from lazy-list
(define filter-multiples
  (lambda (lazy-list n)
    (if (boolean? lazy-list)
        #f
        (if (is-multiples? (car lazy-list) n)
            (filter-multiples ((cdr lazy-list)) n)
            (cons (car lazy-list)
                  (lambda () (filter-multiples ((cdr lazy-list)) n)))))))

(filter-multiples (seq 1 5) 2)

;; (b) Helper function that returns a lazy list that sieves the first element from
;; the rest
(define sieve
  (lambda (lazy-list)
    (if (boolean? lazy-list)
      #f
      (cons (car lazy-list)
            (lambda () (sieve (filter-multiples lazy-list (car lazy-list))))))))

;; (b) Computes a lazy list containing all prime numbers, starting at 2, using
;; "Sieve of Eratosthenes"
(define (primes)
  (sieve (inf-seq 2)))

(first-n (primes) 10)
(nth (primes) 20)

;; (c) Helper function that counts the number of values in the list smaller than n
(define check-smaller-number?
  (lambda (lazy-list n counter)
    (if (boolean? lazy-list) counter
        (if (< (car lazy-list) n)
            (check-smaller-number? ((cdr lazy-list)) n (+ counter 1))
        counter))))

;; (c) Returns the number of primes less than the integer parameter
(define count-smaller-primes
  (lambda (n)
    (check-smaller-number? (primes) n 0)))

(count-smaller-primes 11)