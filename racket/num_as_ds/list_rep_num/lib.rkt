#lang racket
(require racket/match)
(require math/number-theory)

(module+ test
  (require rackunit))


; a helper function to convert a list of integers to a numeric representation
(define (num s)
    (define (raise-power-by-index lst)
        (map (lambda (index element) (expt (nth-prime index) element))
            (range (length lst))
            lst
        )
    )
    
    (cond ((null? s) 1)
        (else (foldl * 1 (raise-power-by-index s)))
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Required functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (not-equal? a b)
  (not (equal? a b))
)

(define (is-empty? n)
  (equal? n 1)
)
(define (is-not-empty? n)
  (not (is-empty? n))
)

(define (divides? p q)
  (zero? (modulo p q)))


(define (highest-exponent-dvides n p)
  (define (impl n p i prev)
      (define cur (* p prev))
      (let-values ([(q r) (quotient/remainder n cur)])
        (cond 
          [(equal? r 0) (impl n p (+ i 1) cur)]
          [else (values (- i 1) prev)])))
  (cond 
    [(divides? n p) (impl n p 1 1)] 
    [else (values 0 1)])
)

; List definition:
;;; The empty list is represented by 1

;;; If j is the kth element of a nonempty list s, and if p is the kth prime number,
;;; then (1) p^j = (expt p j) is a factor of (num s), and (2) no higher power of p is a factor of (num s).

; myequal
; Args: n representing a list s, m representing a list t
; Return: #t if s and t are the same
(define (myequal n m)
    (define (impl n m i)
        (cond 
            [(and (is-empty? n) (is-empty? m)) #t]
            [else (cond
                    [(and (is-not-empty? n) (is-not-empty? m)) 
                        (begin 
                            (define p (nth-prime i))
                            (let-values 
                              ([(a-exp a-prod) (highest-exponent-dvides n p)]
                                [(b-exp b-prod) (highest-exponent-dvides m p)])
                              
                              (cond 
                                [(and (not-equal? a-exp 0) (not-equal? b-exp 0) (equal? a-exp b-exp)) 
                                  (impl (/ n a-prod) (/ m b-prod) (+ i 1))] 
                                [else #f])))]
                    [else #f])
            ]
        )
    )

    (impl n m 0)
)

; ; index-of
; ; Args: n representing a list s, k representing the index of the list [0, n - 1], with the assumption i is always valid
; ; Return: the element of list s at k-th position
(define (index-of n k)
  (define p (nth-prime k))
  (let-values 
    ([(exp prod) (highest-exponent-dvides n p)])
    (cond
      [(equal? exp 0) (error "index " k " is invalid for the list " n)]
      [else exp]
    )
  )
)

; ; head
; ; Args: n representing a list s
; ; Return: the first element of s
(define (head n)
  (index-of n 0)
)

; ; tail
; ; Args: n representing a list s
; ; Return: a number representing the list obtained from s by removing its first element
; (define (tail n)

; )

; ; insert-at-head
; ; Args: n representing a list s, p representing a number
; ; Return: a number representing the list obtained by inserting p at the head of the list s 
; (define (insert-at-head n p)
; )

; ; len
; ; Args: n representing a list s
; ; Return: the number of elements in the list
; (define (len n)

; )

; ; snoc
; ; Args: n representing a list s, q representing a number
; ; Return: the number representing the list obtained by inserting q at the end of the list s
; (define (snoc n q)

; )

; ; last
; ; Args: n representing a list s
; ; Return: the last element in the list s
; (define (last n)
; )

; ; insert-at
; ; Args: n representing a list s, x representing a second number x, y representing the position
; ; Return: the number representing the list obtained by inserting x in the yth position of s.
; (define (insert-at n x y)
; )


; ; myappend
; ; Args: m representing a list s, n representing a list t
; ; Return: the number representing the list formed by appending s and t
; (define (myappend m n)
; )


; ; myreverse
; ; Args: n representing a list s
; ; Return: the number representing the reverse of s
; (define (myreverse n)

; )

; ; palin
; ; Args: n representing a list s
; ; Return: #t if s is a palindrome
; (define (palin n)

; )

; ; palin
; ; Args: n representing a list s
; ; Return: the number representing the sorted of s
; (define (sort n)

; )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Unit Test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ test
    (check-equal? (num (list 5)) 32)
    (check-equal? (num (list 5 2)) 288)
    (check-equal? (num (list 5 2 8)) 112500000)
    (check-equal? (num (list 5 2 8 2)) 5512500000)

    ; myequal
    (check-equal? (myequal (num (list 5 2 8 2)) (num (list 5 2 8 2))) #t)
    (check-equal? (myequal (num (list 5 2 8)) (num (list 5 2 8 2))) #f)
    
    ; index-of
    (check-equal? (index-of (num (list 10 5 2)) 0) 10)
    (check-equal? (index-of (num (list 10 5 2)) 1) 5)
    (check-equal? (index-of (num (list 10 5 2)) 2) 2)

    ; head
    (check-equal? (head (num (list 10 5 2))) 10)
    (check-equal? (head (num (list 5 2))) 5)
    (check-equal? (head (num (list 2))) 2)
)
