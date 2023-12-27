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


; unit test
(module+ test
    (check-equal? (num (list 5)) 32)
    (check-equal? (num (list 5 2)) 288)
    (check-equal? (num (list 5 2 8)) 112500000)
    (check-equal? (num (list 5 2 8 2)) 5512500000)
)
