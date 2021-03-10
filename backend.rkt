#lang racket
(require racket/random)

(define (select-random-figure random-number)
    (cond 
        ((= random-number 0) "C")
        ((= random-number 1) "E")
        ((= random-number 2) "T")
        (else "R")))

(define (select-random-value random-number)
    (cond 
        ((= random-number 1) "A")
        ((= random-number 10) "J")
        ((= random-number 11) "Q")
        ((= random-number 12) "K")
        (else random-number)))

(define (pick-random-card random-value random-figure)
    (list random-value random-figure))



