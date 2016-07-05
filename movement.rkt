#lang racket

(require "./matriz.rkt")
(define chipboard (initGameMatrix))

(define (movechip M x1 y1 x2 y2)
  (define tmpv1 (list->vector (vector-ref M y1)))
  (define val1 (vector-ref tmpv1 x1))
  (define tmpv2 (list->vector (vector-ref M y2)))
  (vector-set! tmpv1 x1 (vector-ref tmpv2 x2))
  (vector-set! tmpv2 x2 val1)
  (vector-set! M y1 (vector->list tmpv1))
  (vector-set! M y2 (vector->list tmpv2))
)

(define (move1 M x1 y1 x2 y2)
  (if (and 
      (equal? (list-ref (vector-ref M y1) x1) 1)
      (zero? (list-ref (vector-ref M y2) x2)))
    (movechip M x1 y1 x2 y2)
    (display "movimiento invalido")
  )
)

(define (move2 M x1 y1 x2 y2)
  (if (and 
      (equal? (list-ref (vector-ref M y1) x1) 2)
      (zero? (list-ref (vector-ref M y2) x2)))
    (movechip M x1 y1 x2 y2)
    (display "movimiento invalido\n")
  )
)

