#lang racket

(provide initGameMatrix)
(provide getPat)
(provide getVat)

(define (set-p2 v cant len)
  (if (> cant 0)
    (begin
      (vector-set! v (- len cant) 2)
      (set-p2 v (- cant 1) len)
    )
    (vector->list v)
  )
)

(define (set-p1 v cant pos)
  (if (< cant 4)
    (begin
      (vector-set! v pos 1)
      (set-p1 v (+ cant 1) (+ pos 1))
    )
    (vector->list v)
  )
)

(define (initMatrix2 m pos cant)
  (define row (vector-ref m pos))
  (if (< pos 4)
    (begin
      (vector-set! m pos (set-p2 (list->vector row) cant (length row)))
      (initMatrix2 m (+ pos 1) (- cant 1))
    )
    m
  )
)

(define (initMatrix1 m pos cant)
  (if (< pos 9)
    (begin
      (vector-set! m pos (set-p1 (list->vector (vector-ref m pos)) cant 0))
      (initMatrix1 m (+ pos 1) (- cant 1))
    )
    m
  )
)

(define (createMatrix x y)
  (make-vector x (make-list y 0))
)


(define (getPat M x y)
  (if (and (< x 9) (< y 9))
    (list-ref (list-ref M y) x)
    (void)
  )
)

(define (getVat M x y)
  (if (and (< x 9) (< y 9))
    (list-ref (vector-ref M y) x)
    (void)
  )
)

;--fucnion para crear la matriz principal de juego
(define (initGameMatrix)
  (define matrix (createMatrix 9 9))
  (initMatrix2 matrix 0 4)
  (initMatrix1 matrix 5 3)
  matrix
)
