#lang racket

(require "./libs/io.rkt")
(provide movechip)

(define (movechip M x1 y1 x2 y2)
  (define tmpv1 (list->vector (vector-ref M y1)))
  (define val1 (vector-ref tmpv1 x1))
  (define tmpv2 (list->vector (vector-ref M y2)))
  (vector-set! tmpv1 x1 (vector-ref tmpv2 x2))
  (vector-set! tmpv2 x2 val1)
  (vector-set! M y1 (vector->list tmpv1))
  (vector-set! M y2 (vector->list tmpv2))
)

(define (move M x1 y1 x2 y2 p)
  (if (and 
      (equal? (list-ref (vector-ref M y1) x1) p)
      (zero? (list-ref (vector-ref M y2) x2)))
    (movechip M x1 y1 x2 y2)
    (begin
      (display "movimiento invalido")
      (move M (in "X1:") (in "Y1:") (in "X2:") (in "Y2:") p)
    )
  )
)

;-- checks Player2 wincondition
(define (checkP2 M lim row)
  (if (< row 9)
    (if (checkP2Aux M lim row)
      (checkP2 M (+ lim 1) (+ row 1))
      #f
    )    
    #t
  )
)

(define (checkP2Aux M lim row)
  (define evalrow (vector-ref M row))
  (if (andmap (lambda (n) (equal? n 2)) (take evalrow lim))
    #t
    #f
  )
)

;-- checks Player 1 win condition
(define (checkP1 M lim row)
  (if (< row 4)
    (if (checkP1Aux M lim row)
      (checkP1 M (- lim 1) (+ row 1))
      #f
    )    
    #t
  )
)

(define (checkP1Aux M lim row)
  (define evalrow (vector-ref M row))
  (if (andmap (lambda (n) (equal? n 1)) (list-tail evalrow lim))
    #t
    #f
  )
)

(define (wincondition M)
  (if (checkP1 M 4 0)
    1
    (if (checkP2 M 1 5)
      2
      0
    )
  )
)

;;(define (playtest M p)
;;  (displayM (vector->list M))
;;  (define win (wincondition M))(newline)
;;  (display "Player")(display p)(newline)
;;  (if (zero? win)
;;    (begin
;;      (move M (in "X1:") (in "Y1:") (in "X2:") (in "Y2:") p)
;;      (if (> p 1)
;;        (playtest M 1)
;;        (playtest M 2)
;;      )
;;    );-- ends with begin
;;    (if (equal? win 1)
;;      (display "Player 1 wins")
;;      (display "Player 2 wins")
;;    )
;;  )
;;)
