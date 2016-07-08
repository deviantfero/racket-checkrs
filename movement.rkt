#lang racket

(require "./libs/io.rkt")
(provide movechip)
(provide move)

(define (movechip M x1 y1 x2 y2)
  (define tmpv1 (list->vector (vector-ref M y1)))
  (define val1 (vector-ref tmpv1 x1))
  (define tmpv2 (list->vector (vector-ref M y2)))
  (vector-set! tmpv1 x1 (vector-ref tmpv2 x2))
  (if (equal? y1 y2)
    (begin
      (vector-set! tmpv1 x2 val1)
      (vector-set! M y1 (vector->list tmpv1))
    )
    (begin
      (vector-set! tmpv2 x2 val1)
      (vector-set! M y1 (vector->list tmpv1))
      (vector-set! M y2 (vector->list tmpv2))
    )
  )
)

;-- checks for horizontal movement
(define (checkHor M x1 x2 y cx)
    (if (> x2 x1)
      (if (< (+ x1 cx) x2) 
        (if (not (zero? (list-ref (vector-ref M y) (+ x1 cx))))
          (checkHor M x1 x2 y (+ cx 1))
          0
        )
        (if (and (> (- x2 1) 0) (equal? (list-ref (vector-ref M y) (- x2 1)) (list-ref (vector-ref M y) x1 )))
          1
          (if (and (< (+ x2 1) 9) (or (zero? (list-ref (vector-ref M y) (+ x2 1)))
                   (equal? (list-ref (vector-ref M y) (+ x2 1)) (list-ref (vector-ref M y) x1 ))))
            1
            2
      )))
      (if (> (- x1 cx) x2) 
        (if (not (zero? (list-ref (vector-ref M y) (- x1 cx))))
          (checkHor M x1 x2 y (+ cx 1))
          0
        )
        (if (and (< (+ x2 1) 9) (equal? (list-ref (vector-ref M y) (+ x2 1)) (list-ref (vector-ref M y) x1 )))
          1
          (if (and (> (- x2 1) 0) (or (zero? (list-ref (vector-ref M y) (- x2 1)))
                   (equal? (list-ref (vector-ref M y) (- x2 1)) (list-ref (vector-ref M y) x1 ))))
            1
            2
     )))
    )
)

;-- checks for vertical movement
(define (checkVer M y1 y2 x cy)
    (if (> y2 y1)
      (if (< (+ y1 cy) y2) 
        (if (not (zero? (list-ref (vector-ref M (+ y1 cy)) x)))
          (checkVer M y1 y2 x (+ cy 1))
          0
        )
        (if (and (> (- y2 1) 0) (equal? (list-ref (vector-ref M (- y2 1)) x) (list-ref (vector-ref M y1) x)))
          1
          (if (and (< (+ y2 1) 9) (or (zero? (list-ref (vector-ref M (+ y2 1)) x))
                   (equal? (list-ref (vector-ref M (+ y2 1)) x) (list-ref (vector-ref M y1) x))))
            1
            2
      )))
      (if (> (- y1 cy) y2) 
        (if (not (zero? (list-ref (vector-ref M (- y1 cy)) x)))
          (checkVer M y1 y2 x (+ cy 1))
          0
        )
        (if (and (< (+ y2 1) 9) (equal? (list-ref (vector-ref M (+ y2 1)) x) (list-ref (vector-ref M y1) x)))
          1
          (if (and (> (- y2 1) 0) (or (zero? (list-ref (vector-ref M (- y2 1)) x))
                   (equal? (list-ref (vector-ref M (- y2 1)) x) (list-ref (vector-ref M y1) x ))))
            1
            2
     )))
    )
)

;-- checks if movement is valid
(define (checkMovement M x1 y1 x2 y2)
  (define difx (- x2 x1))
  (define dify (- y2 y1))
  (printf "~a:~a" difx dify)(newline)
  (if (and (<= difx 1) (>= difx -1) (<= dify 1) (>= dify -1))
    1
    (cond
      ((and (or (> difx 1) (< difx -1)) (zero? dify)) (begin (display (checkHor M x1 x2 y1 1)) (checkHor M x1 x2 y1 1)))
      ((and (or (> dify 1) (< dify -1)) (zero? difx)) (begin (display (checkVer M y1 y2 x1 1)) (checkVer M y1 y2 x1 1)))
    (else 0))
  )
)

(define (move M x1 y1 x2 y2 p)
  (define condition (checkMovement M x1 y1 x2 y2))
  (if (and 
      (equal? (list-ref (vector-ref M y1) x1) p)
      (zero? (list-ref (vector-ref M y2) x2)))
    (case condition
      ((0) #f)
      ((1) (begin (movechip M x1 y1 x2 y2) #t))
      ((2) (begin (movechip M x1 y1 x2 y2) #f))
    );-- ends checkMovement if
    #f
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
