#lang racket

(provide in)
(provide displayM)

(define (in message)
  (display message)
  (read)
)

(define (displayMAUX M c)
  (if (< c (length M))
    (begin
      (display (list-ref M c))(newline)
      (displayMAUX M (+ c 1))
    )
    (newline)
  )
)

(define (displayM M)
  (displayMAUX M 0)
)
