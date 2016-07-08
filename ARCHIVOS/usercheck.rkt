#lang racket

(define file (open-input-file "./users.txt"))

(define (writeuser user pass)
  (define file (open-output-file "./users.txt" #:exists 'append))
  (define file2 (open-output-file "./Puntajes.txt" #:exists 'append))
  (display user file)(display "," file)(display pass file)(newline file)
  (display user file2)(display "/" file2)(display 0 file2)(newline file2)
  (close-output-port file)
  (close-output-port file2)
)

(define (checkuser user pass)
  (define cl (read-line file))
  (display cl)
  (if (not (equal? cl eof))
    (if (and (equal? user (car (string-split cl ","))) (equal? pass (cadr (string-split cl ","))))
      (begin
        (close-input-port file)
        #t
      )
      (checkuser user pass)
    );--ends true
    (begin
      (close-input-port file)
      (writeuser user pass)
      #f
    )
  )
)

(checkuser "vasquezito" "123")
