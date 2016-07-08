#lang racket

(provide checkuser)

(define (writeuser user pass)
  (define file (open-output-file "ARCHIVOS/users.txt" #:exists 'append))
  (define file2 (open-output-file "ARCHIVOS/Puntajes.txt" #:exists 'append))
  (display user file)(display "," file)(display pass file)(newline file)
  (display user file2)(display "/" file2)(display 0 file2)(newline file2)
  (close-output-port file)
  (close-output-port file2)
)

(define (checkuser user pass)
  (define file (open-input-file "ARCHIVOS/users.txt"))
  (checkuserAux user pass file)
)

(define (checkuserAux user pass file)
  (define cl (read-line file))
  (if (not (equal? cl eof))
    (if (equal? user (car (string-split cl ",")))
      (if (equal? pass (cadr (string-split cl ",")))
        (begin
          (close-input-port file)
          #t
        )
        (begin
          (close-input-port file)
          #f
        ))
      (checkuserAux user pass file)
    );--ends true
    (begin
      (close-input-port file)
      (writeuser user pass)
      #f
    )
  )
)
