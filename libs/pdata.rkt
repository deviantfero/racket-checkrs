#lang racket

(provide writePlayer)
(provide writeWinner)
(provide write2Player)
(provide readPlayers)
(provide readWinner)
(provide getHigh)

(define (writePlayer n)
  (define ofile (open-output-file "ARCHIVOS/players.txt" #:exists 'truncate))
  (display n ofile)(newline ofile)
  (close-output-port ofile)
)

(define (writeWinner n)
  (define ofile (open-output-file "ARCHIVOS/win.txt" #:exists 'truncate))
  (display n ofile)(newline ofile)
  (close-output-port ofile)
)

(define (write2Player n)
  (define ofile (open-output-file "ARCHIVOS/players.txt" #:exists 'append))
  (display n ofile)(newline ofile)
  (close-output-port ofile)
)

(define (readPlayers)
  (define rfile (open-input-file "ARCHIVOS/players.txt"))
  (define plist (list (read-line rfile) (read-line rfile)))
  (close-input-port rfile)
  plist
)

(define (readWinner)
  (define rfile (open-input-file "ARCHIVOS/win.txt"))
  (define plist (read-line rfile))
  (close-input-port rfile)
  plist
)

(define (getHighAux c L file)
  (define cl (read-line file))
  (if (and (< c 10) (not (equal? cl eof)))
    (getHighAux (+ c 1) (append L (list cl "\n")) file)
    L
  )
)

(define (getHigh)
  (define rfile (open-input-file "ARCHIVOS/Puntajes.txt"))
  (define plist (getHighAux 0 '() rfile))
  (close-input-port rfile)
  plist
)
