#lang racket
(require "HighScore.rkt")
;;(Ordenar) (Funcion Principal)
(define (Ordenar)
  (define FileR1 (open-input-file "Puntajes.txt" #:mode 'text))
  (define VecR (make-vector (contarLineas FileR1 0) ""))
  (close-input-port FileR1)
  (define FileR2 (open-input-file "Puntajes.txt" #:mode 'text))
  (leer FileR2 VecR (vector-length VecR) 0 (read-line FileR2 'any))
  (close-input-port FileR2)
  (Ordenador VecR (vector-length VecR) 0 1 "")
  (define FileW (open-output-file "Puntajes.txt" #:mode 'text #:exists'replace))
  (Writting VecR FileW (vector-length VecR) 0)
  (close-output-port FileW)
  
)

;; (Ordenador Vector TamañoVector Contador ContadorAuxiliar Cadena)
;;      Se encarga de ordenar los contenidos de un vector de mayor a menor, usando Contador y ContadorAuxiliar para acceder a dos partes
;;      diferentes del vector, usa Cadena como pivote para intercambiar los valores.
(define (Ordenador Vector t cont contAux cad)
  (if (< cont t)
      (if (< contAux t)
          (if (< (string->number (list-ref (string-split (vector-ref Vector cont) "/") 1)) (string->number (list-ref (string-split (vector-ref Vector contAux) "/") 1)))
                (begin
                  (set! cad (vector-ref Vector cont))
                  (vector-set! Vector cont (vector-ref Vector contAux))
                  (vector-set! Vector contAux cad)
                  (Ordenador Vector t 0 1 cad)
                 )
                (Ordenador Vector t cont (+ contAux 1) cad)
           )
          (Ordenador Vector t (+ cont 1) (+ cont 2) cad)
        )
      (display Vector)
    )
)

(define Vec1 #("Carlos/2" "Eduardo/7" "Mario/4"))
