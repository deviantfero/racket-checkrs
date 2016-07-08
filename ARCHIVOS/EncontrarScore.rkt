#lang racket
;; Requiere de las funciones Leer y contarLineas pertenecientes al archivo HighScore.rkt
;;
(require "HighScore.rkt")
;; (EncontrarScore String) (Funcion Principal)
;;   Se encarga de obtener el puntaje perteneciente a String, almacenado en un archivo.
(define (EncontrarScore Nombre)
  (define FileR1 (open-input-file "Puntajes.txt" #:mode 'text))
  (define VecR (make-vector (contarLineas FileR1 0) ""))
  (close-input-port FileR1)
  (define FileR2 (open-input-file "Puntajes.txt" #:mode 'text))
  (leer FileR2 VecR (vector-length VecR) 0 (read-line FileR2 'any))
  (close-input-port FileR2)
  (EncontrarPuntaje Nombre VecR (vector-length VecR) 0)
)
  
;;(EncontrarPuntaje String Vector TamañoVector Contador)
;;    Se encarga de recorrer un Vector y retornanr un entero erteneciente a String, los cuales estan almacenados como una soloa caden.

(define (EncontrarPuntaje Nombre Vector t cont)
  (if (< cont t)
      (if (or (< (string-length (vector-ref Vector cont)) (string-length Nombre)) (not (equal? (substring (vector-ref Vector cont) 0 (string-length Nombre)) Nombre)))
        (EncontrarPuntaje Nombre Vector t (+ cont 1))
        (string->number (substring (vector-ref Vector cont) (+ (string-length Nombre) 1)))
          
       )
      (display "")
   )
)
