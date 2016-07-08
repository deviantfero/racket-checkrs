#lang racket
(provide contarLineas)
(provide leer)
(provide ActualizarHScore)
;; (ActualizarHScore String):(Funcion Principal)
;;      Actualiza el archivo donde se guardan los puntajes, ya sea agregando el nombre y puntaje o aumentando en 1 un puntaje ya existente.
(define (ActualizarHScore Nombre)
  (define FileR1 (open-input-file "ARCHIVOS/Puntajes.txt" #:mode 'text))
  (define VecR (make-vector (contarLineas FileR1 0) ""))
  (close-input-port FileR1)
  (define FileR2 (open-input-file "ARCHIVOS/Puntajes.txt" #:mode 'text))
  (leer FileR2 VecR (vector-length VecR) 0 (read-line FileR2 'any))
  (close-input-port FileR2)
  (display VecR)
  (define VecW (EncontrarEditar Nombre VecR (vector-length VecR) 0))
  (display VecW)
  (define FileW (open-output-file "ARCHIVOS/Puntajes.txt" #:mode 'text #:exists'replace))
  (Writting VecW FileW (vector-length VecW) 0)
  (close-output-port FileW)
)

;; (contarLineas Flujo Contador(Entero)):
;;      Regresa un entero con el total de lineas de un archivo.
(define (contarLineas File cont)
  (if (not(equal? (read-line File 'any) eof))
      (contarLineas File (+ cont 1))
      (if (< cont 1)
          (+ cont 1)
          cont
       )
   )
)
;; (leer Flujo Vector TamañoVector Contador Cadena):
;;     Guarda los contenidos de un archivo dentro de un vector, almacenando cada linea de manera inicial en Cadena para evitar guardar un EOF.
(define (leer File Vector t cont cad)
  (if (< cont t)
      (if (not (equal? cad eof))
        (begin
         (vector-set! Vector cont cad)
         (leer File Vector t (+ cont 1) (read-line File 'any))
        )
        (display "")
      )
      (display "")
    )
)
;; (EncontrarEditar String Vector TamañoVector Contador):
;;    Se encarga de recorrer el vector para encontrar String, en cuyo caso aumenta el numero junto a el en 1 o en caso de no encontrarlo agregarlo a las lineas del archivo. 
(define (EncontrarEditar Nombre Vector t cont)
  (if (< cont t)
      (if (or (< (string-length (vector-ref Vector cont)) (string-length Nombre)) (not (equal? (substring (vector-ref Vector cont) 0 (string-length Nombre)) Nombre)))
        (EncontrarEditar Nombre Vector t (+ cont 1))
        (begin
          (vector-set! Vector cont (string-append Nombre "/" (number->string (+ (string->number (substring (vector-ref Vector cont) (+ (string-length Nombre) 1))) 1))))
          Vector
         )
       )
      (vector-append Vector (make-vector 1 (string-append Nombre "/0")))
   )
)
;; (Writting Vector Flujo Tamaño Vector Contador)
;;     Se encarga de escribir los contenidos de un vector en un archivo.
(define (Writting Vector File t cont)
  (if (< cont t)
      (begin 
        (display (vector-ref Vector cont) File)
        (newline File)
        (Writting Vector File t (+ cont 1))
       )
      (display "")
   )
)
