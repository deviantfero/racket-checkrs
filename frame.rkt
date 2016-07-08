#lang racket/gui

(provide mainframe)

;; Needed to define the mainframe in a different file
;; As it is a mainframe which will be shared by all
;; canvases

(define mainframe
  (new frame%
       [label "Racket Checkers"]
       [width 922]
       [height 576]))
