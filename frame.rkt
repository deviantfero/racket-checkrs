#lang racket/gui

(provide frame)

;; Needed to define the frame in a different file
;; As it is a frame which will be shared by all
;; canvases

(define frame
  (new frame%
       [label "Racket Checkers"]
       [width 922]
       [height 576]))
