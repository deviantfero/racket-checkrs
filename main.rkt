#lang racket/gui

(provide frame)
(require "./login.rkt")
(require "./frame.rkt")

(define bg (make-object bitmap% "img/bg.jpg"))
(define bg-howto (make-object bitmap% "img/HOWTO.jpg"))
(define bg-scores (make-object bitmap% "img/scores.jpg"))

(struct gpt (x y))
(struct gbutton (pt-1 pt-2 img img-pt))
; (struct gbutton (pt-1 pt-2 img img-pt cb))

;-- defines menu-canvas where it connects to all other screens
(define menu-canvas%
  (class canvas%
    (inherit refresh)

    (define sel-button 'none)
    (define sel-button-pt 'none)

    (define scores (make-object bitmap% "img/score.jpg"))
    (define play (make-object bitmap% "img/play.jpg"))
    (define howto (make-object bitmap% "img/howto.jpg"))

    (define buttons
      (list
       (gbutton (gpt 60 442) (gpt 222 529) scores (gpt 38 420))
       (gbutton (gpt 362 339) (gpt 545 409) play (gpt 362 339))
       (gbutton (gpt 710 85) (gpt 883 163) howto (gpt 702 74))))

    (define (handle-mouse-motion x y)
      (let loop ([bl buttons])
        (if (empty? bl)
           (refresh)
           (let* ([b (car bl)]
                 [pt-1 (gbutton-pt-1 b)]
                 [pt-1-x (gpt-x pt-1)]
                 [pt-1-y (gpt-y pt-1)]
                 [pt-2 (gbutton-pt-2 b)]
                 [pt-2-x (gpt-x pt-2)]
                 [pt-2-y (gpt-y pt-2)])
             (if (and (> x pt-1-x) (> y pt-1-y) (< x pt-2-x) (< y pt-2-y))
                 (begin
                   (set! sel-button (gbutton-img b))
                   (set! sel-button-pt (gbutton-img-pt b))
                   (refresh))
                 (begin
                   (set! sel-button 'none)
                   (set! sel-button-pt 'none)
                   (loop (cdr bl))))))))

    (define (action x y)
      (cond
        ([and (and (> x (gpt-x (gbutton-pt-1 (first buttons)))) 
             (< x (gpt-x (gbutton-pt-2 (first buttons)))))
              (and (> y (gpt-y (gbutton-pt-1 (first buttons))))
                    (< y (gpt-y (gbutton-pt-2 (first buttons)))))] (set-canvas 1))
        ([and (and (> x (gpt-x (gbutton-pt-1 (second buttons)))) 
                     (< x (gpt-x (gbutton-pt-2 (second buttons)))))
                      (and (> y (gpt-y (gbutton-pt-1 (second buttons))))
                            (< y (gpt-y (gbutton-pt-2 (second buttons)))))] (set-canvas 2))
        ([and (and (> x (gpt-x (gbutton-pt-1 (third buttons)))) 
                     (< x (gpt-x (gbutton-pt-2 (third buttons)))))
                      (and (> y (gpt-y (gbutton-pt-1 (third buttons))))
                            (< y (gpt-y (gbutton-pt-2 (third buttons)))))] (set-canvas 3))
      ))


    (define/override (on-event evt)
      (let ([evt-type (send evt get-event-type)]
            [evt-x (send evt get-x)]
            [evt-y (send evt get-y)])
        (cond
          [(eq? evt-type 'motion)
           (handle-mouse-motion evt-x evt-y)]
          [(eq? evt-type 'left-up)
            (begin
              (action evt-x evt-y)
              (printf "Click % (~a, ~a)~n" evt-x evt-y))])))

    (define/private (paint-game self dc)
      (send dc draw-bitmap bg 0 0)
      (send dc set-pen "blue" 1 'solid) (send dc set-brush "white" 'transparent) (cond
        [(not (eq? sel-button 'none))
         (let ([img-x (gpt-x sel-button-pt)]
               [img-y (gpt-y sel-button-pt)])
           (send dc draw-bitmap sel-button img-x img-y))]))

    (super-new (paint-callback (lambda (c dc) (paint-game c dc))))))

;-- howto canvas begins


(define howto-canvas%
  (class canvas%
    (inherit refresh)

    (define sel-button 'none)
    (define sel-button-pt 'none)

    (define manual (make-object bitmap% "img/MAN_HOVER.jpg"))
    (define back (make-object bitmap% "img/HBACK_HOVER.jpg"))

    (define buttons
      (list
       (gbutton (gpt 164 407) (gpt 231 482) manual (gpt 148 392))
       (gbutton (gpt 731 399) (gpt 822 471) back (gpt 713 380))))

    (define (handle-mouse-motion x y)
      (let loop ([bl buttons])
        (if (empty? bl)
           (refresh)
           (let* ([b (car bl)]
                 [pt-1 (gbutton-pt-1 b)]
                 [pt-1-x (gpt-x pt-1)]
                 [pt-1-y (gpt-y pt-1)]
                 [pt-2 (gbutton-pt-2 b)]
                 [pt-2-x (gpt-x pt-2)]
                 [pt-2-y (gpt-y pt-2)])
             (if (and (> x pt-1-x) (> y pt-1-y) (< x pt-2-x) (< y pt-2-y))
                 (begin
                   (set! sel-button (gbutton-img b))
                   (set! sel-button-pt (gbutton-img-pt b))
                   (refresh))
                 (begin
                   (set! sel-button 'none)
                   (set! sel-button-pt 'none)
                   (loop (cdr bl))))))))

    (define (action x y)
      (cond
        ([and (and (> x (gpt-x (gbutton-pt-1 (first buttons)))) 
             (< x (gpt-x (gbutton-pt-2 (first buttons)))))
              (and (> y (gpt-y (gbutton-pt-1 (first buttons))))
                    (< y (gpt-y (gbutton-pt-2 (first buttons)))))] (system "mupdf ./pdf/man.pdf"))
        ([and (and (> x (gpt-x (gbutton-pt-1 (second buttons)))) 
                     (< x (gpt-x (gbutton-pt-2 (second buttons)))))
                      (and (> y (gpt-y (gbutton-pt-1 (second buttons))))
                            (< y (gpt-y (gbutton-pt-2 (second buttons)))))] (set-canvas 4))))
                
    (define/override (on-event evt)
      (let ([evt-type (send evt get-event-type)]
            [evt-x (send evt get-x)]
            [evt-y (send evt get-y)])
        (cond
          [(eq? evt-type 'motion)
           (handle-mouse-motion evt-x evt-y)]
          [(eq? evt-type 'left-up)
            (begin
              (action evt-x evt-y)
              (printf "Click % (~a, ~a)~n" evt-x evt-y))])))

    (define/private (paint-game self dc)
      (send dc draw-bitmap bg-howto 0 0)
      (send dc set-pen "blue" 1 'solid) (send dc set-brush "white" 'transparent) (cond
        [(not (eq? sel-button 'none))
         (let ([img-x (gpt-x sel-button-pt)]
               [img-y (gpt-y sel-button-pt)])
           (send dc draw-bitmap sel-button img-x img-y))]))

    (super-new (paint-callback (lambda (c dc) (paint-game c dc))))))

;--score canvas begins here
(define score-canvas%
  (class canvas%
    (inherit refresh)

    (define sel-button 'none)
    (define sel-button-pt 'none)

    (define back (make-object bitmap% "img/SBACK_HOVER.jpg"))

    (define buttons
      (list
       (gbutton (gpt 731 399) (gpt 822 471) back (gpt 713 380))
       (gbutton (gpt 731 399) (gpt 822 471) back (gpt 713 380))))

    (define (handle-mouse-motion x y)
      (let loop ([bl buttons])
        (if (empty? bl)
           (refresh)
           (let* ([b (car bl)]
                 [pt-1 (gbutton-pt-1 b)]
                 [pt-1-x (gpt-x pt-1)]
                 [pt-1-y (gpt-y pt-1)]
                 [pt-2 (gbutton-pt-2 b)]
                 [pt-2-x (gpt-x pt-2)]
                 [pt-2-y (gpt-y pt-2)])
             (if (and (> x pt-1-x) (> y pt-1-y) (< x pt-2-x) (< y pt-2-y))
                 (begin
                   (set! sel-button (gbutton-img b))
                   (set! sel-button-pt (gbutton-img-pt b))
                   (refresh))
                 (begin
                   (set! sel-button 'none)
                   (set! sel-button-pt 'none)
                   (loop (cdr bl))))))))

    (define (action x y)
      (cond
        ([and (and (> x (gpt-x (gbutton-pt-1 (second buttons)))) 
                     (< x (gpt-x (gbutton-pt-2 (second buttons)))))
                      (and (> y (gpt-y (gbutton-pt-1 (second buttons))))
                            (< y (gpt-y (gbutton-pt-2 (second buttons)))))] (set-canvas 4))))
                
    (define/override (on-event evt)
      (let ([evt-type (send evt get-event-type)]
            [evt-x (send evt get-x)]
            [evt-y (send evt get-y)])
        (cond
          [(eq? evt-type 'motion)
           (handle-mouse-motion evt-x evt-y)]
          [(eq? evt-type 'left-up)
            (begin
              (action evt-x evt-y)
              (printf "Click % (~a, ~a)~n" evt-x evt-y))])))

    (define/private (paint-game self dc)
      (send dc draw-bitmap bg-scores 0 0)
      (send dc set-pen "blue" 1 'solid) (send dc set-brush "white" 'transparent) (cond
        [(not (eq? sel-button 'none))
         (let ([img-x (gpt-x sel-button-pt)]
               [img-y (gpt-y sel-button-pt)])
           (send dc draw-bitmap sel-button img-x img-y))]))

    (super-new (paint-callback (lambda (c dc) (paint-game c dc))))))

(define main-canvas (new menu-canvas% (parent frame)))

;-- changes canvas according to button number
(define (set-canvas opt)
  (case opt
    ((1) (begin (new score-canvas% (parent frame))
          (send frame delete-child (car (send frame get-children))) 
          (send frame show #t)))
    ((2) (begin (new loginf-canvas% (parent frame))
          (send frame delete-child (car (send frame get-children))) 
          (send frame show #t)))
    ((3) (begin (new howto-canvas% (parent frame))
          (send frame delete-child (car (send frame get-children))) 
          (send frame show #t)))
    ((4) (begin (new menu-canvas% (parent frame))
          (send frame delete-child (car (send frame get-children))) 
          (send frame show #t)))
  )
)

(send frame show #t)
