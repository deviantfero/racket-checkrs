#lang racket/gui

(provide loginf-canvas%)
(require "./frame.rkt")

(define bg-loginf (make-object bitmap% "img/BOARD_USER.jpg"))
(define bg-logins (make-object bitmap% "img/BOARD_USER.jpg"))
(struct gpt (x y))
(struct gbutton (pt-1 pt-2 img img-pt))

;--loginf canvas begins here
(define loginf-canvas%
  (class canvas%
    (inherit refresh)

    (define sel-button 'none)
    (define sel-button-pt 'none)

    (define container (new pane%
      [parent frame] [spacing 40]
      [border 50]))

    (define username-text (new text-field% 
      [label #f] [parent container]))

    (define back (make-object bitmap% "img/OK_HOVER.jpg"))

    (define buttons
      (list
       (gbutton (gpt 427 327) (gpt 524 383) back (gpt 416 317))
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
                            (< y (gpt-y (gbutton-pt-2 (first buttons)))))] (set-canvas 1))))
                
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
      (send dc draw-bitmap bg-loginf 0 0)
      (send dc set-pen "blue" 1 'solid) (send dc set-brush "white" 'transparent) (cond
        [(not (eq? sel-button 'none))
         (let ([img-x (gpt-x sel-button-pt)]
               [img-y (gpt-y sel-button-pt)])
           (send dc draw-bitmap sel-button img-x img-y))]))

    (super-new (paint-callback (lambda (c dc) (paint-game c dc))))))

;-- login for second player (not actually second player but still)

(define logins-canvas%
  (class canvas%
    (inherit refresh)

    (define sel-button 'none)
    (define sel-button-pt 'none)

    (define back (make-object bitmap% "img/OK_HOVER.jpg"))

    (define buttons
      (list
       (gbutton (gpt 427 327) (gpt 524 383) back (gpt 416 317))
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
                            (< y (gpt-y (gbutton-pt-2 (first buttons)))))] (set-canvas 2))))
                
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
      (send dc draw-bitmap bg-logins 0 0)
      (send dc set-pen "blue" 1 'solid) (send dc set-brush "white" 'transparent) (cond
        [(not (eq? sel-button 'none))
         (let ([img-x (gpt-x sel-button-pt)]
               [img-y (gpt-y sel-button-pt)])
           (send dc draw-bitmap sel-button img-x img-y))]))

    (super-new (paint-callback (lambda (c dc) (paint-game c dc))))))

;-- changes canvas according to button number
(define (set-canvas opt)
  (case opt
    ((1) (begin (display "P1\n")(new logins-canvas% (parent frame))
          (send frame delete-child (car (send frame get-children))) 
          (send frame show #t)))
    ((2) (begin (display "P2\n") (new loginf-canvas% (parent frame))
          (send frame delete-child (car (send frame get-children))) 
          (send frame show #t)))
  )
)

;(define main-canvas (new menu-canvas% (parent frame)))
