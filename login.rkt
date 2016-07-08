#lang racket/gui

(provide loginf-canvas%)
(require 2htdp/image)
(require "./frame.rkt")
(require "./game.rkt")
(require "./win.rkt")
(require "./libs/pdata.rkt")
(require "./ARCHIVOS/usercheck.rkt")

(define bg-loginf (make-object bitmap% "img/BOARD_USER1.jpg"))
(define bg-logins (make-object bitmap% "img/BOARD_USER2.jpg"))
(define username '())
(define password '())
(define passwordshow '())
(define players '())
(define active-field 0)

(struct gpt (x y))
(struct gbutton (pt-1 pt-2 img img-pt))
 
(define (clearfields)
  (set! username '()) 
  (set! password '())
  (set! passwordshow '())
)

;--loginf canvas begins here
(define loginf-canvas%
  (class canvas%
    (inherit refresh)

    (define sel-button 'none)
    (define sel-button-pt 'none)

    (define back (make-object bitmap% "img/OK_HOVER.jpg"))
    (define textfield (make-object bitmap% "img/OK_HOVERx.jpg"))

    (define buttons
      (list
       (gbutton (gpt 427 327) (gpt 524 383) back (gpt 416 317))
       (gbutton (gpt 508 205) (gpt 641 234) textfield (gpt 508 205))
       (gbutton (gpt 508 261) (gpt 641 290) textfield (gpt 508 261))))

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
                            (< y (gpt-y (gbutton-pt-2 (first buttons)))))] 
                   (begin 
                      (if (checkuser (list->string username) (list->string password)) (begin (writePlayer (list->string username)) (clearfields) (set-canvas 1)) (clearfields))))

        ([and (and (> x (gpt-x (gbutton-pt-1 (cadr buttons)))) 
                     (< x (gpt-x (gbutton-pt-2 (cadr buttons)))))
                      (and (> y (gpt-y (gbutton-pt-1 (cadr buttons))))
                            (< y (gpt-y (gbutton-pt-2 (cadr buttons)))))] (set! active-field 1))
        ([and (and (> x (gpt-x (gbutton-pt-1 (cadr buttons)))) 
                     (< x (gpt-x (gbutton-pt-2 (caddr buttons)))))
                      (and (> y (gpt-y (gbutton-pt-1 (caddr buttons))))
                            (< y (gpt-y (gbutton-pt-2 (caddr buttons)))))] (set! active-field 2))
        (else (set! active-field 0))))

    (define (key-action key)
      (cond 
        ((eq? active-field 1)  
             (if (not (equal? key 'release)) 
                (begin
                  (case key
                    ((#\backspace) (if (> (length username ) 0) (set! username (reverse (cdr (reverse username)))) (void)))
                    ((#\return) (set! active-field 0))
                    ((#\tab) (set! active-field 2))
                    (else (if (and (char? key) (not (equal? key #\,)) (not (equal? key #\/)) 
                    (< (length username) 10)) (set! username (append username (list key))) (void)))
                  )
                  (refresh))
                (void)))
        ((eq? active-field 2) 
             (if (not (equal? key 'release)) 
                (begin
                  (case key
                    ((#\backspace) (if (> (length password ) 0) (begin (set! password (reverse (cdr (reverse password)))) (set! passwordshow (reverse (cdr (reverse passwordshow))))) (void)))
                    ((#\return) (set! active-field 0))
                    ((#\tab) (set! active-field 1))
                    (else (if (and (char? key) (not (equal? key #\,)) (not (equal? key #\/)) 
                    (< (length password) 15)) (begin (set! passwordshow (append passwordshow (list #\*))) (set! password (append password (list key)))) (void)))
                  )
                  (refresh))
                (void)))
        (else (display "no field selected\n")))
    )
                
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

    (define/override (on-char evt)
      (let ([char (send evt get-key-code)])
        (key-action char)))

    (define/private (paint-game self dc)
      (send dc draw-bitmap bg-loginf 0 0)
      (send dc draw-text (list->string username) 508 205)
      (send dc draw-text (list->string passwordshow) 508 261)
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
    (define textfield (make-object bitmap% "img/OK_HOVERx.jpg"))

    (define buttons
      (list
       (gbutton (gpt 427 327) (gpt 524 383) back (gpt 416 317))
       (gbutton (gpt 508 205) (gpt 641 234) textfield (gpt 508 205))
       (gbutton (gpt 508 261) (gpt 641 290) textfield (gpt 508 261))))

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
                            (< y (gpt-y (gbutton-pt-2 (first buttons)))))] 
                   (begin 
                      (if (checkuser (list->string username) (list->string password)) (begin (write2Player (list->string username)) (clearfields) (set-canvas 2)) (clearfields))))
        ([and (and (> x (gpt-x (gbutton-pt-1 (cadr buttons)))) 
                     (< x (gpt-x (gbutton-pt-2 (cadr buttons)))))
                      (and (> y (gpt-y (gbutton-pt-1 (cadr buttons))))
                            (< y (gpt-y (gbutton-pt-2 (cadr buttons)))))] (set! active-field 1))
        ([and (and (> x (gpt-x (gbutton-pt-1 (cadr buttons)))) 
                     (< x (gpt-x (gbutton-pt-2 (caddr buttons)))))
                      (and (> y (gpt-y (gbutton-pt-1 (caddr buttons))))
                            (< y (gpt-y (gbutton-pt-2 (caddr buttons)))))] (set! active-field 2))
        (else (set! active-field 0))))

    (define (key-action key)
      (cond 
        ((eq? active-field 1)  
             (if (not (equal? key 'release)) 
                (begin
                  (case key
                    ((#\backspace) (if (> (length username ) 0) (set! username (reverse (cdr (reverse username)))) (void)))
                    ((#\return) (set! active-field 0))
                    ((#\tab) (set! active-field 2))
                    (else (if (and (char? key) (not (equal? key #\,)) (not (equal? key #\/)) (< (length username) 10)) (set! username (append username (list key))) (void)))
                  )
                  (refresh))
                (void)))
        ((eq? active-field 2) 
             (if (not (equal? key 'release)) 
                (begin
                  (case key
                    ((#\backspace) (if (> (length password ) 0) (begin (set! password (reverse (cdr (reverse password)))) 
                      (set! passwordshow (reverse (cdr (reverse passwordshow))))) (void)))
                    ((#\return) (set! active-field 0))
                    ((#\tab) (set! active-field 1))
                    (else (if (and (char? key) (not (equal? key #\,)) (not (equal? key #\/)) (< (length password) 15)) (begin (set! passwordshow 
                        (append passwordshow (list #\*))) (set! password (append password (list key)))) (void)))
                  )
                  (refresh))
                (void)))
        (else (display "no field selected\n")))
    )
                
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

    (define/override (on-char evt)
      (let ([char (send evt get-key-code)])
         (key-action char)))

    (define/private (paint-game self dc)
      (send dc draw-bitmap bg-logins 0 0)
      (send dc draw-text (list->string username) 508 205)
      (send dc draw-text (list->string passwordshow) 508 261)
      (send dc set-pen "blue" 1 'solid) (send dc set-brush "white" 'transparent) (cond
        [(not (eq? sel-button 'none))
         (let ([img-x (gpt-x sel-button-pt)]
               [img-y (gpt-y sel-button-pt)])
           (send dc draw-bitmap sel-button img-x img-y))]))

    (super-new (paint-callback (lambda (c dc) (paint-game c dc))))))

;-- changes canvas according to button number
(define (set-canvas opt)
  (case opt
    ((1) (begin (display "P2\n")(new logins-canvas% (parent mainframe))
          (send mainframe delete-child (car (send mainframe get-children))) 
          (send mainframe show #t)))
    ((2) (begin (display "GAME\n") (new game-canvas% (parent mainframe))
          (send mainframe delete-child (car (send mainframe get-children))) 
          (send mainframe show #t)))
  )
)

;(define main-canvas (new menu-canvas% (parent mainframe)))
