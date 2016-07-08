#lang racket/gui

(require "./frame.rkt")
(require "./matriz.rkt")
(require "./movement.rkt")
(require "./libs/pdata.rkt")
(require "./win.rkt")
(provide game-canvas%)

(define bg (make-object bitmap% "img/BOARD.jpg"))
(define br (read-bitmap "img/bright_token.png"))
(define timg (read-bitmap "img/turn.png"))
(define turn 1)

(struct gpt (x y))
(struct gbutton (pt-1 pt-2 img img-pt) #:mutable)

(define (createButtonR L x y cont difx dify)
  (if (< cont 9)
    (createButtonR 
      (append L (list (gbutton (gpt (+ x difx) (+ y dify)) (gpt (+ x 38 difx) (+ y 24 dify)) br (gpt (- x 10) (- y 10)))))
      x y (+ cont 1) (+ difx 38) (+ dify 24))
    L
  )
)

(define (createButtonC M x y cont difx dify)
  (if (> 9 cont)
    (createButtonC
      (append M (list (createButtonR '() (- x difx) (+ y dify) 0 0 0)))
      x y (+ cont 1) (+ difx 37) (+ dify 23))
    M
  )
)

;; fucntion that creates a matrix with the
;; positions in which the chips will be placed
(define (createGptPosR L x y cont difx dify)
  (if (> 9 cont)
    (createGptPosR
      (append L (list (gpt (+ x difx) (+ y dify))))
      x y (+ cont 1) (+ difx 38) (+ dify 24))
    L
  )
)

(define (createButtonM)
  (createButtonC '() 454 98 0 0 0)
)

;; fucntion that creates a matrix with the
;; positions in which the chips will be placed
(define (createGptPosC M x y cont difx dify)
  (if (> 9 cont)
    (createGptPosC
      (append M (list (createGptPosR '() (- x difx) (+ y dify) 0 0 0)))
      x y (+ cont 1) (+ difx 37) (+ dify 23))
    M
  )
)

;; fucntion that creates a matrix with the
;; positions in which the chips will be placed
(define (createGptM)
  (createGptPosC '() 454 98 0 0 0)
)


;; SOME IMPORTANT GLOBAL DEFINITIONS
(define gm (initGameMatrix))
(define gptmx (createGptM))
(define buttons (createButtonM))
(define mvmntlist '())
(define titor (gbutton (gpt 0 0) (gpt 0 0) timg (gpt 71 393)))

(define (find-xyAux M x y cx cy)
  (define btn (list-ref (list-ref M cy) cx))
  (if (and (> x (gpt-x (gbutton-pt-1 btn))) 
           (> y (gpt-y (gbutton-pt-1 btn)))
           (< x (gpt-x (gbutton-pt-2 btn)))
           (< y (gpt-y (gbutton-pt-2 btn))))
    #t
    #f))

(define (find-xy x y cx cy)
  (if (and (< cx 9) (< cy 9))
    (if (find-xyAux buttons x y cx cy)
      (list cx cy)
      (find-xy x y (+ cx 1) cy)
    )
    (if (< cy 9)
      (find-xy x y 0 (+ cy 1))
      '()
    )
  )
)



;; STARTS GAME CANVAS
(define game-canvas%
  (class canvas%
    (inherit refresh)

    (define sel-button 'none)
    (define sel-button-pt 'none)
    (define active-players (shuffle (readPlayers)))

    (define howto (make-object bitmap% "img/howto.jpg"))
    (define red (read-bitmap "img/red_token.png"))
    (define blue (read-bitmap "img/blue_token.png"))

    (define (handle-mouse-motion x y)
      (map (lambda (bm)
        (let loop ([bl bm])
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
                     (loop (cdr bl)))))))) buttons))

    (define (checkList L)
      (if (or (empty? (car L)) (empty? (cadr L)))
        #f
        #t
      )
    )

    (define (trans-move L)
      (define mo (list
        (find-xy (car L) (cadr L) 0 0)
        (find-xy (caddr L) (cadddr L) 0 0)))
      (display mo)
      (display turn)(newline)
      (if (and (checkList mo) (<= turn 2) (move gm (caar mo) (cadar mo) (caadr mo) (cadadr mo) turn))
        (if (equal? turn 2)
          (begin
            (set! turn 1)
            (set-gbutton-img-pt! titor (gpt 71 393))
            (refresh)
          )
          (begin
            (set! turn (+ turn 1))
            (set-gbutton-img-pt! titor (gpt 710 393))
            (refresh)
          )
        )
        (refresh)
      )
      (set! mvmntlist '())
      ;; display changes and display game matrix
      (newline)
      (map (lambda (x) (display x)(newline)) (vector->list gm))
    )

    (define (action x y)
      (if (< (length mvmntlist) 2)
        (set! mvmntlist (append mvmntlist (list x y)))
        (begin
          (set! mvmntlist (append mvmntlist (list x y)))
          (trans-move mvmntlist)
        )
      )
    )

;;; dibujara las fichas
    (define (draw-chAux dc gm gptm x y)
      ;;chip-position = cp is being obtained from gptmatrix
      (define cp (getPat gptm x y))
      (case (getVat gm x y)
        ((1) (send dc draw-bitmap red (gpt-x cp) (gpt-y cp)))
        ((2) (send dc draw-bitmap blue (gpt-x cp) (gpt-y cp)))
        ((0) (void))))

    (define (draw-ch dc gm gptm x y)
      (if (< x 9)
        (begin
          (draw-chAux dc gm gptm x y)
          (draw-ch dc gm gptm (+ x 1) y)
        )
        (if (< y 9)
          (draw-ch dc gm gptm 0 (+ y 1))
          (void))))

    (define/override (on-event evt)
      (let ([evt-type (send evt get-event-type)]
            [evt-x (send evt get-x)]
            [evt-y (send evt get-y)])
        (cond
          [(eq? evt-type 'motion)
          (case (wincondition gm)
            ((1) (begin (writeWinner (list-ref active-players 0)) (set-canvas 1)))
            ((2) (begin (writeWinner (list-ref active-players 1)) (set-canvas 1)))
            ((0) (void)))]
          [(eq? evt-type 'left-up)
            (begin
              (action evt-x evt-y)
              (printf "Click % (~a, ~a)~n" evt-x evt-y))])))

    (define/private (paint-game self dc)
      (send dc draw-bitmap bg 0 0)
      (send dc draw-text (car active-players) 87 476)
      (send dc draw-text (cadr active-players) 734 476)
      (send dc set-pen "blue" 1 'solid) (send dc set-brush "white" 'transparent) (cond
        [(not (eq? sel-button 'none))
         (let ([img-x (gpt-x sel-button-pt)]
               [img-y (gpt-y sel-button-pt)])
           (send dc draw-bitmap sel-button img-x img-y))])
         (draw-ch dc gm gptmx 0 0)
         (send dc draw-bitmap timg (gpt-x (gbutton-img-pt titor)) (gpt-y (gbutton-img-pt titor)))
        )

    (super-new (paint-callback (lambda (c dc) (paint-game c dc))))))

;-- changes canvas according to button number
(define (set-canvas opt)
  (case opt
    ((1) (begin (new win-canvas% (parent mainframe))
          (send mainframe delete-child (car (send mainframe get-children))) 
          (send mainframe show #t)))
  )
)
