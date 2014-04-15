#lang racket

(provide define-game)

(require racket/gui/base racket/draw)

(define loop%
  (class object%
    (super-new)

    (init-field interval [callback void])

    (define timer void)
    (define again void)
    (define last-stamp (- (current-inexact-milliseconds)
                          interval))

    (define (timer-tick)
      (let* ([now (current-inexact-milliseconds)]
             [diff (- now last-stamp)]
             [delta (/ diff interval)])
        (set! last-stamp now)
        (yield)
        (callback delta)
        (again now)))

    (define (prepare-next-tick tick-time)
      (let* ([next-tick-time (+ tick-time interval)]
             [now (current-inexact-milliseconds)]
             [sleep-time (max 0 (floor (- next-tick-time now)))])
        (send timer start (inexact->exact sleep-time) #t)))

    (define/public (start)
      (set! again prepare-next-tick)
      (set! timer (new timer%
                       [notify-callback timer-tick]
                       [interval interval]
                       [just-once? #t])))

    (define/public (stop)
      (set! again void))

    (define/public (set-interval! new-interval)
      (set! interval new-interval))))

(define game-canvas%
  (class canvas%
    (super-new)
    (inherit get-dc refresh-now)
    (init-field user-on-tick user-on-key fps)

    (define frequency (quotient 1000 fps))
    (define loop void)

    (define/override (on-char key)
      (user-on-key key))

    (define/public (start)
      (set! loop (new loop%
                      [interval frequency]
                      [callback  (lambda (delta)
                                   (refresh-now (curryr user-on-tick delta)
                                                #:flush? #f))]))
      (send loop start))

    (define/public (stop)
      (send loop stop))))

(define (make-frame w h title)
  (new frame%
       [label title]
       [width w]
       [height h]))

(define (make-canvas frame on-tick on-key fps)
  (new game-canvas%
       [parent frame]
       [fps fps]
       [user-on-tick on-tick]
       [user-on-key (or on-key
                        (lambda () #f))]))

(define (show-window w h title fps user-init on-tick on-key)
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
    (parameterize ([current-eventspace (make-eventspace)])
      (define frame (make-frame w h title))
      (define canvas (make-canvas frame on-tick on-key fps))
      (let* ([dc (send canvas get-dc)])
        (user-init dc)
        (send frame show #t)
        (send canvas start))
      (lambda ()
        (send canvas stop)
        (custodian-shutdown-all cust)
        (send frame show #f)))))

(define (define-game
          #:width [w 800]
          #:height [h 600]
          #:title [title "*"]
          #:fullscreen? [full? #f]
          #:fps [fps 30]
          #:init user-init
          #:on-frame on-tick
          #:on-key on-key
          #:exitp exitp)
  (letrec ([stop (show-window w h title fps
                              user-init
                              (lambda (dc delta) (if (exitp) (stop) (on-tick dc delta)))
                              on-key)])
    stop))
