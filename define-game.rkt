#lang racket

(provide define-game)

(require racket/gui/base racket/draw)

(define loop%
  (class object%
    (super-new)

    (init-field interval [callback void])

    (define canon-ms (quotient 1000 60))
    (define loop void)

    (define (tick old-time)
      (let ([new-time (current-inexact-milliseconds)])
        (yield)
        (if (> (- new-time old-time) interval)
            (begin
              (callback (/ (- new-time old-time) canon-ms))
              (queue-callback (thunk (loop new-time)) #t))
            (queue-callback (thunk (loop old-time)) #t))))

    (define/public (start)
      (set! loop tick)
      (loop (current-inexact-milliseconds)))

    (define/public (stop)
      (set! loop void))

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
      ;; I think this thread is not useful
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
          #:fps [fps 60]
          #:init user-init
          #:on-frame on-tick
          #:on-key on-key
          #:exitp exitp)
  (letrec ([stop (show-window w h title fps
                              user-init
                              (lambda (dc delta) (if (exitp) (stop) (on-tick dc delta)))
                              on-key)])
    stop))
