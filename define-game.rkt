#lang racket

(provide define-game)

(require racket/gui/base racket/draw)

(define game-canvas%
  (class canvas%
    (super-new)
    (inherit get-width get-height refresh get-dc refresh-now)
    (init-field user-on-tick user-on-key fps)

    (define frame-timer null)
    (define drawing-thread (thread (lambda ()
                                     (let loop ([delta (thread-receive)])
                                       (refresh-now (lambda (dc)
                                                      (user-on-tick dc delta))
                                                    #:flush? #t)
                                       (loop (thread-receive))))))
    (define frequency (quotient 1000 fps))
    (define canon (quotient 1000 60))
    (define last-tick (current-inexact-milliseconds))

    (define (on-frame)
      (let* ([now (current-inexact-milliseconds)]
             [delta (/ (- now last-tick) canon)])
        (set! last-tick now)
        (thread-send drawing-thread delta)))

    (define/override (on-char key)
      (user-on-key key))
    (define/public (start)
      (set! frame-timer (new timer%
                             [notify-callback on-frame]
                             [interval frequency]
                             [just-once? #f])))

    (define/public (stop)
      (send frame-timer stop))))

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
