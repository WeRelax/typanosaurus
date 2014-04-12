#lang racket

(provide define-game)

(require racket/gui/base racket/draw)

(define game-canvas%
  (class canvas%
    (super-new)
    (inherit get-width get-height refresh get-dc refresh-now)
    (init-field user-on-tick user-on-key fps)

    (define frame-timer null)
    (define frequency (quotient 1000 fps))

    (define (on-frame)
      (refresh-now user-on-tick #:flush? #t)
      (set-timer))

    (define/override (on-char key)
      (user-on-key key))

    (define/private (set-timer)
      (set! frame-timer (new timer%
                             [notify-callback on-frame]
                             [interval frequency]
                             [just-once? #t])))

    (define/public (start)
      (set-timer))

    ))

(define (make-frame)
  (new frame%
       [label "Test"]
       [width 500]
       [height 500]))

(define (make-canvas frame on-tick on-key fps)
  (new game-canvas%
       [parent frame]
       [fps fps]
       [user-on-tick on-tick]
       [user-on-key (or on-key
                        (lambda () #f))]))

(define (show-window user-init on-tick on-key [fps 60])
  (define cust (make-custodian))
  (define frame (make-frame))
  (parameterize ([current-custodian cust])
    (thread (lambda ()
              (let* ([canvas (make-canvas frame on-tick on-key fps)]
                     [dc (send canvas get-dc)])
                (user-init dc)
                (send frame show #t)
                (send canvas start)))))
  (lambda ()
    (custodian-shutdown-all cust)
    (send frame show #f)))

(define-syntax-rule (define-game user-init on-tick on-key exitp)
  (letrec ([stop (show-window
                  user-init
                  (lambda (dc) (if (exitp)
                                   (stop)
                                   (on-tick dc)))
                  on-key
                  25)])
    stop))
