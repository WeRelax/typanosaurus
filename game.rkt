#lang racket

(require "define-game.rkt" racket/draw)

;; state

(define-values (x y) (values 10 30))

(define *text* "the quick brown fox jumps over the lazy dog if (jarl === 12) { alert(\"jarl\"); }")
(define *text-segments* '())

(define (remainder)
  (second *text-segments*))

(define (typed)
  (first *text-segments*))

(define (init-game dc)
  (let ([font (make-font #:size 20 #:face "American Typewriter" #:smoothing 'smoothed)])
    (send dc set-font font))
  (set! *text-segments* `(() ,(string->list *text*))))

(define (draw-text dc)
  (send dc draw-text (list->string (remainder)) x y)
  (send dc draw-text (list->string (typed)) x (+ y 20)))

(define (check-input key)
  (let ([char (send key get-key-code)])
    (when (and (char? char)
               (char=? char (first (remainder))))
      (set! *text-segments* (list (append (typed) (list char))
                                  (cdr (remainder)))))))

(define stop (define-game
               init-game
               draw-text
               check-input
               (lambda () (empty?  (remainder)))))
