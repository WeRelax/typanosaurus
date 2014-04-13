#lang racket

(require "define-game.rkt" racket/draw)

;; ;; state

;; (define-values (x y) (values 10 30))

;; (define *text* "the quick brown fox jumps over the lazy dog if (jarl === 12) { alert(\"jarl\"); }")
;; (define *text-segments* '())

;; (define (remainder)
;;   (second *text-segments*))

;; (define (typed)
;;   (first *text-segments*))

;; ;; game logic

;; (define (init-game dc)
;;   (let ([font (make-font #:size 20 #:face "American Typewriter" #:smoothing 'smoothed)])
;;     (send dc set-font font))
;;   (set! *text-segments* `(() ,(string->list *text*))))

;; (define (draw-text dc)
;;   (send dc draw-text (list->string (remainder)) x y)
;;   (send dc draw-text (list->string (typed)) x (+ y 20)))

;; (define (check-input key)
;;   (let ([char (send key get-key-code)])
;;     (when (and (char? char)
;;                (char=? char (first (remainder))))
;;       (set! *text-segments* (list (append (typed) (list char))
;;                                   (cdr (remainder)))))))

;; --- Type Commando ---

;; state

(define *rays* '())

(struct point (x y))
(struct vector (direction magnitude))

(struct ray (origin speed position) #:mutable)

;; logic

(define (make-ray)
  (let ([x (+ 50 (random 700))])
    (ray
     (point x 200)
     (vector
      (degrees->radians 90)
      1)
     ;;(+ 0.005 (random 5)))
     (point x 0))))

(define (add-ray)
  (let ([ray (make-ray)])
    (set! *rays* (cons ray *rays*))))

(define (move-ray ray delta)
  (let* ([orig (ray-origin ray)]
         [speed (ray-speed ray)]
         [pos (ray-position ray)]
         [dest (point (+ (point-x pos)
                         (* (vector-magnitude speed)
                            (cos (vector-direction speed))
                            delta))
                      (+ (point-y pos)
                         (* (vector-magnitude speed)
                            (sin (vector-direction speed))
                            delta)))])
    (set-ray-position! ray dest)
    (set-ray-speed! ray (vector
                         (+ (vector-direction speed) 0.01)
                         (vector-magnitude speed)))))

;; drawing

(define (draw-ray dc ray)
  (send dc set-pen (make-pen #:color (make-color 255 0 0)
                              #:width 3
                              #:style 'solid))
  (let ([orig (ray-origin ray)]
        [dest (ray-position ray)])
    (send dc draw-line
          (point-x orig)
          (point-y orig)
          (point-x dest)
          (point-y dest))))

;; game control

(define (init-game dc)
  (set! *rays* '())
  (for ([i (range 1 10)])
    (add-ray))
  (send dc set-background (make-color 0 0 0))
  (send dc set-text-foreground (make-color 255 255 255)))

(define (draw-frame dc delta)
  (send dc clear)
  (send dc draw-text (number->string delta) 10 10)
  (for-each (curryr move-ray delta) *rays*)
  (for-each (curry draw-ray dc) *rays*))

(define (on-input key)
  key)

(define (quit?)
  #f)

;; (stop)

(define stop (define-game
               #:width 800
               #:height 600
               #:fps 100
               #:title "Type ^ Command"
               #:init init-game
               #:on-frame draw-frame
               #:on-key on-input
               #:exitp quit?))

;; *** P L A N ***

;; 1. Create the falling missiles
;; 2. Add a box with random words to the missiles
;; 3. Destroy them typing

;; --- Checkpoint 1

;; 4. Big exposion!!
;; 5. Add cities and count lives and scores
;; 6. Game over / you win

;; --- Checkpoint 2

;; 7. Initial menu (simple, just "type this to start")
;; 8. Fullscreen!!
;; 9. Levels

;; --- Checkpoint 3

;; 10. Add effects to the typing (colors, fades, size, etc..)
