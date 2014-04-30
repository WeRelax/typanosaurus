#lang racket

;; config

(define *distance-target* 200)
(define *initial-water-supply* 6)
(define *thirst-delta* 3)
(define *water-delta* 1)

;; reset

(define (generate-initial-game-state)
  (list 0 *distance-target*))

(define (generate-initial-player-state)
  (list 0 *initial-water-supply*))

(define (generate-initial-state)
  (list (generate-initial-game-state)
        (generate-initial-player-state)))

;; I/O

(define (display-state state)
  (displayln state))

(define (get-user-input state)
  (define commands '((1 walk) (2 run) (3 drink)))
  (map (match-lambda [(list n c) (printf "~s: ~s\n" n c)])
       commands)
  (match (assoc (read) commands)
    [(list n command) command]
    [#f (begin (displayln "wrong!\n")
               (get-user-input state))]))

;; game logic

(define (game-running? state)
  #t)

(define (calculate-next-game-actions state input)
  (let ([game-state (car state)])
    (match input
      ;; pygmies, oasis, etc...
      [_ '()])))

(define (can-drink? state)
  (match-let ([(list _ (list _ water)) state])
    (>= water *water-delta*)))

(define (calculate-next-player-actions state input)
  (let ([player-state (cadr state)])
    (match input
      ['drink (if (can-drink? state) '([drink]) '([error "you don't have water!"]))]
      ['walk `([move ,(random 10) 1])]
      ['run `([move ,(+ 5 (random 10)) 2])]
      [_ '()])))

(define (calculate-next-actions state input)
  (let ([game-actions (calculate-next-game-actions state input)]
        [player-actions (calculate-next-player-actions state input)])
    (append game-actions player-actions)))

;; alter game state

(define (inc-miles game-state miles)
  (match-let ([(list walked target) game-state])
    (list (+ walked miles) target)))

(define (inc-thirst player-state delta)
  (match-let ([(list thirst water) player-state])
    (list (+ thirst delta) water)))

(define (move state speed thirst-delta)
  (match-let ([(list game-state player-state) state])
    (list (inc-miles game-state speed)
          (inc-thirst player-state thirst-delta))))

(define (drink state)
  (match-let ([(list game-state player-state) state])
    (let ([thirst (car player-state)]
          [water (cadr player-state)])
      (list game-state
            (list (max 0 (- thirst *thirst-delta*))
                  (- water *water-delta*))))))

(define (game-error state msg)
  (printf "\n** ~s\n\n" msg)
  state)

(define (process-action action state)
  (match action
    [(list 'drink) (drink state)]
    [(list 'move speed thirst) (move state speed thirst)]
    [(list 'error msg) (game-error state msg)]))

(define (alter-state state actions)
  (match actions
    [(? empty?) state]
    [(cons action rest) (alter-state
                              (process-action action state)
                              rest)]))

;; satate := (list game-state player-state [active-things ...])
;; game-state := (list walked-distance target-distance)
;; player-state := (list thirst turns cheto-coefficient)
;; active-thing := pigmy-state | event-oasis | event-storm | event-insolation | ...

(define (main-loop)
  (let loop ([state (generate-initial-state)])
    (when (game-running? state)
      (display-state state)
      (let ([input (get-user-input state)])
        (loop (alter-state state
                           (calculate-next-actions state input)))))))
