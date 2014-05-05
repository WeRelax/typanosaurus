#lang racket

;; config

(define *distance-target* 200)
(define *initial-water-supply* 6)
(define *thirst-delta* 3)
(define *thirst-threshold* 5)
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
  (printf "\n")
  (displayln state)
  (printf "\n"))

(define (display-actions actions)
  (map printf
       (map (lambda (action)
              (match action
                [(cons 'oasis _) "\nYou find a refreshing oasis!\n\n"]
                [(cons 'thieves _) "\nSome thieves stole your water...\n\n"]
                [(cons 'drink _) "\nYou take a little sip of water.\n\n"]
                [(list 'move _ 1) "\nSlowly and ahead we go.\n\n"]
                [(list 'move _ 2) "\nGo, go, go!!\n\n"]
                [_ ""]))
            actions)))

(define (get-user-input state)
  (define commands '((1 walk) (2 run) (3 drink)))
  (map (match-lambda [(list n c) (printf "~s: ~s\n" n c)])
       commands)
  (match (assoc (read) commands)
    [(list n command) command]
    [#f (begin (displayln "wrong!\n")
               (get-user-input state))]))

;; game logic

(define (game-end condition)
  (printf "\n\n *** \n\n")
  (match condition
    ['win (printf "Congrats! You won!!")]
    ['loose (printf "Your camel and you died miserably...")])
  (printf "\n\n *** \n\n")
  #f)

(define (game-running? state)
  (match-let ([(list
                (list walked target)
                (list thirst _)) state])
    (cond
     [(>= walked target) (game-end 'win)]
     [(>= thirst *thirst-threshold*) (game-end 'loose)]
     [#t #t])))

(define (calculate-next-game-actions state input)
  (let ([game-state (car state)])
    '()))

(define (calculante-random-encounters)
  (let ([dice (random 100)])
    (cond
     [(< dice 10) `([oasis ,(random 20)])]
     [(< dice 5) `([thieves ,(random 10)])]
     [#t '()])))

(define (can-drink? state)
  (match-let ([(list _ (list _ water)) state])
    (>= water *water-delta*)))

(define (calculate-next-player-actions state input)
  (let ([player-state (cadr state)])
    (match input
      ['drink (if (can-drink? state) '([drink]) '([error "you don't have water!"]))]
      ['walk `([move ,(random 10) 1] ,@(calculante-random-encounters))]
      ['run `([move ,(+ 5 (random 10)) 2])]
      [_ '()])))

(define (calculate-next-actions state input)
  (let ([game-actions (calculate-next-game-actions state input)]
        [player-actions (calculate-next-player-actions state input)])
    (append game-actions player-actions)))

;; alter game state

(define (game-error state msg)
  (printf "\n** ~s\n\n" msg)
  state)

(define (process-global-action action state)
  (match action
    [(list 'error msg) (game-error state msg)]
    [_ state]))

(define (process-distance distance action)
  (match action
    [(list 'move speed _) (+ distance speed)]
    [_ distance]))

(define (process-target target action)
  target)

(define (process-thirst thirst action)
  (match action
    [(list 'move _ thirst-delta) (+ thirst-delta thirst)]
    [(list 'drink) (max 0 (- thirst *thirst-delta*))]
    [_ thirst]))

(define (process-water water action)
  (match action
    [(list 'drink) (- water *water-delta*)]
    [(list 'oasis n) (+ water n)]
    [(list 'thieves n) (- water n)]
    [_ water]))

(define (process-action action state)
  (let ([state (process-global-action action state)])
    (match-let ([(list
                  (list distance target)
                  (list thirst water)) state])
      (list
       (list (process-distance distance action) (process-target target action))
       (list (process-thirst thirst action) (process-water water action))))))

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
      (let* ([input (get-user-input state)]
             [actions (calculate-next-actions state input)])
        (display-actions actions)
        (loop (alter-state state actions))))))
