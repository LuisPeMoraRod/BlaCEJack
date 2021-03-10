#lang racket
(require racket/gui
         racket/draw
         racket/string
         )

(define *players* null)
(define *x_croupier* 360)

;************************* Welcome Window *************************
#|Define frame for the welcome window, where the name(s) of the player(s) are requested|#
(define welcome-window
  (new frame%
       [label "BlaCEJack"]
       [style (list 'no-resize-border)]
       [alignment (list 'center 'center)]
       [stretchable-width #f]
       [stretchable-height #f]))

#|Define info static message|#
(define mssg 
  (new message%
       [parent welcome-window]
       [label "Insert the username of the player(s). If the amount of players\nis less than 3, leave the text input in blank."]
       [vert-margin 10]
       [horiz-margin 10]))


#|Define text fields for the name of the players (3 is the maximum possible amount)|#
(define name-input-1 
  (new text-field%
       [label "Player 1:"]
       [parent welcome-window]
       [vert-margin 3]
       [horiz-margin 10]))

(define name-input-2 
  (new text-field%
       [label "Player 2:"]
       [parent welcome-window]
       [vert-margin 3]
       [horiz-margin 10]))

(define name-input-3 
  (new text-field%
       [label "Player 3:"]
       [parent welcome-window]
       [vert-margin 3]
       [horiz-margin 10]))

#|Define the button that starts the game with the given names|#
(define play-button 
  (new button%
       [parent welcome-window]
       [label "Play!"]
       [vert-margin 10]
       [horiz-margin 10]
       [callback (lambda (button event)
            (start-game (create-players-list (list (send name-input-1 get-value) 
            (send name-input-2 get-value) (send name-input-3 get-value)))))])); creates list of sublists with the name of each player


;************************* Main Game Window *************************

#|Define main window that hosts the game dinamic|#
(define game-window
  (new frame%
       [label "BlaCEJack"]
       [width 800]
       [height 540]
       [style (list 'no-resize-border)]
       [alignment (list 'left 'center)]
       ))

#|Define label that shows the player who has the current turn|#
(define turn-mssg 
(new message%
       [parent game-window]
       [label "Press 'Begin' button to start the game..."]
       [vert-margin 2]
       [horiz-margin 10]))

#|Deletes null lists from a list
@param lst : list with sublists that contain the names received from text inputs (may contain empty sublists)
@return lst : list without empty elements|#
(define (create-players-list lst)
  (cond ((null? lst) '() )
        ((equal? (car lst) "") (create-players-list (cdr lst)))
        (else (cons (list (car lst) '() #t) (create-players-list (cdr lst))))))

#|Get the name of the player with the current turn
@param players : list with the names and list of cards of the players
@return name of the player with the current turn|#
(define (get-current-turn players)
  (caar players))

#|Dequeues and enqueues the last player that had the turn
@param players : list with the names and list of cards of the players
@return updated players queue|#
(define (update-players-queue players)
  (append(cdr players) (list (car players))))

#|Sets the name of the player who has the current turn in the turn-mssg label
@param name : string|#
(define (set-turn-mssg name) 
  {send turn-mssg set-label (string-join (list "It's your turn: " name))})

#|Creates the main canvas where the cards will be displayed
@param frame : frame%
@param table-bitmap : bitmap%|#
(define (main-canvas frame table-bitmap card-bitmap)
  (new canvas% 
    [parent frame]
    [min-height 500]
    [paint-callback
      (lambda (canvas drawing-context)
        (send drawing-context draw-bitmap table-bitmap 0 0)
        (send drawing-context draw-bitmap card-bitmap 100 50)
        (send drawing-context draw-bitmap card-bitmap 104 54)
        (send drawing-context draw-bitmap card-bitmap 108 58))])
  )

#|Creates horizontal panel where the "Stand" and "New Card" buttons will be placed
@param parent-frame|#
(define (create-horiz-panel parent-frame)
  (new horizontal-panel% 
      [parent parent-frame]
      [alignment (list 'right 'center)]))

#|Creates button to begin game. It disables after being clicked.
@param horiz-panel : horizontal-panel%
@param parent-frame : frame%
@param my-dc : bitmap-dc%
@param amount-players : int
|#
(define (begin-button horiz-panel parent-frame my-dc amount-players)
(new button% [label "Begin"]
      [parent horiz-panel]
      [vert-margin 5]
      [horiz-margin 5]
      [callback
      (lambda (button event)
        ;(send my-dc clear)
        (set-turn-mssg (get-current-turn *players*))
        (send my-dc draw-bitmap (read-bitmap "resources/cards/cardBack_red4.png") *x_croupier* 30)
        (send my-dc draw-bitmap (read-bitmap "resources/cards/cardClubs10.png") (+ *x_croupier* 20) 30)
        (send button enable #f)
        (send parent-frame refresh))]))

#|Creates button for new card. Updates images on canvas. The position of the cards depend on the amount of players.
@param horiz-panel : horizontal-panel%
@param parent-frame : frame%
@param my-dc : bitmap-dc%
@param amount-players : int
|#
(define (new-card-button horiz-panel parent-frame my-dc amount-players)
(new button% [label "New Card"]
      [parent horiz-panel]
      [vert-margin 5]
      [horiz-margin 5]
      [callback
      (lambda (button event)
        ;(send my-dc clear)
        ;(send my-dc draw-bitmap (read-bitmap "plainDoge2.jpg") 100 0)

        (send parent-frame refresh))]))

#|Creates button to stand. The player wont receive more cards.
@param horiz-panel : horizontal-panel%
@param parent-frame : frame%
@param my-dc : bitmap-dc%
|#
(define (stand-button horiz-panel parent-frame my-dc)
(new button% [label "Stand"]
      [parent horiz-panel]
      [vert-margin 5]
      [horiz-margin 5]
      [callback
      (lambda (button event)
        ;(send my-dc clear)
        ;(send my-dc draw-bitmap (read-bitmap "plainDoge2.jpg") 100 0)
        ;(send button enable #f)
        (send parent-frame refresh))]))

#|Adds croupier to queue
@param names : list|#
(define (add-croupier names)
  (cons (list "*croupier*" '() #t) names))

#|Starts game by ...|#
(define (start-game names)
  (send welcome-window show #f)
  (set! *players* (add-croupier names)) ;sets list with sublists of the names of each player and the croupier
  (let 
    ([blackjack-table (read-bitmap "resources/table.jpg")]
    [back-card (read-bitmap "resources/cards/cardBack_red4.png")])
    (main-canvas game-window blackjack-table back-card)
    (let 
      ([horiz-panel (create-horiz-panel game-window)])
      (let ([drawing-context (new bitmap-dc% [bitmap blackjack-table])])
        (begin-button horiz-panel game-window drawing-context (length names))
        (new-card-button horiz-panel game-window drawing-context (length names))
        (stand-button horiz-panel game-window drawing-context)
      )))
  (send game-window show #t)
  )

(send welcome-window show #t)