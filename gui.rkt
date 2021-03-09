#lang racket
(require racket/gui
         racket/draw
         racket/string
         )

(define *players* null)

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
       ))

#|Deletes null lists from a list
@param lst : list with sublists that contain the names received from text inputs (may contain empty sublists)
@return lst : list without empty elements|#
(define (create-players-list lst)
  (cond ((null? lst) '() )
        ((equal? (car lst) "") (create-players-list (cdr lst)))
        (else (cons (list (car lst)) (create-players-list (cdr lst))))))

#|Define label that shows the player who has the current turn|#
(define turn-mssg 
(new message%
       [parent game-window]
       [label "It's your turn: "]
       [vert-margin 2]
       [horiz-margin 10]))

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
  {send turn-mssg set-label (string-join (list (send turn-mssg get-label) name))})

#|Creates the main canvas where the cards will be displayed
@param frame : frame%
@param table-bitmap : bitmap%|#
(define (main-canvas frame table-bitmap)
  (new canvas% 
    [parent frame]
    [min-height 365]
    [paint-callback
      (lambda (canvas drawing-context)
        (send drawing-context draw-bitmap table-bitmap 0 0)
        (send drawing-context draw-bitmap (read-bitmap "resources/test_card.png") 100 100))])
  )

#|Starts game by ...|#
(define (start-game names)
  (send welcome-window show #f)
  (set-turn-mssg (get-current-turn names))
  (let 
    ([blackjack-table (read-bitmap "resources/table.jpg")])
    (main-canvas game-window blackjack-table))
  (send game-window show #t)
  )

(send welcome-window show #t)