#lang racket
(require racket/gui
         racket/draw
         racket/string
        )

(require "logic.rkt")

(define *players* null) ;global variable that is going to register the cards owned by each player
 ;global variables for the position in screen of the cards of each player
(define *x-croupier* 360)
(define *y-croupier* 30)
(define *x-pos0* null)
(define *y-pos0* null)
(define *x-pos1* null)
(define *y-pos1* null)
(define *x-pos2* null)
(define *y-pos2* null)

(define *players-id* (make-hash))

;list containing the position in screen for the next card to be added
(define *cards-pos* null)

;hash table to link a code that represents every card to the path of that card's image (png)
(define cards-hash-table
  (hash "2C" 'resources/cards/cardClubs2.png
        "3C" 'resources/cards/cardClubs3.png
        "4C" 'resources/cards/cardClubs4.png
        "5C" 'resources/cards/cardClubs5.png
        "6C" 'resources/cards/cardClubs6.png
        "7C" 'resources/cards/cardClubs7.png
        "8C" 'resources/cards/cardClubs8.png
        "9C" 'resources/cards/cardClubs9.png
        "10C" 'resources/cards/cardClubs10.png
        "AC" 'resources/cards/cardClubsA.png
        "JC" 'resources/cards/cardClubsJ.png
        "QC" 'resources/cards/cardClubsQ.png
        "KC" 'resources/cards/cardClubsK.png

        "2D" 'resources/cards/cardDiamonds2.png
        "3D" 'resources/cards/cardDiamonds3.png
        "4D" 'resources/cards/cardDiamonds4.png
        "5D" 'resources/cards/cardDiamonds5.png
        "6D" 'resources/cards/cardDiamonds6.png
        "7D" 'resources/cards/cardDiamonds7.png
        "8D" 'resources/cards/cardDiamonds8.png
        "9D" 'resources/cards/cardDiamonds9.png
        "10D" 'resources/cards/cardDiamonds10.png
        "AD" 'resources/cards/cardDiamondsA.png
        "JD" 'resources/cards/cardDiamondsJ.png
        "QD" 'resources/cards/cardDiamondsQ.png
        "KD" 'resources/cards/cardDiamondsK.png

        "2H" 'resources/cards/cardHearts2.png
        "3H" 'resources/cards/cardHearts3.png
        "4H" 'resources/cards/cardHearts4.png
        "5H" 'resources/cards/cardHearts5.png
        "6H" 'resources/cards/cardHearts6.png
        "7H" 'resources/cards/cardHearts7.png
        "8H" 'resources/cards/cardHearts8.png
        "9H" 'resources/cards/cardHearts9.png
        "10H" 'resources/cards/cardHearts10.png
        "AH" 'resources/cards/cardHeartsA.png
        "JH" 'resources/cards/cardHeartsJ.png
        "QH" 'resources/cards/cardHeartsQ.png
        "KH" 'resources/cards/cardHeartsK.png

        "2S" 'resources/cards/cardSpades2.png
        "3S" 'resources/cards/cardSpades3.png
        "4S" 'resources/cards/cardSpades4.png
        "5S" 'resources/cards/cardSpades5.png
        "6S" 'resources/cards/cardSpades6.png
        "7S" 'resources/cards/cardSpades7.png
        "8S" 'resources/cards/cardSpades8.png
        "9S" 'resources/cards/cardSpades9.png
        "10S" 'resources/cards/cardSpades10.png
        "AS" 'resources/cards/cardSpadesA.png
        "JS" 'resources/cards/cardSpadesJ.png
        "QS" 'resources/cards/cardSpadesQ.png
        "KS" 'resources/cards/cardSpadesK.png

        "XX" 'resources/cards/cardBack_red4.png
        ))

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
@return lst : list without empty elements. Every sublist contains a name, an empyt list for the cards and a boolean in #t indicating
that the player is still playing (hasn't ask to stand) i.e. (("Luis" () #t) ("Monica" () #t))|#
(define (create-players-list lst)
  (cond ((null? lst) '() )
        ((equal? (car lst) "") (create-players-list (cdr lst)))
        (else (cons (list (car lst) '() #t) (create-players-list (cdr lst))))))

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
        ;add blackjack deck image to canvas
        (send drawing-context draw-bitmap table-bitmap 0 0)
        ;draw representation of deck of cards in canvas
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
        (set-positions amount-players)
        (set-ids)
        ;(send my-dc clear)
        ;(set-turn-mssg (get-current-turn *players*))
        (for ([i (in-range (length *players*))])
              (set! *players* (card-request *players*)) ;gives two cards to the first player in queue
              (let ([player (get-current-player *players*)])
                  (let ([id (hash-ref *players-id* player)]
                        [cards (get-first-player-cards *players*)])
                            (draw-card id (cadr cards) my-dc) ;draw first card
                            (cond ((equal? id 3) 
                                    (draw-card id '("X" "X") my-dc)) ; draw back of the card for croupier
                                  (else (draw-card id (car cards) my-dc))) ;draw second card
                            ) 
                    )
              (set! *players* (update-players-queue *players*))
              )
        (set-turn-mssg (get-current-player *players*))

        ;(send my-dc draw-bitmap (read-bitmap "resources/cards/cardBack_red4.png") *x-croupier* *y-croupier*)
        ;(send my-dc draw-bitmap (read-bitmap "resources/cards/cardClubs10.png") (+ *x-croupier* 20) *y-croupier*)
        (send button enable #f)
        (send parent-frame refresh))]))

#|Sets the initial values of the global variables that contain the x coordinate of the position in screen of the cards images.
@param amount-players : integer|#
(define (set-positions amount-players)
  (cond ((equal? amount-players 1) (set! *x-pos0* 368) (set! *y-pos0* 337))
        ((equal? amount-players 2) (set! *x-pos0* 265) (set! *y-pos0* 335) (set! *x-pos1* 472) (set! *y-pos1* 335))
        ((equal? amount-players 3) (set! *x-pos0* 155) (set! *y-pos0* 310) (set! *x-pos1* 368) (set! *y-pos1* 337) (set! *x-pos2* 580) (set! *y-pos2* 310))))

#|Sets hashmap that links every player's name (key) with an id (value)|#
(define (set-ids)
  (for ([i (in-range (amount-players *players*))])
      (hash-set! *players-id* (car (list-ref *players* i)) i))
  (hash-set! *players-id* "*croupier*" 3)) ; croupier will always have id = 3

(define (draw-card id card my-dc)
  (let ([card-png (~a (hash-ref cards-hash-table (get-card-code card)))])
      (cond ((equal? id 0)
                (send my-dc draw-bitmap (read-bitmap card-png) *x-pos0* *y-pos0*)
                (set! *x-pos0* (+ *x-pos0* 20)))
            ((equal? id 1)
                (send my-dc draw-bitmap (read-bitmap card-png) *x-pos1* *y-pos1*)
                (set! *x-pos1* (+ *x-pos1* 20)))
            ((equal? id 2)
                (send my-dc draw-bitmap (read-bitmap card-png) *x-pos2* *y-pos2*)
                (set! *x-pos2* (+ *x-pos2* 20)))
            ((equal? id 3)
                (send my-dc draw-bitmap (read-bitmap card-png) *x-croupier* *y-croupier*)
                (set! *x-croupier* (+ *x-croupier* 20))))))

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
  (append names (list(list "*croupier*" '() #t))))

#|Returns the amount of players in the game
@param players : list with players (including croupier)
@return integer|#
(define (amount-players players)
  (- (length players) 1)
)

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
        (begin-button horiz-panel game-window drawing-context (amount-players *players*))
        (new-card-button horiz-panel game-window drawing-context (amount-players *players*))
        (stand-button horiz-panel game-window drawing-context)
      )))
  (send game-window show #t)
  )

(send welcome-window show #t)
;(card-request (card-request (create-players-list '("1" "2"))))