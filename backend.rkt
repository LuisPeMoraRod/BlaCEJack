#lang racket
(require racket/random)


#|Generates a random figure for a card
@param random-number : the value that it will have assigned will be the result of (random 4) and it will represent one of the 
                       4 possible figures of the deck: "H" -> Heart
                                                       "C" -> Clubs
                                                       "D" -> Diamond
                                                       "S" -> Spade
@return a string that represents the card figure|#
(define (pick-random-figure random-number)
    (cond 
        ((= random-number 0) "H")
        ((= random-number 1) "C")
        ((= random-number 2) "D")
        (else "S")))

#|Generates a random value for a card
@param random-number : the value that it will have assigned will be the result of (random 1 13) and it will represent one of the
                       12 possible values that a card-deck has
@return the random value of a card|#
(define (pick-random-value random-number)
    (cond 
        ((= random-number 1) "A")
        ((= random-number 10) "J")
        ((= random-number 11) "Q")
        ((= random-number 12) "K")
        (else random-number)))

#|Generates a random card with a value and a figure
@return a pair that represents the card, the first element represents the value of the card and the second its figure|#
(define (pick-random-card)
    (list (pick-random-value (random 1 13)) (pick-random-figure (random 4))))

#|This function works as an auxiliary to get the cards that a player has
@param players-info : this is a list that contains sublists with the player's info; this includes the player's name, 
                      a list of cards that the player has and a boolean that indicates if the player has already
                      stand or not
@return the list of cards that the first player of the list has|#
(define (get-players-cards-aux players-info)
    (cadar players-info))

#|Creates a list of sublists that contains each of the cards that has been given to the players
@param players-info : this is a list that contains sublists with the player's info; this includes the player's name, 
                      a list of cards that the player has and a boolean that indicates if the player has already
                      stand or not
@return the list of sublists that contains each of the picked cards|#
(define (get-players-cards players-info)
    (cond 
        ((null? players-info) '())
        (else (cons (get-players-cards-aux players-info) (get-players-cards (cdr players-info))))))
