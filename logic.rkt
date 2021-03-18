#lang racket

(require racket/random)

;export functions
(provide get-current-player
         update-players-queue
         card-request
         get-first-player-cards
         get-card-code
         get-player-score
         get-last-card-given
         has-A?
         busted?
         card-request-croupier
         get-rank)


#|Generates a random figure for a card
@param random-number : the value that it will have assigned will be the result of (random 4) and it will represent one of the 
                       4 possible figures of the deck: "H" -> Heart
                                                       "C" -> Clubs
                                                       "D" -> Diamond
                                                       "S" -> Spade
@return : a string that represents the card figure|#
(define (pick-random-figure random-number)
    (cond 
        ((= random-number 0) "H")
        ((= random-number 1) "C")
        ((= random-number 2) "D")
        (else "S")))

#|Generates a random value for a card
@param random-number : the value that it will have assigned will be the result of (random 1 13) and it will represent one of the
                       12 possible values that a card-deck has
@return : the random value of a card|#
(define (pick-random-value random-number)
    (cond 
        ((= random-number 1) "A")
        ((= random-number 10) "J")
        ((= random-number 11) "Q")
        ((= random-number 12) "K")
        (else (~v random-number))))

#|Generates a random card with a value and a figure
@return : a pair that represents the card, the first element represents the value of the card and the second its figure|#
(define (pick-random-card)
    (list (pick-random-value (random 1 13)) (pick-random-figure (random 4))))

#|This function works as an auxiliary to get the cards that a player has
@param players-info : this is a list that contains sublists with the player's info; this includes the player's name, 
                      a list of cards that the player has and a boolean that indicates if the player has already
                      stand or not
@return : the list of cards that the first player of the list has|#
(define (get-first-player-cards players-info-list)
    (cadar players-info-list))

#|Creates a list of sublists that contains each of the cards that has been given to the players
@param players-info : this is a list that contains sublists with the player's info; this includes the player's name, 
                      a list of cards that the player has and a boolean that indicates if the player has already
                      stand or not
@return : the list of sublists that contains each of the picked cards|#
(define (get-picked-cards players-info-list)
    (cond 
        ((null? players-info-list) '())
        (else (cons (get-first-player-cards players-info-list) (get-picked-cards (cdr players-info-list))))))

#|Indicates if an element belongs to a list
@param picked-card : is the card that is being compared to the ones in the list
@param player-cards : is a list of cards (the cards are represented by pairs)
@return : #t if the card belongs to the list or #f if it is not a member|#
(define (member? picked-card player-cards-list) 
    (cond   
        ((null? player-cards-list) #f)
        ((equal? (car player-cards-list) picked-card) #t)
        (else (member? picked-card (cdr player-cards-list)))))

#|Indicates if a random card belongs to the cards that have already been picked by the players
@param players-cards : is a list that contains sublists with the cards that each one of the players have
@param picked-card : is a card that is going to be compare with the ones in the player-cards-list|#
(define (compare-cards players-cards-list picked-card)
    (cond
        ((null? players-cards-list) #f)
        ((and #t (member? picked-card (car players-cards-list))) #t)
        (else (compare-cards (cdr players-cards-list) picked-card))))

#|Updates the first player info by adding a new card to its card's list
@param player-info-list : is a list containing the info of the first player
@param picked-card : is the card that will be given to the player
@return : the player-info-list updated with the new card|#
(define (insert-card-into-list player-info-list picked-card)
    (list (car player-info-list) (cons picked-card (cadr player-info-list)) (cddr player-info-list)))

#|Updates the players information list with the new card added to the first player
@param players-info-list : is the list that contains the information of all the players
@param picked-card : the card that will be given to the first player
@return : the updated players information list|#
(define (give-card-aux players-info-list picked-card)
    (cons (insert-card-into-list (car players-info-list) picked-card) (cdr players-info-list)))

#|Checks if a picked card has already been given to one of the players and if it hasn't is given to the first player of the list
@param players-info-list : is the list that contains the information of all the players
@param picked-card : the card that will be given to the first player
@return : the updated players information list|#
(define (give-card players-info-list picked-card)
    (cond
        ((and #t (compare-cards (get-picked-cards players-info-list) picked-card)) (give-card players-info-list (pick-random-card)))
        (else (give-card-aux players-info-list picked-card))))

#|Function called when the player request a new card. If they don't have cards, they will receive 2 cards, otherwise they will be given one
@param players-info-list : is the list that contains the information of all the players
@return : the updated players information list||#
(define (card-request players-info-list)
    (cond
        ((= (length (get-first-player-cards players-info-list)) 0) 
            (cond ((equal? (get-current-player players-info-list) "*croupier*") 
                        (card-request (card-request-croupier players-info-list)))
                    (else (card-request (give-card players-info-list (pick-random-card))))))
        (else 
            (cond ((equal? (get-current-player players-info-list) "*croupier*")
                        (card-request-croupier players-info-list))
                   (else (give-card players-info-list (pick-random-card)))))))

#|Function called when the croupier requests a new card. Works very similar to card-request, but in this function, a value of 11 or 1 is automatically 
assigned to an Ace card. The card should be 11 if the count won't be greater than 21, otherwise, the card value would be 1.
@param players-info-list 
@return updated players-info-list|#
(define (card-request-croupier players-info-list)
    (card-request-croupier-aux (give-card players-info-list (pick-random-card)))
    )
(define (card-request-croupier-aux players-info-list)
    (cond ((equal? (car (get-last-card-given players-info-list)) "A") ; checks if new card is an Ace
                (cond ((<= (string->number (get-player-score players-info-list)) 10) (last-card-to-11 players-info-list))
                        (else (last-card-to-1 players-info-list))))
          (else players-info-list)))

#|Changes the value of the last card given to the player in turn to a 11|#
(define (last-card-to-11 players)
    (cons (list (get-current-player players) (cons (list "11" (cadr (get-last-card-given players))) ;recreates list by changing the first card of the first player
    (cdr (get-first-player-cards players)))) (cdr players)))

#|Changes the value of the last card given to the player in turn to a 1|#
(define (last-card-to-1 players)
    (cons (list (get-current-player players) (cons (list "1" (cadr (get-last-card-given players))) ;recreates list by changing the first card of the first player
    (cdr (get-first-player-cards players)))) (cdr players)))

#|Given a card, this function generates a code corresponding to that card
@param card : is the card that the code is going to be generated for. It has the form: ("value" "figure")
@return : a string that represents a code corresponding to that specific card. It has the form "valuefigure"|#
(define (get-card-code card)
    (string-append (car card) (cadr card)))

#|Fuction that allows to access the last card given to the first player of the list (current player)
@param players-info-list : is the list that contains the information of all the players
@return : the last card given to the current player |#
(define (get-last-card-given players-info-list) 
    (car (get-first-player-cards players-info-list)))

#|This fuction calls the get-last-card-given and the get-card-code to return the card code of the last card given to the current player
@param players-info-list : is the list that contains the information of all the players
@return : the code of the last card given to the current player|#
(define (get-last-card-code players-info-list)
    (get-card-code (get-last-card-given players-info-list)))

#|Given a player list of cards, it sums the value of each one of the cards
@param player-cards : the list of cards of the current player
@return : the score of the current player|#
(define (get-player-score-aux player-cards)
    (cond 
        ((null? player-cards) 0)
        (else
            (+ (get-card-value (car player-cards)) (get-player-score-aux (cdr player-cards))))))

#|Checks if the value of the card is a letter or a number
@param card : the card to retrieve the value from
@return : return the integer 10 if the value is a letter, or the corresponding number if the value is an integer|#
(define (get-card-value card)
    (cond
        ((string->number (car card)) (string->number (car card)))
        ((equal? (car card) "A") 0) ;Ace is initially set as 0 for ot
        (else 10))) ;J, Q or K card has value of 10 

#|Function call from the GUI to check the score of the current player
@param players-info-list : is the list that contains the information of all the players
return : the score of the current player as a string|#
(define (get-player-score players-info-list)
    (~v (get-player-score-aux (get-first-player-cards players-info-list))))

#|Checks if the player doesn't want any more cards
@param player-cards : the list of cards of the current player
@return : #t or #f depending on the player stand status|#
(define (get-stand-status player-info-list)
    (caddr player-info-list))

#|Changes the stand status of the current player to false
@param player-cards : the list of cards of the current player
@return : the player-info-list with the updated status|#

(define (set-stand-to-false player-info-list)
    (list (car player-info-list) (cadr player-info-list) #f))

#|Checks if the score of the crupier is less than 16
@param players-info-list : is the list that contains the information of all the players
return : true if the score is less than 16 or false if it is equal or greater than 17|#
(define (less-than-16? players-info-list)
    (cond
        ((> (string->number (get-player-score players-info-list)) 16) #f)
        (else #t)))

#|Once all players stand, this function is called for the crupier to pick cards until the score is greater than 16
@param players-info-list : is the list that contains the information of all the players
return : the updated players-info-list with all the players on stand including the crupier|#
(define (cupier-play players-info-list)
    (cond 
        ((less-than-16? players-info-list) (cupier-play (card-request players-info-list)))
        (else players-info-list)))

(define (count-cards players-info-list)
    (length (get-first-player-cards players-info-list)))

(define (get-final-score-aux players-info-list)
    (list (string->number (get-player-score players-info-list)) (get-current-player players-info-list) (count-cards players-info-list)))

(define (get-final-score players-info-list)
    (cond 
        ((null? players-info-list) '())
        (else (cons (get-final-score-aux players-info-list) (get-final-score (cdr players-info-list))))))

(define (delete-final-element element-list)
    (cond
        ((= (length element-list) 1) '())
        (else (cons (car element-list) (delete-final-element (cdr element-list))))))

(define (get-final-element element-list)
    (cond
        ((= (length element-list) 1) (car element-list))
        (else (get-final-element (cdr element-list)))))

(define (check-and-switch players-scores)
    (cond
        ((= (length players-scores) 1) players-scores)
        ((<= (caar players-scores) (caadr players-scores)) (cons (car players-scores) (check-and-switch (cdr players-scores))))
        (else (cons (cadr players-scores) (check-and-switch (cons (car players-scores) (cddr players-scores)))))))

(define (bubble-sort-scores players-scores)
    (cond 
        ((null? players-scores) '())
        (else 
            (cons (get-final-element (check-and-switch players-scores)) (bubble-sort-scores (delete-final-element (check-and-switch players-scores)))))))

(define (greater-than-21 players-scores)
    (cond
        ((<= (caar players-scores) 21) '())
        (else (reverse (cons (car players-scores) (greater-than-21 (cdr players-scores)))))))

(define (less-than-21 players-scores)
    (cond
        ((>= (caar players-scores) 21) (less-than-21 (cdr players-scores))) 
        (else players-scores)))

(define (get-card-count player-score)
    (caddr player-score))

(define (equal-to-21 players-scores reference)
    (cond
        ((= (caar players-scores) 21) 
            (cond
                ((null? reference) (equal-to-21 (cdr players-scores) (list (car players-scores))))
                ((< (get-card-count (car reference)) (get-card-count (car players-scores))) (equal-to-21 (cdr players-scores) (append reference (list (car players-scores)))))
                (else (equal-to-21 (cdr players-scores) (append (list (car players-scores)) reference)))))
        ((> (caar players-scores) 21) (equal-to-21 (cdr players-scores) reference))
        (else reference)))

(define (get-rank-aux players-scores)
    (append (equal-to-21 players-scores '()) (less-than-21 players-scores) (greater-than-21 players-scores)))

(define (get-rank players-info-list)
    (get-rank-aux (bubble-sort-scores (get-final-score players-info-list))))
;-------------------------------------------------------------------------
;@author: Luis Pedro


#|Get the name of the player with the current turn
@param players : list with the names and list of cards of the players
@return name of the player with the current turn|#
(define (get-current-player players)
    (caar players))

#|Dequeues and enqueues the last player that had the turn
@param players : list with the names and list of cards of the players
@return updated players queue|#
(define (update-players-queue players)
    (append(cdr players) (list (car players))))

#|Checks if the first player has any Ace within his cards
@param players-info-list
@return boolean|#
(define (has-A? players-info-list)
    (has-A-aux (get-first-player-cards players-info-list)))
(define (has-A-aux cards)
    (cond ((null? cards) #f)
          ((equal? (caar cards) "A") #t)
          (else (has-A-aux (cdr cards)))))

#|Checks if the first player (current turn) has a score greater than 21
@param players-info-list
@return boolean|#
(define (busted? players-info-list)
    (> (string->number (get-player-score players-info-list)) 21))