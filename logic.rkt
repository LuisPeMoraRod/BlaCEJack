#lang racket

(require racket/random)

#|
    This file contains the logic behind the BlaCEJack game
    @author Monica Waterhouse
|#

;export functions to be used in gui.rkt

(provide get-current-player
         update-players-queue
         card-request
         get-first-player-cards
         get-card-code
         get-player-score
         get-last-card-given
         has-A?
         busted?
         cupier-play)


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
        ((member? picked-card (car players-cards-list)) #t)
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
        ((compare-cards (get-picked-cards players-info-list) picked-card) (give-card players-info-list (pick-random-card)))
        (else (give-card-aux players-info-list picked-card))))

#|Function called when the player request a new card. If they don't have cards, they will receive 2 cards, otherwise they will be given one
@param players-info-list : is the list that contains the information of all the players
@return : the updated players information list||#
(define (card-request players-info-list)
    (cond
        ((= (length (get-first-player-cards players-info-list)) 0) (card-request (give-card players-info-list (pick-random-card))))
        (else (give-card players-info-list (pick-random-card)))))

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
@return : true if the score is less than 16 or false if it is equal or greater than 17|#
(define (less-than-16? players-info-list)
    (cond
        ((> (string->number (get-player-score players-info-list)) 16) #f)
        (else #t)))

#|Once all players stand, this function is called for the crupier to pick cards until the score is greater than 16
@param players-info-list : is the list that contains the information of all the players
@return : the updated players-info-list with all the players on stand including the crupier|#
(define (cupier-play players-info-list)
    (cond 
        ((less-than-16? players-info-list) (cupier-play (card-request players-info-list)))
        (else players-info-list)))

#|Count the number of cards the first player on the list has
@param players-info-list : is the list that contains the information of all the players
@return an integer with the number of cards the first player has|#
(define (count-cards players-info-list)
    (length (get-first-player-cards players-info-list)))

#|Creates a new list with the score information of a player
@param players-info-list : is the list that contains the information of all the players
@return : a list that contains: (score, name-of-the-player, amoubt-of-cards)|#
(define (get-final-score-aux players-info-list)
    (list (string->number (get-player-score players-info-list)) (get-current-player players-info-list) (count-cards players-info-list)))

#|Creates a list with the score information of all the players
@param players-info-list : is the list that contains the information of all the players
@return : a list with sublist that contain the score information of each player. Each sublist contains: (score, name-of-the-player, amoubt-of-cards)|#
(define (get-final-score players-info-list)
    (cond 
        ((null? players-info-list) '())
        (else (cons (get-final-score-aux players-info-list) (get-final-score (cdr players-info-list))))))

#|Deletes the last element of a list
@param element-list : the list for which the last element will be deleted
@return : the element-list without the last element|#
(define (delete-final-element element-list)
    (cond
        ((= (length element-list) 1) '())
        (else (cons (car element-list) (delete-final-element (cdr element-list))))))

#|Retrieves the last element of a list
@param element-list : the list from which the last element will be retrieved
@return : the last element of the element-list|#
(define (get-final-element element-list)
    (cond
        ((= (length element-list) 1) (car element-list))
        (else (get-final-element (cdr element-list)))))

#|This function puts the element that has the highest value at the end of the list by checking if the value of the score of the
current player is minor to the one next to it, if it is, it moves to the next element without making a switch, and if it is not 
it makes a switch and moves to the next element.
@param players-scores : this is a list that contains sublist wit the score info of each player: (score, player-name, amount-of-cards)
@return : the players-score list with the subllist of the highest score at the end)|#
(define (bubble-sort-aux players-scores)
    (cond
        ((= (length players-scores) 1) players-scores)
        ((<= (caar players-scores) (caadr players-scores)) (cons (car players-scores) (bubble-sort-aux (cdr players-scores))))
        (else (cons (cadr players-scores) (bubble-sort-aux (cons (car players-scores) (cddr players-scores)))))))

#|This function sorts the players-score list starting from the highest value to the lowest value using the bubble sort algorithm
@param players-scores : this is a list that contains sublist wit the score info of each player: (score, player-name, amount-of-cards)
@return : the players-scores list sorted from the highest to lowest score|#
(define (bubble-sort-scores players-scores)
    (cond 
        ((null? players-scores) '())
        (else 
            (cons (get-final-element (bubble-sort-aux players-scores)) (bubble-sort-scores (delete-final-element (bubble-sort-aux players-scores)))))))

#|Checks which scores are higher that 21 and add them to a list
@param players-scores : this is a list that contains sublist wit the score info of each player: (score, player-name, amount-of-cards)
@return : a list with sublist of the players which scores are higher than 21|#
(define (greater-than-21 players-scores)
    (cond
        ((null? players-scores) '())
        ((<= (caar players-scores) 21) '())
        (else (reverse (cons (car players-scores) (greater-than-21 (cdr players-scores)))))))

#|Checks which scores are lower that 21 and add them to a list
@param players-scores : this is a list that contains sublist wit the score info of each player: (score, player-name, amount-of-cards)
@return : a list with sublist of the players which scores are lower than 21|#
(define (less-than-21 players-scores)
    (cond
        ((null? players-scores) '())
        ((>= (caar players-scores) 21) (less-than-21 (cdr players-scores))) 
        (else players-scores)))

#|Gets the amount of cards that the current player player has
@param players-scores : this is a list that contains sublist wit the score info of each player: (score, player-name, amount-of-cards)
return : an integer with the amount of cards a player has|#
(define (get-card-count player-score)
    (caddr player-score))

#|Creates a list with the scores that are equal to 21 and orders them depending on the amount of cards the player has,
this means that if two or more players have a score of 21, the one that has less cards will be the first on the list
@param players-scores : this is a list that contains sublist wit the score info of each player: (score, player-name, amount-of-cards)
@param reference : a list with sublists: (score, player-name, amount-of-cards), which first element will be the one that has less
                   amount of cards.
@return : a list of sublists with the players score information which scores are equal to 21 and sorted depending on the amount of cards|#
(define (equal-to-21 players-scores reference)
    (cond
        ((null? players-scores) reference)
        ((= (caar players-scores) 21) 
            (cond
                ((null? reference) (equal-to-21 (cdr players-scores) (list (car players-scores))))
                ((< (get-card-count (car reference)) (get-card-count (car players-scores))) (equal-to-21 (cdr players-scores) (append reference (list (car players-scores)))))
                (else (equal-to-21 (cdr players-scores) (append (list (car players-scores)) reference)))))
        ((> (caar players-scores) 21) (equal-to-21 (cdr players-scores) reference))
        (else reference)))

#|With the sorted scores (from highest to lowest) this function creates the final game rank according to the game rules
@param players-scores : this is a list that contains sublist wit the score info of each player: (score, player-name, amount-of-cards). This list must be sorted
@return : a list with the final game ranking starting with the players whose score is equal to 21 and arrange by their card amounts, then
          the players whose score is lower than 21 from highest to lowest and finally the ones that have a score greater than 21|#
(define (get-rank-aux players-scores)
    (append (equal-to-21 players-scores '()) (less-than-21 players-scores) (greater-than-21 players-scores)))

#|This function creates the final game rank according to the game rules
@param players-info-list : is the list that contains the information of all the players
@return : a list with the final game ranking starting with the players whose score is equal to 21 and arrange by their card amounts, then
          the players whose score is lower than 21 from highest to lowest and finally the ones that have a score greater than 21|#
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

;(set-A-card '(("luis" (("A" "S") ("3" "H")) ) ("moni" (("9" "S")))) "11" '("A" "S"))
;(has-A? '(("luis" (("A" "S") ("3" "H")) ) ("moni" (("9" "S")))))