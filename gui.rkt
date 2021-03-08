#lang racket
(require racket/gui
         racket/draw)

(define welcome-window
    (new frame%
        [label "BlaCEJack"]
        [width 400]
        [height 300]
        [style '(no-resize-border)]
        [alignment '(center center)]
        [stretchable-width #f]
        [stretchable-height #f]))

(define name-input-1 
    (new text-field%
        [label "Player 1:"]
        [parent welcome-window]
        [min-width 300]
        [min-height 30]
        [vert-margin 10]
        [horiz-margin 10]))

(send welcome-window show #t)