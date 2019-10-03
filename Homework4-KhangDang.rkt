;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Homework4-KhangDang) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Excercise 1
(define-struct slope-int-line [m b x y])
; A LineData is one of:
; - (make-slop-int-line Number Number Number Number)
; - Number
; and represents the m,b constants and x,y variables needed for a line in slope-intercept form,
; or the x value of a vertical line

; Excercise 2
; for the intepretation of MonkeyChain, it is missing the case of there might be no monkey.

; Excercise 3
(define-struct delay [reason minutes])
; A TrainStatus is one of:
; - Integer
; - String
; - (make-delay String PosInt)
; - false
; and represents number of minutes away, a status message (e.g., "arriving"),
; minutes of delay and reason, or that the train has been cancelled
(define TRAINSTATUS1 5)
(define TRAINSTATUS2 "arriving")
(define TRAINSTATUS3 (make-delay "brake error" 15))
(define TRAINSTATUS4 false)

; trainstatus-temp : TrainStatus -> ???
;(define (trainstatus-temp ts)
;  (cond
;    [(integer? ts)...]
;    [(string? ts)...]
;    [(delay? ts)...(delay-reason ts)...(delay-minutes ts)...]
;    [(false? ts)...]))

; announcement : PosInt String Trainstatus -> String
; Computes an announcement for the train given a train number, destination, and Trainstatus
(check-expect (announcement 9 "Portland" 1) "Train 9 to Portland is 1 minute away.")
(check-expect (announcement 123 "Newark" "arriving") "Train 123 to Newark is arriving.")
(check-expect (announcement 82 "Philadelphia" "boarding") "Train 82 to Philadelphia is boarding.")
(check-expect (announcement 76 "Chicago" (make-delay "signaling error" 10)) "Train 76 to Chicago is delayed 10 minutes due to signaling error.")
(check-expect (announcement 9348 "Boston" false) "Train 9348 to Boston is cancelled.")
              
(define (announcement train-num desti ts)
  (string-append "Train " (number->string train-num) " to " desti " is "
                 (cond
                    [(integer? ts) (how-far-away ts)]
                    [(string? ts) ts]
                    [(delay? ts) (time-delayed-reason ts)]
                    [(false? ts) "cancelled"]) "."))

; how-far-away : TrainStatus -> String
; produces a string that reports how far away the train is given the train's status
(check-expect (how-far-away 1) "1 minute away")
(check-expect (how-far-away 5) "5 minutes away")
(define (how-far-away ts)
  (string-append (number->string ts) (if (equal? 1 ts) " minute " " minutes ") "away"))

; time-delayed-reason : TrainStatus -> String
; produces a string that reports the time delayed and reason given the train's status
(check-expect (time-delayed-reason TRAINSTATUS3) "delayed 15 minutes due to brake error")
(check-expect (time-delayed-reason (make-delay "signaling error" 1)) "delayed 1 minute due to signaling error")
(define (time-delayed-reason ts)
  (string-append "delayed " (number->string (delay-minutes ts))
                 (if (equal? 1 (delay-minutes ts)) " minute " " minutes ") "due to " (delay-reason ts)))

; Excercise 4
(require 2htdp/image)
(require 2htdp/universe)

(define REDSCREEN (rectangle 300 200 "solid" "red"))
(define BLUESCREEN (rectangle 300 200 "solid" "blue"))

(define-struct red [ticks-left])
(define-struct blue [ticks-left])
 
; A ReflexGameState (RGS) is one of:
; - (make-red Nat)
; - (make-blue Nat)
; and represents the current state of the game,
; either red or blue, and how many ticks are left
; to show in that state before switching to the other

(define RED1 (make-red 140))
(define RED2 (make-red 100))
(define RED3 (make-red 1))

; red-temp : RED -> ???
(define (red-temp red)
  (...(red-ticks-left red)...))

(define BLUE1 (make-blue 14))
(define BLUE2 (make-blue 10))
(define BLUE3 (make-blue 1))

; blue-temp : BLUE -> ???
(define (blue-temp blue)
  (...(blue-ticks-left blue)...))

; rgs-temp : RGS -> ???
;(define (rgs-temp rgs)
;  (cond
;    [(red? rgs)...(red-ticks-left)...]
;    [(blue? rgs) ...(blue-ticks-left)...]))

; reflexes : Any -> Boolean
; allows user to see a Boolean indicating whether or not they exited the program during a blue screen.
(define (reflexes rgs)
  (exit-during-blue?
   (big-bang rgs
     [to-draw draw-screen]
     [on-tick count-down-or-change-color]
     [stop-when key-pressed?])))

; draw-screen : RGS -> Image
; draws an image of red or blue color given the current number and how many ticks it has left
(check-expect (draw-screen RED1) REDSCREEN)
(check-expect (draw-screen RED2) REDSCREEN)
(check-expect (draw-screen RED3) REDSCREEN)
(check-expect (draw-screen BLUE1) BLUESCREEN)
(check-expect (draw-screen BLUE2) BLUESCREEN)
(check-expect (draw-screen BLUE3) BLUESCREEN)

(define (draw-screen rgs)
  (cond
    [(red? rgs) REDSCREEN]
    [(blue? rgs) BLUESCREEN]))

; count-down-or-change-color : RGS -> RGS
; counts down ticks of the current color and switches to the next screen color if current color has 1 tick left
(check-expect (count-down-or-change-color RED1) (make-red 139))
(check-expect (count-down-or-change-color RED2) (make-red 99))
(check-expect (count-down-or-change-color RED3) (make-blue 14))
(check-expect (count-down-or-change-color BLUE1) (make-blue 13))
(check-expect (count-down-or-change-color BLUE2) (make-blue 9))
(check-expect (count-down-or-change-color BLUE3) (make-red 140))

(define (count-down-or-change-color rgs)
  (cond
    [(red? rgs)
     (cond
       [(and (>= 140 (red-ticks-left rgs)) (> (red-ticks-left rgs) 1)) (make-red (sub1 (red-ticks-left rgs)))]
       [(equal? (red-ticks-left rgs) 1) (make-blue 14)])]
    [(blue? rgs)
     (cond
       [(and (>= 28 (blue-ticks-left rgs)) (> (blue-ticks-left rgs) 1)) (make-blue (sub1 (blue-ticks-left rgs)))]
       [(equal? (blue-ticks-left rgs) 1) (make-red 140)])]))

; key-pressed? : rgs KeyEvent -> Boolean
; determines whether or not a key is pressed
(check-expect (key-pressed? "o") true)
(check-expect (key-pressed? (make-red 2)) false)

(define (key-pressed? ke)
  (key-event? ke))

; exit-during-blue? : rgs -> Boolean
; determines whether or not user exit during blue screen
;(define (exit-during-blue? rgs)
;  ...

; Excercise 5
(define BACKGROUND (empty-scene 200 200))
; A Posn is a (make-posn Number Number)
; and represents a 2D position
 
(define-struct circ [radius center])
(define-struct sq [side center])
 
; A Shape is one of...
; - (make-circ PosNumber Posn)
; - (make-sq PosNumber Posn)
; and represents a cirle with a radius and center
; or a square with side length and center

(define CIRC1 (make-circ 5 (make-posn 10 15)))
(define CIRC2 (make-circ 10 (make-posn 24 7)))

; circ-temp : CIRC -> ???
(define (circ-temp circ)
  (...(circ-radius circ)...(circ-center circ)...))

(define SQ1 (make-sq 7 (make-posn -9 40)))
(define SQ2 (make-sq 25 (make-posn 15 -15)))

; sq-temp : SQ -> ???
(define (sq-temp sq)
  (...(sq-side sq)...(sq-center sq)...))

(define SHAPE1 CIRC1)
(define SHAPE2 CIRC2)
(define SHAPE3 SQ1)
(define SHAPE4 SQ2)

; shape-temp : SHAPE -> ???
(define (shape-temp shape)
  (cond
    [(circ? shape) (...(circ-temp (circ-radius shape))...(circ-temp (circ-center shape)))]
    [(sq? shape) (...(sq-temp (sq-side shape))...(sq-temp (sq-center shape)))]))

; blink : SHAPE -> Image
; displays a shape with purple outline, switches to solid visual when user clicks inside shape,
; re-centers shape to mouse position when user clicks outside shape
(define (blink shape)
  (big bang shape
       [to-draw draw-shape]
       [on-mouse switch-visual-or-change-center]))

; draw-shape : SHAPE
; draws a shape with of the program
(check-expect (draw-shape SHAPE1) (overlay/offset (circle (circ-radius SHAPE1) "outline" "purple")
                                                  (- (circ-center (posn-x SHAPE1))) (- (circ-center (posn-y SHAPE1))) (BACKGROUND)))
(check-expect (draw-shape SHAPE3) (overlay/offset (square (sq-side SHAPE3) "outline" "purple")
                                                  (- (sq-center (posn-x SHAPE3))) (- (sq-center (posn-y SHAPE3))) (BACKGROUND)))
(define (draw-shape shape)
  (cond
    [(circ? shape) (overlay/offset (circle (circ-radius shape) "outline" "purple")
                                                  (- (circ-center (posn-x shape))) (- (circ-center (posn-y shape))) BACKGROUND)]
    [(sq? shape) ((overlay/offset (square (sq-side shape) "outline" "purple")
                                                  (- (sq-center (posn-x shape))) (- (sq-center (posn-y shape))) BACKGROUND)

    
    
                  


    
       
    






 



                 
                 