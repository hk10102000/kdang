;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Homework5v1-Khang Dang|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Excercise 1

; A List-of-strings is one of:
; - '()
; - (cons String List-of-strings)
; intepretation a list of strings

(define LOST-0 '())
(define LOST-1 (cons "a" LOST-0))
(define LOST-2 (cons "b" LOST-1))
(define LOST-3 (cons "a" LOST-2))

; lost-temp : Lost -> ???
(define (lost-temp lost)
  (cond
    [(empty? lost)...]
    [(cons? lost)(...(first lost)...
                                (lost-temp (rest lost))...)]))

; str-in-list-even?/v1 : String Lost -> Boolean
; returns true if a given string occurs an even amount of times in given list of strings
(check-expect (str-in-list-even?/v1 "b" LOST-3) false)
(check-expect (str-in-list-even?/v1 "a" LOST-2) false)
(check-expect (str-in-list-even?/v1 "a" LOST-3) true)

(define (str-in-list-even?/v1 str lost)
  (cond
    [(empty? lost) true]
    [(cons? lost)(even? (time-occurs str lost))]))

; time-occurs? : String Lost -> Number
; returns the amount of times the given string occurs in the given list of strings
(check-expect (time-occurs "b" LOST-1) 0)
(check-expect (time-occurs "a" LOST-2) 1)
(check-expect (time-occurs "a" LOST-3) 2)

              
(define (time-occurs str lost)
  (cond
   [(empty? lost) 0]
    [(cons? lost)(+ (if (string=? str (first lost)) 1 0)
                               (time-occurs str (rest lost)))]))

; Excercise 2

; str-in-list-even?/v2 : String Lost -> Boolean
; returns false if previous result of is true
(check-expect (str-in-list-even?/v2 "b" LOST-3) true)
(check-expect (str-in-list-even?/v2 "a" LOST-2) true)
(check-expect (str-in-list-even?/v2 "a" LOST-3) false)

(define (str-in-list-even?/v2 str lost)
  (cond
    [(empty? lost) false]
    [(cons? lost)(not (str-in-list-even?/v1 str lost))]))

; Excercise 3

; A List-of-numbers is one of:
; - '()
; - (cons Number List-of-numbers
; intepretation a list of numbers

(define LON-0 empty)
(define LON-1 (cons 5 LON-0))
(define LON-2 (cons 49 LON-1))
(define LON-3 (cons -8 LON-2))
(define LON-4 (cons 5 LON-3))

; lon-temp : Lon -> ???
(define (lon-temp lon)
  (cond
    [(empty? lon)...]
    [(cons? lon)(...(first lon)...
                               (lon-temp (rest lon)...))]))

; all-the-nums : Lon -> Number
; returns the sum of the given list-of-strings
(check-expect (all-the-nums LON-0) 0)
(check-expect (all-the-nums LON-1) 5)
(check-expect (all-the-nums LON-2) 54)
(check-expect (all-the-nums LON-3) 46)

(define (all-the-nums lon)
  (cond
    [(empty? lon) 0]
    [(cons? lon)(+ (first lon)
                               (all-the-nums (rest lon)))]))

; Excercise 4

; flatten : Lon -> Lon
; converts a list of list of numbers into a single list of numbers
(check-expect (flatten LON-0) LON-0)
(check-expect (flatten LON-1) (list 5))
(check-expect (flatten LON-2) (list 49 5))
(check-expect (flatten LON-3) (list -8 49 5))

(define (flatten lon)
  (cond
    [(empty? lon) lon]
    [(cons? lon) (append (cons (first lon) '())
                                    (flatten (rest lon)))]))

; Excercise 5
(require 2htdp/image)
(require 2htdp/universe)

(define BG (rectangle 300 300 "solid" "white"))
(define BLACKSCR (rectangle 300 300 "solid" "black"))

(define-struct slide [title shown hidden])
; A Slide is a (make-slide String LoS LoS)
; and represents a slide's title,
; what bullets have been shown (top to bottom)
; and which are hidden (top to bottom)
 
; A Slideshow is one of:
; - empty
; - (cons Slide Slideshow)
; and represents an ordered slideshow

(define S-1 (make-slide "Strings" LOST-3 LOST-1))
(define S-2 (make-slide "Numbers" LON-3 LON-2))
(define S-3 (make-slide "Strings and Numbers" LOST-3 LON-3))
(define SL-0 empty)
(define SL-1 (cons S-1 empty))
(define SL-2 (cons S-2 SL-1))
(define SL-3 (cons S-3 SL-2))
(define SL-4 (cons S-2 empty))
(define SL-5 (cons S-1 SL-4))


; slide-temp : Slide -> ???
(define (slide-temp s)
  (...(slide-title s)...
      (slide-shown s)...
      (slide-hidden s)...))

; slideshow-temp : Slideshow -> ???
(define (slideshow-temp sl)
  (cond
    [(empty? sl)...]
    [(cons? sl)(...(slide-temp (first sl))...
                   (slideshow-temp (rest sl))...)]))

; strong-singularity : Slideshow -> Slideshow
; begins the animation witht the first slide

;(define (strong-singularity sl)
;  (big-bang sl
;    [to-draw draw-sl]
;    [on-key show-hidden-or-next]
;    [stop-when? black?]))

; draw-sl : Slideshow -> Image
; produces an image of the first slide from given slideshow
(check-expect (draw-sl SL-0) BG)
(check-expect (draw-sl SL-1)(overlay (text "Strings \n a \n b \n a" 20 "black") BG))
(check-expect (draw-sl SL-2)(overlay (text "Numbers \n -8 \n 49 \n 5" 20 "black") BG))
(check-expect (draw-sl SL-3)(overlay (text "Strings and Numbers \n a \n b \n a" 20 "black") BG))
(check-expect (draw-sl SL-4)(overlay (text "Numbers \n -8 \n 49 \n 5" 20 "black") BG))
(check-expect (draw-sl SL-5)(overlay (text "Strings \n a \n b \n a" 20 "black") BG))

(define (draw-sl sl)
  (cond
     [(empty? sl) BG]
    [(cons? sl)(overlay (text (str-title-shown (first sl)) 20 "black") BG)]))

; str-title-shown : Slide -> String
; returns a string of title and shown bullet points that of a given slide
(check-expect (str-title-shown S-1) "Strings \n a \n b \n a")
(check-expect (str-title-shown S-2) "Numbers \n -8 \n 49 \n 5")
(check-expect (str-title-shown S-3) "Strings and Numbers \n a \n b \n a")

(define (str-title-shown s)
  (string-append (slide-title s)
                 (cond
                   [(empty? (slide-shown s)) ""]
                   [(cons? (slide-shown s))
                    (cond
                      [(string? (first (slide-shown s)))(bullet-str (slide-shown s))]
                      [(number? (first (slide-shown s)))(bullet-num (slide-shown s))])])))

; bullet-str : Lost -> String
; returns a string listing the strings of the given list of strings
(check-expect (bullet-str LOST-1) " \n a")
(check-expect (bullet-str LOST-3) " \n a \n b \n a")

(define (bullet-str lost)
  (cond
    [(empty? lost) ""]
    [(cons? lost) (string-append " \n " (first lost)
                                 (bullet-str (rest lost)))]))

; bullet- : Lost -> String
; returns a string listing the strings of the given list of strings
(check-expect (bullet-num LON-1) " \n 5")
(check-expect (bullet-num LON-3) " \n -8 \n 49 \n 5")

(define (bullet-num lost)
  (cond
    [(empty? lost) ""]
    [(cons? lost) (string-append " \n " (number->string(first lost))
                                 (bullet-num (rest lost)))]))

; show-hidden-or-next : Slide KeyEvent -> Slide
; returns

; Excercise 6
; a
(define-struct pixel [shade])
; a Feature is a (make-pixel NonNegInt)

(define FEAT-1 (make-pixel 0))
(define FEAT-2 (make-pixel 255))
(define FEAT-3 (make-pixel 100))

; b
; a Row is one of:
; - empty
; - (cons Feature Row)
; Intepretation: represents a sequence of pixels for 1 row of a bitmap

(define ROW-1 empty)
(define ROW-2 (cons FEAT-1 ROW-1))
(define ROW-3 (cons FEAT-2 ROW-2))
(define ROW-4 (cons FEAT-3 ROW-3))
(define ROW-5 empty)
(define ROW-6 (cons FEAT-2 ROW-1))
(define ROW-7 (cons FEAT-1 ROW-2))
(define ROW-8 (cons FEAT-3 ROW-3)) 

; row-temp : Row -> ???
(define (row-temp r)
  (...
   (cond
     [(empty? r)...]
     [(cons? r)(...(first r)...
                   (row-temp (rest r))...)])))
  
; a Bitmap is one of:
; - empty
; - (cons Row Bitmap)
; represents a sequence of rows of features in a bitmap

(define BM-1 empty)
(define BM-2 (cons ROW-4 BM-1))
(define BM-3 (cons ROW-8 BM-2))

; bitmap-temp : Bitmap -> ???
(define (bitmap-temp bm)
  (...
   (cond
     [(empty? bm)...]
     [(cons? bm)(...(first bm)...
                   (bitmap-temp (rest bm))...)])))

; c
; a Instance is one of:
; - empty
; - (cons Feature Instance)
; intepretation: represents a a flattened sequence of features

(define INS-1 empty)
(define INS-2 (cons FEAT-1 INS-1))
(define INS-3 (cons FEAT-2 INS-2))
(define INS-4 (cons FEAT-3 INS-3))

; instance-temp : Instance -> ???
(define (instance-temp ins)
  (...
   (cond
     [(empty? ins)...]
     [(cons? ins)(...(first ins)...
                     (instance-temp (rest ins))...)])))

; d

  








                     
                       

    
    

                   
         
             

                           






