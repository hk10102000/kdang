;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Homework5v2-Khang Dang|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Excercise 1

; A List-of-strings is one of:
; - '()
; - (cons String List-of-strings)
; interpretation a list of strings

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
; interpretation a list of numbers

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

; show-hidden-or-next : Slideshow KeyEvent -> Slideshow
; shows the hidden bullets of slide or go to next slide when a key is pressed
(define (show-hidden-or-next sl ke)
  (if (key=? ke "rightarrow")
      (cond
        [(some-hidden? sl)(show-hidden sl)]
        [(all-shown? sl)(next-slide sl)]
        [(last-slide? sl)(black-screen sl)])
      sl))

; some-hidden

; black? : Slideshow -> Boolean
; returns true if the program displays a black-screen
;(define (black? sl)
;  (BLACKSCR
        
           
        


; Excercise 6
; a
(define-struct pixel [shade])
; a Feature is a (make-pixel NonNegInt)

(define F-1 (make-pixel 0))
(define F-2 (make-pixel 255))
(define F-3 (make-pixel 100))

; b
; a Row is one of:
; - empty
; - (cons Feature Row)
; Interpretation: represents a sequence of pixels for 1 row of a bitmap

(define R-1 empty)
(define R-2 (cons F-1 R-1))
(define R-3 (cons F-2 R-2))
(define R-4 (cons F-3 R-3))
(define R-5 empty)
(define R-6 (cons F-2 R-1))
(define R-7 (cons F-1 R-2))
(define R-8 (cons F-3 R-3))

(define R-9 (cons F-1 (cons F-1 (cons F-1 (cons F-3 (cons F-2 (cons F-2 (cons F-1 empty))))))))
(define R-10 (cons F-1 (cons F-1 (cons F-3 (cons F-2 (cons F-1 (cons F-3 (cons F-2 empty))))))))
(define R-11 (cons F-1 (cons F-1 (cons F-2 (cons F-1 (cons F-1 (cons F-3 (cons F-2 empty))))))))
(define R-12 (cons F-1 (cons F-2 (cons F-3 (cons F-1 (cons F-1 (cons F-3 (cons F-2 empty))))))))
(define R-13 (cons F-1 (cons F-2 (cons F-1 (cons F-1 (cons F-1 (cons F-2 (cons F-3 empty))))))))
(define R-14 (cons F-2 (cons F-3 (cons F-1 (cons F-1 (cons F-1 (cons F-2 (cons F-1 empty))))))))
(define R-15 (cons F-2 (cons F-1 (cons F-1 (cons F-1 (cons F-2 (cons F-3 (cons F-1 empty))))))))
(define R-16 (cons F-2 (cons F-3 (cons F-1 (cons F-2 (cons F-1 (cons F-1 (cons F-1 empty))))))))
(define R-17 (cons F-2 (cons F-2 (cons F-2 (cons F-3 (cons F-1 (cons F-1 (cons F-1 empty))))))))

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
; Interpretation: represents a sequence of rows of features in a bitmap

(define BM-1 empty)
(define BM-2 (cons R-4 BM-1))
(define BM-3 (cons R-8 BM-2))

(define BM-4 (cons R-9
                   (cons R-10
                         (cons R-11
                               (cons R-12
                                     (cons R-13
                                           (cons R-14
                                                 (cons R-15
                                                       (cons R-16
                                                             (cons R-17 empty)))))))))) 

; bitmap-temp : Bitmap -> ???
(define (bitmap-temp bm)
  (...
   (cond
     [(empty? bm)...]
     [(cons? bm)(...(row-temp (first bm))...
                   (bitmap-temp (rest bm))...)])))

; c
; a Instance is one of:
; - empty
; - (cons Feature Instance)
; interpretation: represents a a flattened sequence of features

(define INS-1 empty)
(define INS-2 (cons F-1 INS-1))
(define INS-3 (cons F-2 INS-2))
(define INS-4 (cons F-3 INS-3))

(define INS-5 (cons F-2 (cons F-2 (cons F-2 (cons F-3 (cons F-1 (cons F-1 (cons F-1 INS-1))))))))
(define INS-6 (cons F-2 (cons F-3 (cons F-1 (cons F-2 (cons F-1 (cons F-1 (cons F-1 INS-5))))))))
(define INS-7 (cons F-2 (cons F-1 (cons F-1 (cons F-1 (cons F-2 (cons F-3 (cons F-1 INS-6))))))))
(define INS-8 (cons F-2 (cons F-3 (cons F-1 (cons F-1 (cons F-1 (cons F-2 (cons F-1 INS-7))))))))
(define INS-9 (cons F-1 (cons F-2 (cons F-1 (cons F-1 (cons F-1 (cons F-2 (cons F-3 INS-8))))))))
(define INS-10 (cons F-1 (cons F-2 (cons F-3 (cons F-1 (cons F-1 (cons F-3 (cons F-2 INS-9))))))))
(define INS-11 (cons F-1 (cons F-1 (cons F-2 (cons F-1 (cons F-1 (cons F-3 (cons F-2 INS-10))))))))
(define INS-12 (cons F-1 (cons F-1 (cons F-3 (cons F-2 (cons F-1 (cons F-3 (cons F-2 INS-11))))))))
(define INS-13 (cons F-1 (cons F-1 (cons F-1 (cons F-3 (cons F-2 (cons F-2 (cons F-1 INS-12))))))))

; instance-temp : Instance -> ???
(define (instance-temp ins)
  (...
   (cond
     [(empty? ins)...]
     [(cons? ins)(...(first ins)...
                     (instance-temp (rest ins))...)])))

; d
(define-struct training [bitmap/tn instance/tn corresponding-digit])
; interpretation (make-training Bitmap Instance Number) means a training image with
; bitmap representation,
; instance representation and
; corresponding-digit(0-9) of known content

(define TRAINING-0 (make-training BM-4 INS-13 0))

; training-temp : Trainning -> ???
(define (training-temp tn)
  (...(bitmap-temp (training-bitmap/tn tn))...
      (instance-temp (training-instance/tn tn))...
      (corresponding-digit tn))...)

(define-struct testing [bitmap/tt instance/tt])
; interpretation (make-testing Bitmap Instance) means a testing image with
; bitmap representation and
; instance representation

(define TESTING-0 (make-testing BM-4 INS-13))

; testing-temp : Testing -> ???
(define (testing-temp tt)
  (...(bitmap-temp (testing-bitmap/tt tn))...
      (instance-temp (testing-instance/tt tn))...))

; e
(define-struct neighbor [training distance])
; intepretation (make-neighbor Training NonNegInt) means a
; training image and
; distance between that training image and a testing image in question.

(define NEIGHBOR-0 (make-neighbor TRAINING-0 2729))

; neighbor-temp : Neighbor -> ???
(define (neighbor-temp nb)
  (...(neighbor-training nb)...
      (neighnor-distance nb)...))
 


  








                     
                       

    
    

                   
         
             

                           






