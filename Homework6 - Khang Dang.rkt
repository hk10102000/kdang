;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Homework6 - Khang Dang|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Excercise 1

; A Posn is a (make-posn Number Number)
; and represents a coordinate on the x-y plane

(define POSN1 (make-posn 9 12))
(define POSN2 (make-posn 4 3))

; posn-template : Posn -> ???
(define (posn-template p)
  (... (posn-x p) ... (posn-y p) ...))

; A ListOfPosns (LoP) is one of:
; - empty
; - (cons Posn LoP)

(define LOP-EMPTY empty)
(define LOP-FULL (cons POSN1 (cons POSN2 empty)))

; lop-template : LoP -> ???
(define (lop-template lop)
  (cond [(empty? lop) ...]
        [(cons? lop)
         (... (posn-template (first lop))
              (lop-template (rest lop)) ...)]))

; sum-x-coords : [List-of Posn] -> Number
; Sum all the x-coordinates in the list of positions
(check-expect (sum-x-coords empty) 0)
(check-expect (sum-x-coords
               (cons (make-posn 3 4)
                     (cons (make-posn 5 12)
                           empty))) 8)
(define (sum-x-coords lop)
  (cond
    [(empty? lop) 0]
    [(cons? lop)
     (+ (posn-x (first lop))
        (sum-x-coords (rest lop)))]))
 
; mult-distances : [List-of Posn] -> Number
; Multiply all the distances from each position to the origin
(check-expect (mult-distances empty) 1)
(check-expect (mult-distances
               (cons (make-posn 3 4)
                     (cons (make-posn 5 12)
                           empty))) 65)
(define (mult-distances lop)
  (cond
    [(empty? lop) 1]
    [(cons? lop)
     (* (distance-to-origin (first lop))
        (mult-distances (rest lop)))]))
 
; distance-to-origin : Posn -> Number
; Produces the distance from this position to the origin
(check-within (distance-to-origin (make-posn 2 2)) (sqrt 8) 1e-06)
(check-expect (distance-to-origin (make-posn 3 4)) 5)
(define (distance-to-origin p)
  (sqrt (+ (sqr (posn-x p)) (sqr (posn-y p)))))

; do-to-all : [List-of Posn] [Posn -> Number] -> Number
; Return a number by applying the given function to the list
(check-expect (do-to-all empty sum-x-coords) 0)
(check-expect (do-to-all (cons (make-posn 3 4) (cons (make-posn 5 12) empty))
                         sum-x-coords) 8)
(check-expect (do-to-all empty mult-distances) 1)
(check-expect (do-to-all (cons (make-posn 3 4) (cons (make-posn 5 12) empty))
                         mult-distances) 65)
               
(define (do-to-all lop func) (func lop))

; sum-x-coords.v2 : [List-of Posn] -> Number
; Sum all the x-coordinates in the list of positions
(check-expect (sum-x-coords.v2 empty) 0)
(check-expect (sum-x-coords.v2
               (cons (make-posn 3 4)
                     (cons (make-posn 5 12)
                           empty))) 8)
(define (sum-x-coords.v2 lop)
  (cond
    [(empty? lop) 0]
    [(cons? lop)
     (do-to-all lop sum-x-coords)]))

; mult-distances.v2 : [List-of Posn] -> Number
; Multiply all the distances from each position to the origin
(check-expect (mult-distances.v2 empty) 1)
(check-expect (mult-distances.v2
               (cons (make-posn 3 4)
                     (cons (make-posn 5 12)
                           empty))) 65)
(define (mult-distances.v2 lop)
  (cond
    [(empty? lop) 1]
    [(cons? lop)
     (do-to-all lop mult-distances)]))

; b.
; biggest-difference.v0 : [List-of Posn] -> Number
; Returns the largest difference between a posns's x and y values in given list of pons
(check-expect (biggest-difference.v0 empty) 0)
(check-expect (biggest-difference.v0 (cons (make-posn 3 4) (cons (make-posn 5 12) empty))) 7)

(define (biggest-difference.v0 lop)
  (cond [(empty? lop) 0]
        [(cons? lop)
         (max (xy-dif (first lop)) 
             (biggest-difference.v0 (rest lop)))]))

; xy-dif : Posn -> Number
; returns the absolute value of difference between x-posn and y-posn of given posn
(check-expect (xy-dif (make-posn 3 4)) 1)
(check-expect (xy-dif (make-posn 5 12)) 7)
(check-expect (xy-dif (make-posn 12 5)) 7)

(define (xy-dif p)
  (abs (- (posn-x p) (posn-y p))))

; biggest-difference : [List-of Posn] -> Number
; Returns the largest difference between a posns's x and y values in given list of pons
(check-expect (biggest-difference empty) 0)
(check-expect (biggest-difference (cons (make-posn 3 4) (cons (make-posn 5 12) empty))) 7)

(define (biggest-difference lop)
  (cond
    [(empty? lop) 0]
    [(cons? lop)
     (do-to-all lop biggest-difference)]))

; Excercise 2
; my-animate : WorldState

; Excercise 3
; A [NEList-of String] is one of:
; - (cons String empty)
; - (cons String [NEList-of String])
; and represents a list with at least one string

(define NELOStr-0 (cons "hello" empty))
(define NELOStr-1 (cons "hello" (cons "bye" (cons "see ya" empty))))
(define NELOStr-2 (cons "apple" (cons "banana" (cons "orange" empty))))

; nelostr-template : [NEList-of String] -> ???
(define (nelostr-template nelostr)
  (cond [(empty? (rest nelostr)) (... (first nelostr) ...)]
        [(cons? (rest nelostr))
         (... (first nelostr)
              (nelostr-template (rest nelostr)) ...)]))

; earliest : [NEList-of String] [String String -> Boolean] -> String
; Produce a string that comes the earliest based on the condition of given function
(check-expect (earliest NELOStr-1 string<?) "bye")
(check-expect (earliest NELOStr-2 string<?) "apple")
(check-expect (earliest NELOStr-1 string>?) "see ya")
(check-expect (earliest NELOStr-2 string>?) "orange")
(check-expect (earliest NEOLOStr-1 empty-rest?) "see ya")
(check-expect (earliest NEOLOStr-2 empty-rest?) "orange")

(define (earliest nelostr func)
  (cond [(empty? (rest nelostr)) (first nelostr)]
        [(cons? (rest nelostr))
         (if (func (first nelostr) (earliest (rest nelostr) func))
             (first nelostr)
             (earliest (rest nelostr) func))]))

; earliest-lex : [NEList-of String] -> String
; returns the string that comes earliest lexographically in given non-empty list of strings
(check-expect (earliest-lex NELOStr-1) "bye")
(check-expect (earliest-lex NELOStr-2) "apple")

(define (earliest-lex nelostr)
  (earliest nelostr string<?))

; last-lex : [NEList-of String] -> String
; returns the string that comes last lexographically in given non-empty list of strings
(check-expect (last-lex NELOStr-1) "see ya")
(check-expect (last-lex NELOStr-2) "orange")

(define (last-lex nelostr)
  (earliest nelostr string>?))

; last-str : [NEList-of String] -> String
; returns the string that comes last in given non-empty list of strings
(check-expect (last-str NELOStr-1) "see ya")
(check-expect (last-str NELOStr-2) "orange")

(define (last-str nelostr)
  (earliest nelostr empty-rest?))

; empty-rest? : String String -> Boolean
; returns true if the second string is empty
(check-expect (empty-rest? "orange" empty) true)
(check-expect (empty-rest? "apple" "banana") false)

(define (empty-rest? s1 s2)
  (empty? s2))

; Excercise 4
(define-struct cup [oz color material])
 
; A Cup is a (make-cup NonNegNumber String String)
; and represents a cup's capacity in fluid ounces, color, and material
 
(define CUP1 (make-cup 10 "brown" "wood"))
(define CUP2 (make-cup 8 "brown" "ceramic"))
(define CUP3 (make-cup 10 "red" "plastic"))
(define CUP4 (make-cup 6 "clear" "plastic"))

(define CUPS
  (cons CUP1
        (cons CUP2
              (cons CUP3
                    (cons CUP4 empty)))))


(define CUPS-OZ-10 (cons CUP1 (cons CUP3 empty)))
(define CUPS-OZ-8 (cons CUP2 empty))
(define CUPS-OZ-6 (cons CUP4 empty))

(define CUPS-COLOR-BROWN (cons CUP1 (cons CUP2 empty)))
(define CUPS-COLOR-RED (cons CUP3 empty))
(define CUPS-COLOR-CLEAR (cons CUP4 empty))

; A Bin is one of:
; - empty
; - (cons Cup Bin)
; represents a list of cups that have a similar key characteristic

(define BIN-CAPACITY (cons BIN-10 (cons BIN-8 (cons BIN-6 empty))))
(define BIN-COLOR (cons CUPS-COLOR-BROWN (cons CUPS-COLOR-RED (cons CUPS-COLOR-CLEAR empty))))

; create-binning : [List-of Cup] [Cup -> Cup-selector] [Cup Cup -> Boolean] -> Bin
; returns a Bin with a similar key characteristic from the given list of cups
(check-expect (create-binning CUPS oz equal?) BIN-CAPACITY)
(check-expect (creating-binning CUPS cup-color string=?) BIN-COLOR)

(define (create-binning loc key equi?)
  (cond
    [(empty? loc) empty]
    [(cons? loc)
     (if (equi? (cup-key (first loc)) (create-binning (rest loc) key equi?))
         (cons (first loc) 
              

  

