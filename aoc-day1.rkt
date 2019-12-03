;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname aoc-day1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; module-fuel : number -> number
; takes in a and returns the amount of fuel required to launch the module
; where a is the module mass
(define (module-fuel a)
  (- (floor (/ a 3)) 2))

(check-expect (module-fuel 12) 2)
(check-expect (module-fuel 14) 2)
(check-expect (module-fuel 1969) 654)
(check-expect (module-fuel 100756) 33583)

; total-module-fuel : list-of-list-of-number -> number
; takes in a and returns the total amount of fuel required to launch the modules
; where a is a list of list of mass
(define (total-module-fuel a)
  (apply + (map (lambda (b) (module-fuel (first b))) a)))

(check-expect (total-module-fuel (list (list 12) (list 14) (list 1969) (list 100756))) 34241)

; total-fuel : number -> number
; takes in a and returns the total amount of fuel required to launch the module, accounting for the fuel itself
; where a is the module mass
(define (total-fuel a)
  (local [(define new-fuel (module-fuel a))]
    (if (<= new-fuel 0)
        0
        (+ new-fuel (total-fuel new-fuel)))))

(check-expect (total-fuel 14) 2)
(check-expect (total-fuel 1969) 966)
(check-expect (total-fuel 100756) 50346)

; all-fuel : list-of-list-of-number -> number
; takes in a and returns the total amount of fuel required to launch the modules, accounting for the fuel itself
; where a is a list of list of mass
(define (all-fuel a)
  (apply + (map (lambda (a) (total-fuel (first a))) a)))

(check-expect (all-fuel (list (list 14) (list 1969) (list 100756))) 51314)

; aoc-day1-1 : string -> num
; takes a file path, input, and returns the solution to day 1,
; problem 1.
(define (aoc-day1-1 input)
  (total-module-fuel (read-words-and-numbers/line input)))

(aoc-day1-1 "aoc-day1.txt")

; aoc-day1-2 : string -> num
; takes a file path, input, and returns the solution to day 1,
; problem 2.
(define (aoc-day1-2 input)
  (all-fuel (read-words-and-numbers/line input)))

(aoc-day1-2 "aoc-day1.txt")