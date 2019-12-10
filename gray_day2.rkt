;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname gray_day2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
; replace-element : list number any -> list
; replace element in a at postion b with c
; where a is a list, b is a position in the list, and c is any value
(define (replace-element a b c)
  (cond
    [(empty? a) empty]
    [(= b 0) (cons c (rest a))]
    [else (cons (first a) (replace-element (rest a) (- b 1) c))]))

(check-expect (replace-element (list 1 2 3) 2 69) (list 1 2 69))

; resolve-pointer : list number -> any
; returns the value of the element that is being pointed to in the element with the position b
; where a is a list, and b is a positon in the list
(define (resolve-pointer a b)
  (list-ref a (list-ref a b)))

(check-expect (resolve-pointer (list 1 6) 0) 6)

; interpret : list number -> list
; interpret program using pc and return the new state of the intcode program
; where program is an intcode program, and pc is a number representing the current instuction that the interpeter is on
(define (interpret program pc)
  (cond
    [(= (list-ref program pc) 99) program]
    [(= (list-ref program pc) 1)
     (interpret (replace-element program (list-ref program (+ pc 3)) (+ (resolve-pointer program (+ pc 1)) (resolve-pointer program (+ pc 2)))) (+ pc 4))]
    [(= (list-ref program pc) 2)
     (interpret (replace-element program (list-ref program (+ pc 3)) (* (resolve-pointer program (+ pc 1)) (resolve-pointer program (+ pc 2)))) (+ pc 4))]))


(check-expect (interpret (list 1 0 0 0 99) 0) (list 2 0 0 0 99))
(check-expect (interpret (list 2 3 0 3 99) 0) (list 2 3 0 6 99))
(check-expect (interpret (list 2 4 4 5 99 0) 0) (list 2 4 4 5 99 9801))
(check-expect (interpret (list 1 1 1 4 99 5 6 0 99) 0) (list 30 1 1 4 2 5 6 0 99))

; brute-force : list number number number -> list
; brute force b and c until output is equal to d using a and return the new state
; where a is an intcode program, b is the value of the noun, c is the value of the verb, and d is the output to achieve 
(define (brute-force a b c d)
  (local [(define calculated (interpret (replace-element (replace-element a 1 b) 2 (+ c 1)) 0))]
    (cond
      [(= (first calculated) d) calculated]
      [(= c 99) (brute-force a (+ b 1) 0 d)]
      [else (brute-force a b (+ c 1) d)])))

; aoc-day2-1 : string num -> num
; takes a file path, input, a output to achieve, output and returns the solution to day 2,
; problem 2.
(define (aoc-day2-2 input output)
  (local [(define program (brute-force (map string->number (first (read-csv-file input))) 0 0 output))]
    (+ (* 100 (second program)) (third program))))

; aoc-day2-1 : string num num -> num
; takes a file path, input, and returns the solution to day 2,
; problem 1.
(define (aoc-day2-1 input)
  (first (interpret (replace-element (replace-element (map string->number (first (read-csv-file input))) 1 12) 2 2) 0)))

(aoc-day2-1 "gray_day2.txt")
(aoc-day2-2 "gray_day2.txt" 19690720)