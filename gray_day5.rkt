#lang racket
(require 2htdp/batch-io)
; replace-element : list number any -> list
; replace element in a at postion b with c
; where a is a list, b is a position in the list, and c is any value
(define (replace-element a b c)
  (cond
    [(empty? a) empty]
    [(= b 0) (cons c (rest a))]
    [else (cons (first a) (replace-element (rest a) (- b 1) c))]))

; split-string : string -> list-of-string
; takes a and returns a list of strings containing the charatcers in a
; where a is any string
(define (split-string a)
  (cond
    [(equal? a "") empty]
    [(string? a) (cons (substring a 0 1) (split-string (substring a 1)))]))

; split-number : number -> list-of-number
; takes a and returns a list of numbers containing the digits in a
; where a is any number
(define (split-number a)
  (map string->number (split-string (number->string a))))

; resolve-pointer : list number -> any
; returns the value of the element that is being pointed to in the element with the position b
; where a is a list, and b is a positon in the list
(define (resolve-pointer a b)
  (list-ref a (list-ref a b)))

; parameter-types : number number -> list-of-number
; takes in a and returns a list which has all of the parameter types bases on b
; where a is a opcode, and b is a number representing the amount of parameters
(define (parameter-types a b)
  (local [(define parameters (reverse (split-number (quotient a 100))))]
    (append parameters (build-list (- b (length parameters)) (lambda (a) 0)))))

; resolve-parameter : number list number -> number
; takes in a and c and uses b to return the value of the parameter
; where a is a parameter type, b is a program, and c is a posititon in the list where the parameter is located
(define (resolve-parameter a b c)
  (cond
    [(= a 0) (resolve-pointer b c)]
    [(= a 1) (list-ref b c)]))
      
; interpret : list number -> list
; interpret program using pc and return the new state of the intcode program
; where program is an intcode program, and pc is a number representing the current instuction that the interpeter is on
(define (interpret program pc)
  (local [(define opcode (list-ref program pc))]    
    (cond
      [(= (modulo opcode 100) 99) program]
      [(= (modulo opcode 100) 1)
       (interpret (replace-element program (list-ref program (+ pc 3)) (+ (resolve-parameter (first (parameter-types opcode 3)) program (+ pc 1)) (resolve-parameter (second (parameter-types opcode 3)) program (+ pc 2)))) (+ pc 4))]
      [(= (modulo opcode 100) 2)
       (interpret (replace-element program (list-ref program (+ pc 3)) (* (resolve-parameter (first (parameter-types opcode 3)) program (+ pc 1)) (resolve-parameter (second (parameter-types opcode 3)) program (+ pc 2)))) (+ pc 4))]
      [(= (modulo opcode 100) 3) (interpret (replace-element program (list-ref program (+ pc 1)) (begin (print "Input Required:") (read))) (+ pc 2))]
      [(= (modulo opcode 100) 4) (begin (print (number->string (resolve-parameter (first (parameter-types opcode 1)) program (+ pc 1)))) (newline) (interpret program (+ pc 2)))]
      [(= (modulo opcode 100) 5) (if (not (= (resolve-parameter (first (parameter-types opcode 2)) program (+ pc 1)) 0))
                                     (interpret program (resolve-parameter (second (parameter-types opcode 2)) program (+ pc 2)))
                                     (interpret program (+ pc 3)))]
      [(= (modulo opcode 100) 6) (if (= (resolve-parameter (first (parameter-types opcode 2)) program (+ pc 1)) 0)
                                     (interpret program (resolve-parameter (second (parameter-types opcode 2)) program (+ pc 2)))
                                     (interpret program (+ pc 3)))]
      [(= (modulo opcode 100) 7) (if (< (resolve-parameter (first (parameter-types opcode 3)) program (+ pc 1)) (resolve-parameter (second (parameter-types opcode 3)) program (+ pc 2)))
                                     (interpret (replace-element program (list-ref program (+ pc 3)) 1) (+ pc 4))
                                     (interpret (replace-element program (list-ref program (+ pc 3)) 0) (+ pc 4)))]
      [(= (modulo opcode 100) 8) (if (= (resolve-parameter (first (parameter-types opcode 3)) program (+ pc 1)) (resolve-parameter (second (parameter-types opcode 3)) program (+ pc 2)))
                                     (interpret (replace-element program (list-ref program (+ pc 3)) 1) (+ pc 4))
                                     (interpret (replace-element program (list-ref program (+ pc 3)) 0) (+ pc 4)))])))

; aoc-day5-both : string -> num
; takes a file path, input, and returns the solution to both problems in day 5.
(define (aoc-day5-both input)
 (interpret (map string->number (first (read-csv-file input))) 0))

(aoc-day5-both "gray_day5.txt"); Input 1 when prompted.
(aoc-day5-both "gray_day5.txt"); Input 5 when prompted.