#lang racket

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

; increasing-or-same? : list-of-number -> boolean
; takes a and returns a boolean whether the number has digits that are increasing or same
; where a is list-of-number
(define (increasing-or-same? a)
  (cond
    [(empty? (rest a)) true]
    [(>= (second a) (first a)) (increasing-or-same? (rest a))]
    [(< (second a) (first a)) false]))

; two-adjacent? : list-of-number -> boolean
; takes a and returns a boolean whether the number has two equal adjacent digits
; where a is list-of-number
(define (two-adjacent? a)
  (cond
    [(empty? (rest a)) false]
    [(= (second a) (first a)) true]
    [else (two-adjacent? (rest a))]))

; valid? : number -> boolean
; takes a and returns a boolean whether the number is a valid password
; where a is any six digit number
(define (valid? a)
  (and
   (increasing-or-same? (split-number a))
   (two-adjacent? (split-number a))))

; brute-force : number number function -> list-of-number
; takes a and returns a list of valid passwords (using c) by incrementing a until a is equal to b
; where a is any six digit number, b is any six digit number, and c is a function that takes in a six digit number
(define (brute-force a b c)
  (cond
    [(= a b) empty]
    [(c a) (cons a (brute-force (+ a 1) b c))]
    [else (brute-force (+ a 1) b c)]))

; repeats-for : list-of-number -> number
; takes a and returns the amount of times that that b is repeated in adjacent digits, starting from the first digit
; where a is list-of-number, and b is a number
(define (repeats-for a b)
  (cond
    [(or (empty? a) (not (= (first a) b))) 0]
    [(= (first a) b) (+ 1 (repeats-for (rest a) b))]))

; only-two-adjacent? : list-of-number -> boolean
; takes a and returns a boolean whether the number has two equal adjacent digits, which are not a part of a larger group
; where a is list-of-number
(define (only-two-adjacent? a)
  (cond
    [(empty? a) false]
    [(> (repeats-for a (first a)) 2) (only-two-adjacent? (list-tail a (repeats-for a (first a))))]
    [(= (repeats-for a (first a)) 2) true]
    [else (only-two-adjacent? (rest a))]))

; valid-2? : number -> boolean
; takes a and returns a boolean whether the number is a valid password, with the new criteria
; where a is any six digit number
(define (valid-2? a)
  (and
   (increasing-or-same? (split-number a))
   (only-two-adjacent? (split-number a))))

; aoc-day4-1 : string -> number
; takes a range, input, and returns the solution to day 4,
; problem 1.
(define (aoc-day4-1 input)
  (length (brute-force (+ (string->number (first (string-split input "-"))) 1) (string->number (second (string-split input "-"))) valid?)))

; aoc-day4-2 : string -> number
; takes a range, input, and returns the solution to day 4,
; problem 2.
(define (aoc-day4-2 input)
  (length (brute-force (+ (string->number (first (string-split input "-"))) 1) (string->number (second (string-split input "-"))) valid-2?)))

(aoc-day4-1 "134564-585159")
(aoc-day4-2 "134564-585159")