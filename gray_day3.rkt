#lang racket
(require lang/posn)
(require 2htdp/batch-io)

; An intersection is a list containing
; - the point of intersection, represented as a posn
; - the point at the beginning of the 1st segment containing the intersction, represented as a posn
; - the point at the beginning of the 2nd segment containing the intersection, represented as a posn
; The extra data above is helpful when solving part 2

; Begin Shared Code

; Made fresh:tm: by Lleyton, Joel, and Iggy
; Max contributed a negative amount of moral support (He actually contributed nothing though.)

; ccw? : posn posn posn -> boolean
; determines if the points are listed in a counter clockwise order using a, b, c
; where a, b, c are all posns
(define (ccw? a b c)
  (> (* (- (posn-y c) (posn-y a)) (- (posn-x b) (posn-x a))) (* (- (posn-y b) (posn-y a)) (- (posn-x c) (posn-x a)))))

; intersects? : posn posn posn posn -> boolean
; determines if the lines intersect using a, b, c, d
; where a, b, c, d are all posns
(define (intersects? a b c d)
  (and (not (equal? (ccw? a c d) (ccw? b c d))) (not (equal? (ccw? a b c) (ccw? a b d)))))

; get-intersection : posn posn posn posn -> intersection
; gets the intersecion point using a, b, c, d
; where a, b, c, d are all posns
(define (get-intersection a b c d)
  (if (intersects? a b c d)
      (if (= (posn-x a) (posn-x b))
          (list (make-posn (posn-x a) (posn-y c)) a c)
          (list (make-posn (posn-x c) (posn-y a)) a c))
      false))

; End Shared Code

; manhattan-distance : posn posn -> number
; calculates the manhattan distance using a and b
; where a and b are posns
(define (manhattan-distance a b)
  (+ (abs (- (posn-x a) (posn-x b))) (abs (- (posn-y a) (posn-y b)))))

; get-points : list-of-posns list-of-strings -> list-of-posns
; takes in a and applies the directions in b and returns the new list
; where a is a list containing the known existing points, b is a list containing directions
(define (apply-directions a b)
  (cond
    [(empty? b) a]
    [(cons? b) (apply-directions
                (append a (list (cond
                                  [(equal? (string-ref (first b) 0) #\U) (make-posn (posn-x (last a)) (+ (posn-y (last a)) (string->number (substring (first b) 1))))]
                                  [(equal? (string-ref (first b) 0) #\D) (make-posn (posn-x (last a)) (- (posn-y (last a)) (string->number (substring (first b) 1))))]
                                  [(equal? (string-ref (first b) 0) #\L) (make-posn (- (posn-x (last a)) (string->number (substring (first b) 1))) (posn-y (last a)))]
                                  [(equal? (string-ref (first b) 0) #\R) (make-posn (+ (posn-x (last a)) (string->number (substring (first b) 1))) (posn-y (last a)))])))
                (rest b))]))

; calculate-intersections : list-of-posns posn posn -> list-of-intersection
; takes in a and calculates intersecting points with the line segment determined by b and c
; where a is a list containing posns repsresenting connected line segments, b is the 1st endpoint of the line segment, c is a 2nd endpoint of the line segment
(define (calculate-intersections a b c)
  (cond
    [(= (length a) 1) empty]
    [(cons? a) (if (intersects? (first a) (second a) b c)
                   (cons (get-intersection (first a) (second a) b c) (calculate-intersections (rest a) b c))
                   (calculate-intersections (rest a) b c))]))

; calculate-all-intersections : list-of-posns list-of-posns -> list-of-intersection
; takes in a and b and calculates all of the intersections
; where a is a list containing posns repsresenting connected line segments, and b is a list containing posns repsresenting connected line segments
(define (calculate-all-intersections a b)
  (cond
    [(= (length a) 1) empty]
    [(cons? a) (append (calculate-intersections b (first a) (second a)) (calculate-all-intersections (rest a) b))]))

; sort-by-manhattan-distance : list-of-intersection -> list-of-intersection
; takes in a and sorts a by manhattan-distance
; where a is a list of intersections
(define (sort-by-manhattan-distance a)
  (sort (map first a) (lambda (a b) (< (manhattan-distance (make-posn 0 0) a) (manhattan-distance (make-posn 0 0) b)))))

; calculate-steps : list-of-posns -> number
; takes in a and calculates the amount of steps in a
; a is a list containing posns repsresenting connected line segments
(define (calculate-steps a)
  (cond
    [(= (length a) 1) 0]
    [(cons? a) (+ (manhattan-distance (first a) (second a)) (calculate-steps (rest a)))]))

; path-to-intersection : 
(define (path-to-intersection a b c)
  (cond
    [(equal? (first a) b) (cons b (cons c empty))]
    [(cons? a) (cons (first a) (path-to-intersection (rest a) b c))]))

; sort-by-best-intersection : list-of-posns list-of-posns list-of-intersection -> list-of-intersection
; takes in line1 and line2 and intersections, and sorts intersections by the amount of steps
; line1 is a list containing posns repsresenting connected line segments, line2 is a list containing posns repsresenting connected line segments, and intersections is a list of intersection 
(define (sort-by-best-intersection line1 line2 intersections)
  (sort intersections
        (lambda (a b)
          (< (+ (calculate-steps (path-to-intersection line1 (third a) (first a))) (calculate-steps (path-to-intersection line2 (second a) (first a))))
             (+ (calculate-steps (path-to-intersection line1 (third b) (first b))) (calculate-steps (path-to-intersection line2 (second b) (first b))))))))

; aoc-day3-1 : string num -> num
; takes a file path, input, a output to achieve, output and returns the solution to day 3,
; problem 1.
(define (aoc-day3-1 input)
  (local [(define contents (read-csv-file input))]
    (manhattan-distance (make-posn 0 0) (first (sort-by-manhattan-distance (calculate-all-intersections (apply-directions (list (make-posn 0 0)) (first contents)) (apply-directions (list (make-posn 0 0)) (second contents))))))))


; aoc-day3-2 : string num -> num
; takes a file path, input, a output to achieve, output and returns the solution to day 3,
; problem 2.
(define (aoc-day3-2 input)
  (local [(define contents (read-csv-file input)) (define line1 (apply-directions (list (make-posn 0 0)) (first contents))) (define line2 (apply-directions (list (make-posn 0 0)) (second contents)))]
    (local [(define intersection (first (sort-by-best-intersection line1 line2 (calculate-all-intersections line1 line2))))]
      (+ (calculate-steps (path-to-intersection line2 (second intersection) (first intersection))) (calculate-steps (path-to-intersection line1 (third intersection) (first intersection)))))))

(aoc-day3-1 "gray_day3.txt")
(aoc-day3-2  "gray_day3.txt")