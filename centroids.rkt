#lang racket/base

(require racket/bool)
(require racket/list)
(require racket/local)
(require "table.rkt")

;; A class is a List-of symbol.  For example, (black blue) is a class
;; of two values, black and blue.

(define trivial-case
  '((0 1 black) (0 2 black) (0 3 black) (0 4 black)
    (1 1 black) (1 2 black) (1 3 black) (1 4 black)
    (3 1 green) (3 2 green) (3 3 green) (3 4 green)
    (4 1 green) (4 2 green) (4 3 green) (4 4 green)))

(define (drop-last ls)
  (take ls (sub1 (length ls))))

(define trivial-case-two trivial-case)
(define trivial-case-one (map (lambda (p) (append p '(black))) (map drop-last trivial-case)))

(define table-trivial-case-one
  `((names ("x" "y" "class")) (rows ,trivial-case-one)))
(define table-trivial-case-two
  `((names ("x" "y" "class")) (rows ,trivial-case-two)))
(define table-trivial-case-empty 
  `((names ("x" "y" "class")) (rows ,empty)))

(define real-case-one 
  '((0 0 green) (0 1 black) (0 2 black) (0 3 black) (0 4 black)
    (1 0 green) (1 1 black) (1 2 black) (1 3 black) (1 4 black)
    (3 0 black) (3 1 green) (3 2 green) (3 3 green) (3 4 green)
    (4 0 black) (4 1 green) (4 2 green) (4 3 green) (4 4 green)))
(define table-real-case-one   `((names ("x" "y" "class")) (rows ,real-case-one)))

(define table-hard-case-one 
  `((names ("x" "y" "class")) 
    (rows ((0 1 black) (0 2 black) (0 3 black) (0 4 black)
           (1 1 green) (1 2 green) (1 3 green) (1 4 green)
           (3 1 black) (3 2 black) (3 3 black) (3 4 black)
           (4 1 green) (4 2 green) (4 3 green) (4 4 green)))))

(define table-hard-case-two
  `((names ("x" "y" "class")) 
    (rows ((0 1 black) (0 2 black) (0 3 black) (0 4 green)
           (1 1 green) (1 2 green) (1 3 green) (1 4 green)
           (3 1 black) (3 2 black) (3 3 black) (3 4 black)
           (4 1 green) (4 2 green) (4 3 green) (4 4 black)))))

(module+ test
  (require rackunit)
  (check-equal? (euclidean-distance '(0 0) '(0 0)) 0)
  (check-equal? (euclidean-distance '(1 1) '(2 2)) (sqrt 2)))

(define (distance t1 t2)
  (euclidean-distance t1 t2))

(define (sum ls)
  (foldl + 0 ls))

(define (euclidean-distance t1 t2)
  (sqrt (sum (squared (errors t1 t2)))))

(define (squared ls)
  (define (square-of n) (* n n))
  (map square-of ls))

(define (errors t1 t2)
   (define (distance-between x y)
     (abs (- x y)))
   (for/list ([x (in-list t1)]
              [y (in-list t2)])
     (distance-between x y)))

(module+ test
  (check-equal? (filter-by-class 'black table-trivial-case-two)
                '((names ("x" "y" "class"))
                  (rows ((0 1 black) (0 2 black) (0 3 black) (0 4 black)
                         (1 1 black) (1 2 black) (1 3 black) (1 4 black)))))
  (check-equal? (filter-by-class 'green table-trivial-case-two)
                '((names ("x" "y" "class"))
                  (rows ((3 1 green) (3 2 green) (3 3 green) (3 4 green)
                         (4 1 green) (4 2 green) (4 3 green) (4 4 green))))))

(define (filter-by-class c table)
  (define (is-of-class-c? tuple)
    (symbol=? c (last tuple)))
  `((names ,(table->names table))
    (rows ,(filter is-of-class-c? (table->rows table)))))

(define partition-by-class filter-by-class)

;; The centroid of a list of points LS is a point C whose ith
;; coordinate is the arithmetic mean of the ith-coordinate of each
;; point in LS.

(module+ test
  (check-equal? (centroid-of-n-coords '((0 0) (0 0) (0 0)) 2) '(0 0))
  (check-equal? (centroid-of-n-coords '((0 1) (0 2) (0 3)) 2) '(0 2))
  (check-equal? (centroid-of-n-coords '((0 1) (0 2) (0 3) (0 4)) 2) '(0 10/4))
  (check-equal? (average-of-ith-coordinates 0 '((0 0) (0 0) (0 0))) 0)
  (check-equal? (average-of-ith-coordinates 1 '((0 1) (0 2) (0 3))) 2)
  (check-equal? (average-of-ith-coordinates 1 '((0 1) (0 2) (0 3) (0 4))) 10/4)
  (check-equal? (centroid-of-tuples (table->rows (filter-by-class 'green table-trivial-case-two))) '(7/2 10/4))
  (check-equal? (centroid-of-tuples (table->rows (filter-by-class 'black table-trivial-case-two))) '(1/2 10/4))
  (check-equal? (centroid-of-table `((names (x y z)) (rows ,trivial-case-one))) '(2 5/2)))

(define (average ls-of-numbers)
  (/ (sum ls-of-numbers) (length ls-of-numbers)))

(define (ith-coordinates i ls-of-tuples)
    (map (lambda (tuple) (list-ref tuple i)) ls-of-tuples))

(define (average-of-ith-coordinates i ls-of-tuples)
  (average (ith-coordinates i ls-of-tuples)))

(define (centroid-of-n-coords ls-of-tuples n-coords)
  (for/list ([i (build-list n-coords values)])
    (average-of-ith-coordinates i ls-of-tuples)))

(define (centroid-of-tuples ls-of-tuples)
  (centroid-of-n-coords ls-of-tuples (length (drop-last (first ls-of-tuples)))))

(define (centroid-of-table table)
  (centroid-of-tuples (table->rows table)))

;; A Centroid is a point of the form '(x y) where x, y are rational
;; numbers.

;; A CentroidTree is either
;; - empty OR
;; (struct Centroid CentroidTree CentroidTree)

(define-struct centroid-tree (node left right) #:transparent)

(module+ test
  (define tree-empty empty)
  (define tree-trivial-case-one 
    (make-centroid-tree (centroid-of-tuples trivial-case-one) empty empty))
  (define tree-trivial-case-two 
    (make-centroid-tree (centroid-of-tuples trivial-case-one)
                        (make-centroid-tree (centroid-of-tuples '((0 1 black) (0 2 black) (0 3 black) (0 4 black)
                                                                  (1 1 black) (1 2 black) (1 3 black) (1 4 black)))
                                            empty
                                            empty)
                        (make-centroid-tree (centroid-of-tuples '((3 1 green) (3 2 green) (3 3 green) (3 4 green)
                                                                  (4 1 green) (4 2 green) (4 3 green) (4 4 green)))
                                            empty
                                            empty)))

  (check-equal? (centroid-tree-maker table-trivial-case-empty 'c1 'c2) tree-empty)
  (check-equal? (centroid-tree-maker table-trivial-case-one 'black 'c2) tree-trivial-case-one)
  (check-equal? (centroid-tree-maker table-trivial-case-two 'black 'green) tree-trivial-case-two)
  (check-equal? (table-pure? table-trivial-case-one) true)
  (check-equal? (table-pure? `((names (x y z)) (rows ,trivial-case-two))) false))

(define (table-pure? table)
  ;; A table is pure if and only if all of its tuples are of the same class.
  (cond [(table-empty? table) true]
        [else (= 1 (length (column-values (last (table->names table)) table)))]))

(define (centroid-of-table-with-class c table)
  (let ([filtered-table (filter-by-class c table)])
    (cond [(empty? (table->rows filtered-table)) null]
          [else (centroid-of-table filtered-table)])))   

(define (nearby centroid another table)
  ;; Produces a table of tuples which are closer to CENTROID than to
  ;; ANOTHER centroid.
  (let ([empty-table  `((names ,(table->names table)) (rows ()))])
    (cond [(null? centroid) (table-empty (table->names table))]
          ((null? another) (table-empty (table->names table)))
          [else (nearby-helper centroid another table)])))

(define (nearby-helper centroid another table)
  (let ([closer-to-centroid? 
         (lambda (t) (< (distance (drop-last t) centroid) 
                        (distance (drop-last t) another)))])
    `((names ,(table->names table))
      (rows ,(filter closer-to-centroid? (table->rows table))))))

(define (count-votes table c1 c2)
  (list (list (count (lambda (t) (equal? (last t) c1)) (table->rows table)) c1)
        (list (count (lambda (t) (equal? (last t) c2)) (table->rows table)) c2)))

(define (majority table c1 c2)
  ;; The tie-breaker here is a random selection.
  (let* ([votes (count-votes table c1 c2)]
         [candidate-a (first votes)]
         [candidate-b (second votes)])
    (if (equal? (first candidate-a) (first candidate-b))
        (first (shuffle votes))
        (argmax car votes))))

(define (centroid-tree-maker table c1 c2)
  ;; The Centroid Tree Maker Algorithm 
  (cond [(table-empty? table) empty]
        [(table-pure? table) 
         (let* ([col-class (last (table->names table))]
                [pure-class (first (column-values col-class table))]
                [node (cons (count-val pure-class (column-by-name col-class table)) pure-class)])
           (make-centroid-tree `(,(centroid-of-table table) ,node) empty empty))]
        [else
         (let* ([col-class (last (table->names table))]
                [major-class (cadr (majority table c1 c2))]
                [node (cons (count-val major-class (column-by-name col-class table)) major-class)])
           (make-centroid-tree `(,(centroid-of-table table) ,node) ; node
                               ; left
                               (centroid-tree-maker (nearby (centroid-of-table-with-class c1 table) 
                                                            (centroid-of-table-with-class c2 table) table) c1 c2)
                               ; right
                               (centroid-tree-maker (nearby (centroid-of-table-with-class c2 table) 
                                                            (centroid-of-table-with-class c1 table) table) c1 c2)))]))

(define (table->centroid-tree table)
  (let* ([classes (column-values (last (table->names table)) table)]
         [c1 (first classes)]
         [c2 (if (= 1 (length classes)) c1 (second classes))])
    (centroid-tree-maker table c1 c2)))

(provide (all-defined-out))
