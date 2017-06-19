#lang racket
(require "table.rkt")
(require "tree-pic.rkt")

(define (setosa-or-not c)
  (if (not (symbol=? c 'Iris-setosa)) 'Not-setosa 'Iris-setosa))

(define (table->binary-table table)
  (table-row-class-map string->number setosa-or-not table))

(define base1 (table->binary-table (csv->table "iris/training-1.csv")))
(define test1 (table->binary-table (csv->table "iris/test-1.csv")))

(define base2 (table->binary-table (csv->table "iris/training-2.csv")))
(define test2 (table->binary-table (csv->table "iris/test-2.csv")))

(define (render node)
  (let* ([centroid (text (format "~a"
                                 (map (lambda (n) (real->decimal-string n 1)) (first node)))
                         (cons 'bold "Helvetica") 16)]
         [class (text (format "~a"
                              `(,(car (second node)) ,(cdr (second node))))
                      (cons 'bold "Helvetica") 16)])
    (frame (vc-append 0 centroid class))))

(save-pict (table->tree-picture base1 render) "slides/iris-tree.png" 'png)
