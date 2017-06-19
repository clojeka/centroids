#lang racket
(require "table.rkt")
(require "tree-pic.rkt")

(define table-hard-case
  '((names ("x" "y" "class"))
    (rows
     (("0.5" "0.5" black) ("0.5" "0.5" green)))))

(define (table->binary-table table)
  (table-row-class-map string->number identity table))

(define easy (table->binary-table (csv->table "trivial/trivial-1.csv")))
(define hard (table->binary-table (csv->table "trivial/hard-1.csv")))

(define (render node)
  (let* ([centroid (text (format "~a"
                                 (map (lambda (n) (real->decimal-string n 1)) (first node)))
                         (cons 'bold "Helvetica") 16)]
         [class (text (format "~a"
                              `(,(car (second node)) ,(cdr (second node))))
                      (cons 'bold "Helvetica") 16)])
    (frame (vc-append 0 centroid class))))

(save-pict (table->tree-picture easy render) "slides/easy-tree.png" 'png)
(save-pict (table->tree-picture hard render) "slides/hard-tree.png" 'png)
