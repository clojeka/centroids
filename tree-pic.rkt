#lang racket/base

(require pict/tree-layout)
(require pict)
(require racket/class)
(require racket/list)
(require racket/bool)
(require "centroids.rkt")

(define trivial-case
  '((0 1 black) (0 2 black) (0 3 black) (0 4 black)
    (1 1 black) (1 2 black) (1 3 black) (1 4 black)
    (3 1 green) (3 2 green) (3 3 green) (3 4 green)
    (4 1 green) (4 2 green) (4 3 green) (4 4 green)))

(define trivial-case-two trivial-case)
(define trivial-case-one (map (lambda (p) (append p '(black))) (map drop-last trivial-case)))

(define table-trivial-case-one
  `((names (x y class)) (rows ,trivial-case-one)))
(define table-trivial-case-two
  `((names (x y class)) (rows ,trivial-case-two)))
(define table-trivial-case-empty 
  `((names (x y class)) (rows ,empty)))

(define real-case-one 
  '((0 0 green) (0 1 black) (0 2 black) (0 3 black) (0 4 black)
    (1 0 green) (1 1 black) (1 2 black) (1 3 black) (1 4 black)
    (3 0 black) (3 1 green) (3 2 green) (3 3 green) (3 4 green)
    (4 0 black) (4 1 green) (4 2 green) (4 3 green) (4 4 green)))
(define table-real-case-one   `((names (x y class)) (rows ,real-case-one)))

(define table-real-case-two
  `((names (x y class))
    (rows ((0 0 black) (0 1 black) (0 2 black) (0 3 black) (0 4 black)
            (1 0 black) (1 1 black) (1 2 black) (1 3 black) (1 4 black)
            (3 0 black) (3 1 green) (3 2 green) (3 3 green) (3 4 green)
            (4 0 black) (4 1 green) (4 2 green) (4 3 green) (4 4 green)))))

(define table-hard-case-one 
  `((names (x y class)) 
    (rows ((0 1 black) (0 2 black) (0 3 black) (0 4 black)
           (1 1 green) (1 2 green) (1 3 green) (1 4 green)
           (3 1 black) (3 2 black) (3 3 black) (3 4 black)
           (4 1 green) (4 2 green) (4 3 green) (4 4 green)))))

(define table-hard-case-two
  `((names (x y class)) 
    (rows ((0 1 black) (0 2 black) (0 3 black) (0 4 green)
           (1 1 green) (1 2 green) (1 3 green) (1 4 green)
           (3 1 black) (3 2 black) (3 3 black) (3 4 black)
           (4 1 green) (4 2 green) (4 3 green) (4 4 black)))))

(define table-hard-case-3
  `((names ("x" "y" "class"))
    (rows (("0.5" "0.5" "black")
           ("0.5" "0.5" "green")))))

(define (render-node node)
  (let* ([centroid (text (format "~a" (first node)) (cons 'bold "Helvetica") 16)]
         [class (text (format "~a" (second node)) (cons 'bold "Helvetica") 16)])
    (frame (vc-append 0 centroid class))))
  
(define (centroid-tree->tree-layout ct [render render-node])
  ;; Consumes a CentroidTree and produces its exact match as a
  ;; TreeLayout --- so it can be rendered by drawing functions.
  (cond [(empty? ct) false]
        [else 
         (let ([node (render (centroid-tree-node ct))])
           (tree-layout #:pict node 
                        (if (empty? (centroid-tree-left ct))
                            false
                            (tree-edge #:edge-width 2
                                       (centroid-tree->tree-layout (centroid-tree-left ct) render)))
                        (if (empty? (centroid-tree-left ct))
                            false
                            (tree-edge #:edge-width 2
                                       (centroid-tree->tree-layout (centroid-tree-right ct) render)))))]))

(define (table->tree-picture table [render render-node])
  (naive-layered (centroid-tree->tree-layout (table->centroid-tree table) render)))

(define (save-pict p name type)
  (let ([bm (pict->bitmap p)])
    (send bm save-file name type)))

(provide (all-defined-out) text frame vc-append)
