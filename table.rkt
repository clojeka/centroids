#lang racket/base

(require csv-reading)
(require racket/bool)
(require racket/list)
(require racket/string)

(define-struct table (attrs classes tuples) #:transparent)
;; examples
;; (make-table '(x y class) '(black green) '((1.1 1.2 black) (0 0 green)))

(define (table2-trivial-case-one)
  (make-table '(x y class) '(black green)
              '((0 1 black) (0 2 black) (0 3 black) (0 4 black)
                (1 1 black) (1 2 black) (1 3 black) (1 4 black)
                (3 1 green) (3 2 green) (3 3 green) (3 4 green)
                (4 1 green) (4 2 green) (4 3 green) (4 4 green))))

(define (table2-empty? table)
  (empty? (table-tuples table)))

(define (table2-filter-by-class c table)
  (define (is-of-class-c? tuple)
    (symbol=? c (last tuple)))
  (make-table (table-attrs table) (table-classes table)
              (filter is-of-class-c? (table-tuples table))))

(define (table2-empty table)
  (make-table (table-attrs table) (table-classes table) empty))

(define (table2-class-of tuple)
  (last tuple))

(define (table2-pure? table)
  (if (<= (length (table2-column-values (last (table-attrs table)) table)) 1)
      true 
      false))

(define (table2-invert-selection table-selected table-source)
  (define (not-in-selection? t)
    (not (member t (table-tuples table-selected))))
  (make-table (table-attrs table-source) 
              (table-classes table-source)
              (filter not-in-selection? (table-tuples table-source))))

(define (table2-length table)
  (length (table-tuples table)))

(define (csv->table file)
  (let ([data (csv->list (make-csv-reader (open-input-file file)))])
    `((names ,(first data))
      (rows ,(rest data)))))

(define (table->rows table)
  (second (assoc 'rows table)))

(define (table->names table)
  (second (assoc 'names table)))

(define (table-empty? table)
  (empty? (second (assoc 'rows table))))

(define (table-empty names)
  `((names ,names)
    (rows ,empty)))

(define (table-length table)
  (length (table->rows table)))

(define (table-width table)
  (length (table->names table)))

(define (table->classes table)
  (let ([col (last (table->names table))])
    (sort (column-values col table) symbol<?)))

(define (table->csv table name [sep ","])
  ;; (define (print/out s)
  ;;   (print (string-append s "\r\n")))
  (with-output-to-file name #:exists 'replace 
    (lambda ()
      (printf "~a\r\n"(string-append (string-join (table->names table) sep)))
      (for ([row (table->rows table)])
        (printf "~a\r\n" (string-join row sep))))))

(define (drop-last ls)
  (take ls (sub1 (length ls))))

;; table transformations
(define (table-row-map f table)
  `((names ,(table->names table))
    (rows ,(map (lambda (t) (append (map f (drop-last t)) (list (string->symbol (last t)))))
                (table->rows table)))))

(define (table-row-class-map f g table)
  ;; Takes two functions F, G and maps F(tuple) G(class) onto the TABLE.
  `((names ,(table->names table))
    (rows ,(map (lambda (t) (append (map f (drop-last t)) (list (g (string->symbol (last t))))))
                (table->rows table)))))

;; partitions
(define (partitions->csv ps dir)
  (for ([p (in-list ps)]
        [i (in-naturals 1)])
    (let ([base (string-append dir "/" "training-" (number->string i) ".csv")]
          [test (string-append dir "/" "test-" (number->string i) ".csv")])
      (table->csv (first p) base ",")
      (table->csv (second p) test ","))))

(define (10-fold-partitions table)
  (for/list ([i (in-naturals 1)]
             #:break (> i 10))
    (let-values ([(base test) (k-fold i 10 table)])
      `(,base ,test))))

(define (k-fold seed k table)
  (let* ([len (table-length table)]
         [training-size (ceiling (* len (/ (- 100 k) 100)))]
         [training (k-random-list seed training-size len)]
         [test (k-random-list-revert len training)]
         [training+1 (map add1 training)]
         [test+1 (map add1 test)])
    (values 
     `((names ,(table->names table))
       (rows ,(map (lambda (n) (nth-tuple n table)) training+1)))
     `((names ,(table->names table))
       (rows ,(map (lambda (n) (nth-tuple n table)) test+1))))))

(module+ test
    (require rackunit)
    (define table (csv->table "iris/iris.csv"))

    (let-values ([(base test) (k-fold 1 10 table)])
      (check-equal? (+ (table-length base) (table-length test)) (table-length table)))

    (let-values ([(base test) (k-fold 1 50 table)])
      (check-equal? (table-length base) (ceiling (/ (table-length table) 2))))

    (let-values ([(base test) (k-fold 1 0 table)])
      (check-equal? (table-length base) (table-length table))
      (check-equal? (table-length test) 0)))

;; (define (k-random k m)
;;   ;; Produces K natural numbers, each in [1,M].
;;   (random-seed 1) ;; We need the same selection for any random partition
;;   (for/list ([i (in-naturals 1)] #:break (> i k)) (random 1 m)))

(define (k-random-list-revert max-value source)
  (let ([is-not-in-source? (lambda (i) (not (member i source)))])
    (filter is-not-in-source? (build-list max-value values))))

(define (k-random-list seed k m)
  (random-seed seed)
  (k-random-list-helper k m empty))

(module+ test
    (check-equal? (length (k-random-list-revert 150 (k-random-list 1 140 150))) 10))

(define (k-random-list-helper k m ls-of-int)
  (cond 
    [(> k m) (error "impossible list")]
    [(= (length ls-of-int) k) ls-of-int] ; base case
    [else
     (let ([r (random m)])
       (if (member r ls-of-int)
           (k-random-list-helper k m ls-of-int) ; try again
           (k-random-list-helper k m (cons r ls-of-int))))]))

(define (nth-tuple n table)
  ;; Produces the N-th tuple of TABLE.
  (list-ref (table->rows table) (sub1 n)))

(define (slice from to table)
  `((names ,(table->names table))
    (rows ,(for/list ([i (in-naturals from)] #:break (> i to))
            (nth-tuple i table)))))

(define (head table [size 10])
  (partition 1 size table))

(define (tail table [size 10])
  (let ([len (table-length table)])
    (partition (add1 (- len size)) len table)))

;; columns
(define (column-values name table)
  (remove-duplicates (column-by-name name table)))

(define (table2-column-values name table)
  (remove-duplicates (table2-column-by-name name table)))

(define (table2-column-by-name name table)
  (let ((idx (index-of name (table-attrs table))))
    (if (not idx) 
        (error "column not found")
        (column-get idx (table-tuples table)))))

(define (column-by-name name table)
  (let ((idx (index-of name (table->names table))))
    (if (not idx) 
        (error "column not found")
        (column-get idx (table->rows table)))))

(define (count-val val ls)
  (count (lambda (x) (equal? val x)) ls))

(define (drop-column col table)
  (let ([idx (add1 (index-of col (table->names table)))]
        [rows (table->rows table)])
    `((names ,(append (take (table->names table) (sub1 idx)) (drop (table->names table) idx)))
      (rows ,(for/list ([r rows])
              (append (take r (sub1 idx)) (drop r idx)))))))

;; filter rows by a value in a column
(define (filter-rows-by-val val column table)
  (define (has-val-in-row? row)
    (symbol=? val (list-ref row (index-of column (table->names table)))))
  `((names ,(table->names table))
    (rows ,(filter has-val-in-row? (table->rows table)))))

;; low-level utilities
(define (index-of x l)
  (for/or ([y l] [i (in-naturals)] #:when (equal? x y)) i))

(define (column-get position ls)
  (map (lambda (xls) (list-ref xls position)) ls))

(define (substitute ls at-index with-val)
  (if (empty? ls) 
      ls
      (cons (if (zero? at-index) with-val (first ls))
            (substitute (rest ls) (sub1 at-index) with-val))))

(define (list->values ls)
  (apply values `(,@ls)))

(provide (all-defined-out))
