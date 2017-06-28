#lang racket
(require "table.rkt")
(require "centroids.rkt")

(define (table2-attrs-as-number-classes-as-symbol table)
  (table2-map (lambda (x) (exact->inexact (string->number x))) string->symbol table))

(define (table->binary-table table)
  (table-row-class-map (lambda (x) (exact->inexact (string->number x))) identity table))

(define (csv->blob f)
  (table->blob (table->table2 (table->binary-table (csv->table f)))))

;; (define (reduce table)
;;   (make-table (table-attrs table)
;;               (table-classes table)
;;               (for/list ([i (map add1 (k-random-list 1 100 (table2-length table)))])
;;                 (table2-nth-tuple i table))))

(define (csv->test-table f)
  (table->table2 (table->binary-table (csv->table f))))

(define (results dir)
  (for*/fold ([ls empty])
             ([i (in-range 1 11)]
              [j (in-range 1 11)]
              #:unless (= i j))
    (let* ([csv-train (string-append "bases/" dir "/train" (number->string i) ".csv")]
           [csv-test (string-append  "bases/" dir "/test" (number->string j) ".csv")]
           [test-table (table->table2 (table->binary-table (csv->table csv-test)))])
      (append ls (test-classifier-last-class-thin (csv->blob csv-train) test-table)))))

(define (correct? r)
  (equal? (last (car r)) (second r)))

(define (incorrect? r)
  (not (correct? r)))

(define (yes? r)
  (equal? (last (car r)) (second r)))

(define (not? r)
  (not (yes? r)))

;; (define (results-per-fold dir)
;;   (for/list ([i (in-range 1 11)])
;;     (let* ([csv-train (string-append "bases/" dir "/train" (number->string i) ".csv")]
;;            [csv-test (string-append  "bases/" dir "/test" (number->string i) ".csv")]
;;            [test-table (table->table2 (table->binary-table (csv->table csv-test)))])
;;       (test-classifier-last-class-thin (csv->blob csv-train) test-table))))

(define (results-k-fold dir [tester test-classifier-last-class-thin])
  (for/fold ([ls empty]) ([i (in-range 1 11)])
    (let* ([csv-train (string-append "bases/" dir "/train" (number->string i) ".csv")]
           [csv-test (string-append  "bases/" dir "/test" (number->string i) ".csv")]
           [test-table (table->table2 (table->binary-table (csv->table csv-test)))])
      (append ls (tester (csv->blob csv-train) test-table)))))

(define (filter-results-by-class c results)
  (let ([is-it-of-class-c? (λ (r) (symbol=? c (last (car r))))])
    (filter is-it-of-class-c? results)))

(define (confusion dir [tester test-classifier-last-class-thin])
  (let ([classes (table->table2 
                     (table->binary-table 
                      (csv->table (string-append  "bases/" dir "/test1.csv"))))]
        [results (results-k-fold dir tester)])
    (let ([yes-results (filter-results-by-class 'yes results)]
          [not-results (filter-results-by-class 'not results)])
      (list (list (for/sum ([r (in-list yes-results)]) (if (yes? r) 1 0))
                  (for/sum ([r (in-list yes-results)]) (if (not? r) 1 0)))
            (list (for/sum ([r (in-list not-results)]) (if (not? r) 1 0))
                  (for/sum ([r (in-list not-results)]) (if (yes? r) 1 0)))))))

(define (matrix-yes cm)
  (first cm))

(define (matrix-not cm)
  (second cm))

(define (accuracy cm)
  ;; Takes a confusion matrix and produces its accuracy
  (let ([pos (apply + (matrix-yes cm))] 
        [neg (apply + (matrix-not cm))]
        [tp (first (matrix-yes cm))]
        [tn (second (matrix-not cm))])
    (exact->inexact (/ (+ tp tn) (+ pos neg)))))

(define (results-short dir)
  ;; What's so horribly wrong in this function?  I think it's
  ;; super-tuned.
  (list 
   (for/sum ([r (in-list (results dir))])
     (if (symbol=? (last (car r)) (second r)) 1 0)) 'correct
   (for/sum ([r (in-list (results dir))])
     (if (symbol=? (last (car r)) (second r)) 0 1)) 'incorrect))

(define (results-incorrectly-classified dir)
  (for/list ([r (in-list (results dir))] 
             #:when (not (symbol=? (last (car r)) (second r))))
    (list (car r) (second r))))

(define (all-last-class)
  (time (for/list ([name (in-list (all-bases))])
          (let ([cm (confusion (symbol->string name))])
            (list name 'matrix cm
                  'accuracy (accuracy cm))))))

(define (all-sum)
  (time (for/list ([name (in-list (all-bases))])
          (let ([cm (confusion (symbol->string name) test-classifier-sum)])
            (list name 'matrix cm
                  'accuracy (accuracy cm))))))

(define (all-bases)
  '(iris-setosa-or-not 
    iris-versicolor-or-not 
    iris-virginica-or-not 
    forestfires 
    fertility 
    appendicitis
    glass
    blood
    breast-cancer
    magic04
    parkinson
    spam
    ))
