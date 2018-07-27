#lang racket

;;; File:
;;;   lists.rkt
;;; Summary:
;;;   A variety of procedure associated with lists.
;;; Author:
;;;   Samuel A. Rebelsky

(provide
  (contract-out
   [all (-> procedure? list? boolean?)]
   [index-of (-> any/c list? integer?)]
   [reduce (-> (-> any/c any/c any) list? any/c)]
   [tally (-> procedure? list? integer?)]
   [tally-all (-> list? list?)]
   [sort-by-count-decreasing (-> list? list?)]
   [sort-by-count-increasing (-> list? list?)]
   ))

;;; Procedure:
;;;   all
;;; Parameters:
;;;   pred?, a unary predicate
;;;   lst, a list
;;; Purpose:
;;;   Determine if pred? holds for all the values in lst.
;;; Produces:
;;;   ok?, a Boolean
;;; Preconditions:
;;;   [Standard]
;;; Postconditions:
;;;   If there is an i such that (pred? (list-ref lst i))
;;;     fails to hold, then ok? is false.
;;;   Otherwise, ok? is true.
(define all
  (lambda (pred? lst)
    (or (null? lst)
        (and (pred? (car lst))
             (all pred? (cdr lst))))))

;;; Procedure:
;;;   index-of
;;; Parameters:
;;;   val, a Scheme val
;;;   lst, a list
;;; Purpose:
;;;   Find the index of val in lst
;;; Produces:
;;;   index, an integer
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   if index is -1, val does not appear in lst.
;;;   Otherwise,
;;;     (list-ref lst index) is val
;;;     For no i smaller than index is (list-ref lst i) val.
(define index-of
  (lambda (val lst)
    (let kernel ([pos 0]
                 [remaining lst])
      (cond
        [(null? remaining)
         -1]
        [(equal? val (car remaining))
         pos]
        [else
         (kernel (+ pos 1) (cdr remaining))]))))

;;; Procedure:
;;;   tally
;;; Parameters:
;;;   pred?, a unary predicate
;;;   lst, a list of values
;;; Purpose:
;;;   Count how many values in the list match the predicate
;;; Produces:
;;;   count, a non-negative integer
;;; Preconditions:
;;;   pred? can be applied to every element of lst.
;;; Postconditions:
;;;   There are exactly `count` elements in `lst` for which `pred?` holds.
(define tally
  (lambda (pred? lst)
    (let kernel ([count 0]
                 [remaining lst])
      (cond
        [(null? remaining)
         count]
        [(pred? (car remaining))
         (kernel (+ 1 count)
                 (cdr remaining))]
        [else
         (kernel count
                 (cdr remaining))]))))

;;; Procedure:
;;;   tally-all
;;; Parameters:
;;;   lst, a list of values
;;; Purpose:
;;;   Tallies all of the values in lst
;;; Produces:
;;;   tallies, a list of (key count) lists.
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   * If val appears k times in lst, then (val k) appears in tallies
;;;   * If (val k) appears in tallies, then val appears k times in lst
;;;   * Each value in lst is the car of exactly one list in tallies.
(define tally-all
  (let ([update-tallies!
          (lambda (tallies val)
	    (if (hash-has-key? tallies val)
	        (hash-update! tallies val add1)
                (hash-set! tallies val 1)))])
    (lambda (lst)
      (let ([tallies (make-hash)])
        (let kernel ([remaining lst])
	  (cond
	    [(null? remaining)
	     (map (lambda (pair)
                    (list (car pair) (cdr pair)))
                  (hash->list tallies))]
	    [else
	     (update-tallies! tallies (car remaining))
	     (kernel (cdr remaining))]))))))

;;; Procedure:
;;;   sort-by-count-decreasing
;;;   sort-by-count-increasing
;;; Parameters:
;;;   tallies, a list of '(val num) lists
;;; Purpose:
;;;   Sort tallies (from hightest to lowest or lowest to highest)
;;; Produces:
;;;   sorted, a list of '(val num) lists
;;; Preconditions:
;;;   * All elements of tallies are two-element lists, the second of which
;;;     is a number
;;; Postconditions:
;;;   * sorted is a permutation of tallies
;;; Postconditions (sorty-by-count-decreasing):
;;;   * For all reasonable i
;;;     (cadr (list-ref sorted i)) >= (cadr (list-ref sorted (+ i 1)))
;;; Postconditions (sorty-by-count-increasing):
;;;   * For all reasonable i
;;;     (cadr (list-ref sorted i)) <= (cadr (list-ref sorted (+ i 1)))
(define sort-by-count-decreasing
  (lambda (tallies)
    (sort tallies (lambda (v1 v2) (>= (cadr v1) (cadr v2))))))

(define sort-by-count-increasing
  (lambda (tallies)
    (sort tallies (lambda (v1 v2) (<= (cadr v1) (cadr v2))))))


;;; Procedure:
;;;   reduce
;;; Parameters:
;;;   op, a binary procedure
;;;   lst, a list of values of the form (val1 val2 ... valn)
;;; Purpose:
;;;   Creates a new value by repeatedly applying op to neighboring
;;;   pairs of values in lst.
;;; Produces:
;;;   result, a value
;;; Preconditions:
;;;   op must be applicable to pairs of elements of lst.
;;;   op must return the same types that it takes as input
;;; Postconditions:
;;;   In infix notation, result is val1 op val2 op val3 ... op valn
;;;   the order of the evalution of the operations is undefined
(define (reduce op lst)
  (cond
    [(not (pair? lst))
     (error "reduce: Requires a non-empty list, given " lst)]
    [else
     (let ([rev (reverse lst)])
       (foldl op (car rev) (cdr rev)))]))
