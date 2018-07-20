#lang racket

;;; File:
;;;   text.rkt
;;; Summary:
;;;   A variety of procedures for working with text.
;;; Author:
;;;   Samuel A. Rebelsky

(provide (all-defined-out))

;;; Macro:
;;;   one-of
;;; Parameters:
;;;   exp1, an expression
;;;   exp2, an expression
;;;   ...
;;;   expn, an expression
;;; Purpose:
;;;   Select and evaluate one of the expressions.
(define-syntax one-of
  (lambda (stx)
    (let ([datum (syntax->datum stx)])
      (cond
        [(symbol? datum)
         (datum->syntax stx '(quote <macro:one-of>))]
        [(null? (cdr datum))
         (error "one-of: requires at least one parameter")]
        [else
         (let ([len (length (cdr datum))]
               [thunks (map (lambda (exp) `(lambda () ,exp))
                                          (cdr datum))])
           (let ([code `(let ([vec ,(cons 'vector thunks)])
                          ((vector-ref vec (random ,len))))])
             ; (write code) (newline)
             (datum->syntax stx code)))]))))

;;; Procedure:
;;;   add-indefinite-article
;;; Parameters:
;;;   str, a string
;;; Purpose:
;;;   Add the appropriate article to the front of the string.
;;; Produces:
;;;   newstr, a string
(define (add-indefinite-article str)
  (let ([first (substring str 0 1)])
    (cond
      [(string-contains? "aeiou" first)
       (string-append "an " str)]
      [(string-contains? "AEIOU" first)
       (string-append "An " (string-downcase first) (substring str 1))]
      [(char-upper-case? (string-ref str 0))
       (string-append "A " (string-downcase first) (substring str 1))]
      [else
       (string-append "a " str)])))

;;; Procedure:
;;;   capitalize
;;; Parameters:
;;;   str, a string
;;; Purpose:
;;;   capitalize the first letter of str.
;;; Produce:
;;;   capitalized, a string
(define (capitalize str)
  (if (starts-with-capital? str)
      str
      (string-append (string-upcase (substring str 0 1))
                     (substring str 1))))

;;; Procedure:
;;;   regexp-pieces
;;; Parameters:
;;;   re, a regexp
;;; Purpose:
;;;   Split the string at every copy of re, generating a list of strings.
;;; Produces:
;;;   pices, a list of strings
(define (regexp-pieces regexp str)
  (let ([add-substring
         (lambda (start finish lst)
           (if (< start finish)
               (cons (substring str start finish) lst)
               lst))]
        [len (string-length str)])
    (let kernel ([cur 0]
                 [positions (regexp-match-positions* regexp str)]
                 [substrings null])
      (if (null? positions)
          (reverse (add-substring cur len substrings))
          (let* ([front (caar positions)]
                 [back (cdar positions)])
            (kernel back
                    (cdr positions)
                    (add-substring front back
                                   (add-substring cur front
                                                  substrings))))))))

;;; Procedure:
;;;   starts-with-capital?
;;; Parameters:
;;;   str, a string
;;; Purpose:
;;;   Determine if str starts with a capital letter.
;;; Produces:
;;;   swc?, a Boolean
(define (starts-with-capital? str)
  (and (string? str)
       (> (string-length str) 0)
       (char-upper-case? (string-ref str 0))))

;;; Procedure:
;;;   starts-with-vowel?
;;; Parameters:
;;;   str, a string
;;; Purpose:
;;;   Determines if str starts with a vowel (at least in English)
;;; Produces:
;;;   swv?, a Boolean
(define (starts-with-vowel? str)
  (and (string? str)
       (> (string-length str) 0)
       (string-contains? "aeiouAEIOU" (substring str 0 1))))
