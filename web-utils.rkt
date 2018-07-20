#lang racket

;;; File:
;;;   web-utils.rkt
;;; Summary:
;;;   A variety of useful procedures for processing Web pages in Racket.
;;; Author:
;;;   Samuel A. Rebelsky

(require html-parsing)
(require net/url)
(require sxml/sxpath)

(provide (all-defined-out))

; +-------------------+----------------------------------------------
; | xexp manipulation |
; +-------------------+

;;; Procedure:
;;;   xexp/attributes-fix
;;; Parameters:
;;;   attributes, a portion of an xexp expression representing
;;;   attributes
;;; Purpose:
;;;   Deal with a small problem in how html-parsing handles empty attributes
;;; Produces:
;;;   fixed-attributes, an equivalent set of attributes
;;; Preconditions:
;;;   attributes is of the form `((name value) ... (name) ....)
;;; Postconditions:
;;;   Any element of the form `(name) is replaced by `(name "")
(define (xexp/attributes-fix attributes)
  (cond
    [(null? attributes)
     null]
    [(and (pair? (car attributes))
          (null? (cdar attributes)))
     (cons (list (caar attributes) "")
           (xexp/attributes-fix (cdr attributes)))]
    [else
     (cons (car attributes)
           (xexp/attributes-fix (cdr attributes)))]))

;;; Procedure:
;;;   xexp-cleanup
;;; Parameters:
;;;   xexp, an xexp expression
;;; Purpose:
;;;   Deal with a few potential issues with xexps, either those generated
;;;   by html->xexp or those written by students.
;;; Produces:
;;;   clean, an xexp expression
(define (xexp/cleanup xexp)
  (cond
    ; Two little hacks to help my students
    [(number? xexp)
     (number->string xexp)]
    [(symbol? xexp)
     (symbol->string xexp)]
    ; Non-pairs should stay as is
    [(not (pair? xexp))
     xexp]
    ; Attributes
    [(eq? (car xexp) '@)
     (cons '@ (xexp/attributes-fix (cdr xexp)))]
    ; Everything else
    [else
     (cons (car xexp)
           (map xexp/cleanup (cdr xexp)))]))

;;; Procedure:
;;;   xexp/top-element
;;; Parameters:
;;;   top, a top-level xexp.
;;; Purpose:
;;;   Extract the element from a top-level xexp.
;;; Produces:
;;;   element, the primary element
;;; Preconditions:
;;;   top has the form '(*TOP* ... element ...), where the values other
;;;   than the element must either be (a) strings, (b) processing instructions
;;;   of the form '(*PI* ...), (c) comments of the form '(*COMMENT* ...),
;;;   (d) annotations of the form '(@ ...).
;;; Postconditions
;;;   The first element in top what is not of the given form.
;;; Ponderings:
;;;   The official syntax of SXML/xexp is at
;;;     http://okmij.org/ftp/Scheme/SXML.html#Grammar.
;;;   That syntax has the nice property that the last item in the list
;;;   is the element.  However, html-parsing seems to allow things
;;;   after the element.
;;;     > (html->xexp "<input type='text'/>\n\n\n")
;;;     '(*TOP* (input (@ (type "text"))) "\n" "\n" "\n")
(define (xexp/top-element top)
  (if (or (not (list? top))
          (null? top)
          (not (symbol? (car top)))
          (not (string-ci=? "*TOP*" (symbol->string (car top)))))
      (error "xexp/top-element: expected a top-level element, received" top)
      (let kernel ([rest (cdr top)])
        (cond
          [(null? rest)
           (error "xexp/top-element: Could not find an element")]
          [(not (pair? (car rest)))
           (kernel (cdr rest))]
          [(not (symbol? (caar rest)))
           (kernel (cdr rest))]
          [(string-contains? "*@"
                             (substring (symbol->string (caar rest)) 0 1))
           (kernel (cdr rest))]
          [else
           (car rest)]))))

;;; Procedure:
;;;   xexp->xexpr
;;; Parameters:
;;;   xexp, an xexp
;;; Purpose:
;;;   Convert xexp to an xexpr
;;; Produces:
;;;   xexpr, a corresponding xexpr
(define (xexp->xexpr xexp)
  (let kernel ([xexp (xexp/cleanup xexp)])
    (cond
      ; Two little hacks to help my students
      [(number? xexp)
       (number->string xexp)]
      [(symbol? xexp)
       (symbol->string xexp)]
      ; Non-pairs should stay as is
      [(not (pair? xexp))
       xexp]
      ; If we start with *TOP*, the stuff we care about is at the end.
      [(eq? (car xexp) '*TOP*)
       (kernel (xexp/top-element xexp))]
      ; Drop the @ for parameters
      [(eq? (car xexp) '@)
       (cdr xexp)]
      [else
       (cons (car xexp)
             (map kernel (cdr xexp)))])))

; +-------------+-------------------------------------------------
; | Fetch pages |
; +-------------+

;;; Procedure:
;;;   fetch-page
;;; Parameters:
;;;   url, a string representing a URL
;;; Purpose:
;;;   Fetch the page at the given URL.
;;; Produces:
;;;   page, an xexp
;;; Problems:
;;;   Not particularly robust.
(define (fetch-page url)
  (xexp/top-element (fetch-page-top url)))

(define (fetch-page-top url)
  (html->xexp (get-pure-port (string->url url))))

;;; Procedure:
;;;   fetch-page-as-HTML
;;; Parameters:
;;;   url, a string representing a URL
;;; Purpose:
;;;   Fetch the page at the given URL.
;;; Produces:
;;;   page, a string
;;; Problems:
;;;   Not particularly robust.
(define (fetch-page-as-html url)
  (let ([port (get-pure-port (string->url url))])
    (let kernel ()
      (let ([line (read-line port)])
        (cond
          [(eof-object? line)
           (close-input-port port)
           null]
          [else
           (cons line (kernel))])))))

; +--------------------------------+---------------------------------
; | Extract information from pages |
; +--------------------------------+

;;; Procedure:
;;;   extract-text
;;; Parameters:
;;;   xexp, an xexp
;;; Purpose:
;;;   Extract the text (string) from an xexp
;;; Produces:
;;;   text, a string
(define extract-text (sxpath "string(/)"))

;;; Procedure:
;;;   extract-by-tag
;;; Parameters:
;;;   tag, a symbol
;;;   xexp, an xexp
;;; Purpose:
;;;   Extract all of the expression in exp that start with the given
;;;   tag.
;;; Produces:
;;;   expressions, a list of xexps
(define (extract-by-tag tag xexp)
  ((sxpath (string-append "//" (symbol->string tag))) xexp))

;;; Procedure:
;;;   extract-attribute
;;; Parameters:
;;;   attribute, a symbol
;;;   xexp, an xexp
;;; Purpose:
;;;   Extract the given attribute from the expression.
(define (extract-attribute attribute xexp)
  (let ([default ""])
    (cond
      [(not (pair? xexp))
       default]
      [(null? (cdr xexp))
       default]
      [(not (pair? (cadr xexp)))
       default]
      [(not (eq? '@ (caadr xexp)))
       default]
      [else
       (let kernel ([attributes (cdadr xexp)])
         (cond 
           [(null? attributes)
            default]
           [(eq? attribute (caar attributes))
            (cadar attributes)]
           [else
            (kernel (cdr attributes))]))])))

; +-----------------+------------------------------------------------
; | Transform pages |
; +-----------------+
