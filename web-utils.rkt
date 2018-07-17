#lang racket
(require html-parsing)
(require net/url)
(require sxml/sxpath)

(provide (all-defined-out))

;;; Procedure:
;;;   fetch-page
;;; Parameters:
;;;   URL, a string representing a URL
;;; Purpose:
;;;   Fetch the page at the given URL.
;;; Produces:
;;;   page, an xexp
;;; Problems:
;;;   Not particularly robust.
(define (fetch-page url)
  (html->xexp (get-pure-port (string->url url))))

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
  (cond
    [(not (pair? xexp))
     null]
    [(null? (cdr xexp))
     null]
    [(not (pair? (cadr xexp)))
     null]
    [(not (eq? '@ (caadr xexp)))
     null]
    [else
     (let kernel ([attributes (cdadr xexp)])
       (cond 
         [(null? attributes)
	  null]
	 [(eq? attribute (caar attributes))
	  (cadar attributes)]
	 [else
	  (kernel (cdr attributes))]))]))
