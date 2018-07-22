#lang racket

;;; File:
;;;   web-utils.rkt
;;; Summary:
;;;   A variety of useful procedures for processing Web pages in Racket.
;;; Author:
;;;   Samuel A. Rebelsky

(require html-parsing)
(require net/url)
(require sxml)
(require "text.rkt")
(require "lists.rkt")

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
;;;   fetch-page-pure
;;; Parameters:
;;;   url, a string that represents a URL
;;; Purpose:
;;;   Fetch the page at the given URL.
;;; Produces:
;;;   page, an xexp expression
(define (fetch-page-pure url)
  (if (regexp-match #rx"^http" url)
      (xexp/top-element (fetch-page-top url))
      `(html
         (head (title "Invalid URL"))
         (body
          (h1 "Invalid URL")
          (p "Could not process the URL '" ,url "'")))))

(define (fetch-page-top url)
  (html->xexp (get-pure-port (string->url url))))

;;; Procedure:
;;;   fetch-page
;;; Parameters:
;;;   url, a string representing a URL
;;; Purpose:
;;;   Fetch the page at the given URL, rewriting internal URLs so that
;;;   it appears correctly.
;;; Produces:
;;;   page, an xexp expression
;;; Problems:
;;;   Not particularly robust.
(define (fetch-page url)
  (update-urls (fetch-page-pure url)
               url))

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

;;; Procedure:
;;;   page-transform-elements
;;; Parameters:
;;;   page, a page in the standard xexp format
;;;   tag, a symbol
;;;   transform, a procedure from elements to elements
;;; Purpose:
;;;   Transform all of the elements that start with the given tag
;;; Produces:
;;;   transformed, a page in the standard xexp format
(define (page-transform-elements page tag transform)
  ((sxml:modify (list (string-append "//"
                                     (symbol->string tag))
                      ; sxml:modify expects a three parameter proc;
                      ; we expect a one-parameter proc.
                      (lambda (element context root)
                        (transform element))))
   page))

;;; Procedure:
;;;   page-delete-elements
;;; Parameters:
;;;   page, a page in the standard xexp format
;;;   tag, a symbol
;;; Purpose:
;;;   Delete all of the elements with the given tag
;;; Produces:
;;;   newpage, a page in the standard xexp format
(define (page-delete-elements page tag)
  ((sxml:modify (list (string-append "//"
                                     (symbol->string tag))
                      'delete))
   page))

;;; Procedure:
;;;   page-replace-text
;;; Parameters:
;;;   page, a page in the standard xexp format
;;;   pattern, a string or a regular expression
;;;   replacement, a string (for now)
;;; Purpose:
;;;   Replace all instances of pattern in the text by replacement
;;; Produces:
;;;   newpage, a page in the standard xexp format
;;; Problem:
;;;   Does not yet handle the situation in which replacement is a
;;;   replacement element.
(define (page-replace-text page pattern replacement)
  (if (pair? replacement)
      (page-replace-text page pattern
                         (lambda (element) replacement))
      (let* ([rxpattern (if (regexp? pattern)
                            pattern
                            (regexp pattern))]
             [proc (cond
                     [(string? replacement)
                      (lambda (element context root)
                        (regexp-replace* rxpattern element replacement))]
                     [(symbol? replacement)
                      (let ([str (symbol->string replacement)])
                        (lambda (element context root)
                          (regexp-replace* rxpattern element str)))]
                     [(procedure? replacement)
                      (lambda (element context root)
                        (let kernel ([pieces (regexp-pieces rxpattern element)]
                                     [results null])
                          (cond
                            [(null? pieces)
                             (if (all string? results)
                                 (reduce string-append (reverse results))
                                 (cons 'span (reverse results)))]
                            [(regexp-match rxpattern (car pieces))
                             (kernel (cdr pieces)
                                     (cons (replacement (car pieces)) results))]
                            [else
                             (kernel (cdr pieces)
                                     (cons (car pieces) results))])))]
                     [else
                      (error "Invalid replacement" replacement)])])
        ((sxml:modify (list "//text()" proc))
         page))))

;;; Procedure:
;;;   page-add-to-end
;;; Parameters:
;;;   page, a page
;;;   element, an xexp
;;; Purpose:
;;;   Add element to the end of the body of page
(define (page-add-to-end page element)
  ((sxml:modify (list "//body"
                      'insert-into
                      element))
   page))

;;; Procedure:
;;;   update-attribute
;;; Parameters:
;;;   xexp, an xexp
;;;   parameter, a symbol
;;;   replacement, a procedure or string
;;; Purpose:
;;;   Update the given parameter of the xexp, either by applying
;;;   replacement or by using replacement.
;;; Produces:
;;;   updated, an xexp
(define (update-attribute xexp parameter replacement)
  (let ([new-attributes
         (let kernel ([attributes (cdr (extract-attributes xexp))])
           (cond
             [(null? attributes)
              (if (string? replacement)
                  (list (list parameter replacement))
                  null)]
             [(eq? parameter (caar attributes))
              (let ([value (if (null? (cdar attributes))
                               ""
                               (cadar attributes))])
                (cons (list parameter (if (procedure? replacement)
                                          (replacement value)
                                          replacement))
                      (cdr attributes)))]
             [else
              (cons (car attributes)
                    (kernel (cdr attributes)))]))])
    (cons (car xexp)
          (cons (cons '@ new-attributes)
                (if (has-attributes? xexp)
                    (cddr xexp)
                    (cdr xexp))))))

;;; Procedure:
;;;   update-urls
;;; Parameters:
;;;   xexp, an xexp expression
;;;   current-url, a string or URL
;;; Purpose:
;;;   Transform all the relative URLs to be absolute.
;;; Produces:
;;;   updated, an xexp expression
(define (update-urls xexp current-url)
  (let* ([url (if (string? current-url)
                  (string->url current-url)
                  current-url)]
         [fix (lambda (relative)
                (url->string (combine-url/relative url relative)))]
         [fixer (lambda (attribute)
                  (lambda (element context root)
                    (update-attribute element attribute fix)))])
    ((sxml:modify (list "//a" (fixer 'href))
                  (list "//img" (fixer 'src))
                  (list "//link" (fixer 'href))
                  (list "//script" (fixer 'src)))
     xexp)))

; +-------------------------+----------------------------------------
; | Miscellaneous utilities |
; +-------------------------+

;;; Procedure:
;;;   has-attributes?
;;; Parameters:
;;;   xexp, an xexp expression
;;; Purpose:
;;;   Determine if xexp appears to have attributes
;;; Produces:
;;;   appears-to-have-attributes?, a Boolean
(define (has-attributes? xexp)
  (and (pair? xexp)
       (not (null? (cdr xexp)))
       (attributes? (cadr xexp))))

;;; Procedure:
;;;   attributes?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determines if val appears to represent a set of attributes.
;;; Produces:
;;;   appears-to-be-attributes?, a Boolean
(define (attributes? val)
  (and (pair? val)
       (eq? '@ (car val))))

;;; Procedure:
;;;   extract-attributes
;;; Parameters:
;;;   xexp, an xexp expression
;;; Purpose:
;;;   Extract the attributes from the given xexp.
;;; Produces:
;;;   attributes, a list
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   * If (has-attributes? xexp), attributes is the attributes of xexp.
;;;   * Otherwise, attributes is the empty list of attributes
(define (extract-attributes xexp)
  (if (has-attributes? xexp)
      (cadr xexp)
      '(@)))
