#lang racket

;;; File:
;;;   server.rkt
;;; Summary:
;;;   A really simple Web server for the code camp.
;;; Author:
;;;   Samuel A. Rebelsky

(provide (all-defined-out))
(require "web-utils.rkt")
(require "lists.rkt")
(require html-parsing)
(require html-writing)
(require web-server/servlet
         web-server/servlet-env)
(require web-server/http/bindings)

; +---------+--------------------------------------------------------
; | Globals |
; +---------+

;;; Value:
;;;   pages
;;; Type:
;;;   Hash table
;;; Summary:
;;;   A list of all page->thing mappings.
(define pages (make-hash))

; +---------------------------+--------------------------------------
; | Primary client procedures |
; +---------------------------+

;;; Procedure:
;;;   get
;;; Parameters:
;;;   key, a string
;;;   stuff, bindings
;;;   default, a string
;;; Purpose:
;;;   Get the value associated with key in stuff.
;;;   (Read a form input.)
;;; Produces:
;;;   result, a string
;;; Postconditions:
;;;   If the user provided something for the key, gives you that thing.
;;;   Otherwise, gives you default.
(define (get key stuff default)
  (let ([sym (if (string? key)
                 (string->symbol key)
                 key)])
    (if (exists-binding? sym stuff)
        (extract-binding/single sym stuff)
        default)))

;;; Procedure:
;;;   serve-procedure
;;; Parameters:
;;;   path, a string
;;;   proc, a unary procedure
;;; Purpose:
;;;   Handle path with proc
;;; Produces:
;;;   [Nothing; called for the side effect]
(define (serve-procedure path proc)
  (hash-set! pages path (list 'procedure proc)))
(define (serve-proc path proc)
  (hash-set! pages path (list 'proc proc)))

;;; Procedure:
;;;   serve-string
;;; Parameters:
;;;   path, a string
;;;   str, a string
;;;   type, a string
;;; Purpose:
;;;   Indicate that we should handle path by returning str.
;;;   type shoudl be "html", "css", or "plain"
;;; Produces:
;;;   [Nothing; called for the side effect]
(define (serve-string path str type)
  (hash-set! pages path (list 'string str type)))

;;; Procedure:
;;;   serve-file
;;; Parameters:
;;;   path, a string
;;;   filepath, a string
;;; Purpose:
;;;   Indicate that we should handle path by returning the contents
;;;   of the given file.
;;; Produces:
;;;   [Nothing; called for the side effect]
(define (serve-file path file)
  (hash-set! pages path (list 'file file)))

;;; Procedure:
;;;   start-server
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Start the server and open a Web page
(define (start-server)
  (serve/servlet serve
                 #:listen-ip #f
                 #:servlet-regexp #rx""
                 #:servlet-path "/"))

; +-----------+------------------------------------------------------
; | Utilities |
; +-----------+

;;; Procedure:
;;;   pages->xexpr
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Makes an xexpr for all the pages we have.
;;; Produces:
;;;   xexpr, an xexpr that represents a list of all the pages.
(define (pages->xexpr)
  (let kernel ([remaining (sort (hash-keys pages) string-ci>=?)]
               [xexpr null])
    (if (null? remaining)
        (cons 'ul xexpr)
        (let ([path (car remaining)])
          (kernel (cdr remaining)
                  (cons `(li (a ((href ,path))
                                ,path))
                        xexpr))))))

;;; Procedure:
;;;   display-line
;;; Parameters:
;;;   val1, a Scheme value
;;;   val2, a Scheme value
;;;   ...
;;;   valn, a Scheme value
;;; Purpose:
;;;   display all the values in line
;;; Produces:
;;;   [Nothing; called for the side effect.]
(define display-line
  (lambda vals
    (for-each display vals)
    (newline)))

;;; Procedure:
;;;   read-file
;;; Parameters:
;;;   fname, a string
;;; Purpose:
;;;   Read the contents of the given file.
;;; Produces:
;;;   Contents, a byte string
(define (read-file fname)
  (let* ([port (open-input-file (string-append (getenv "HOME")
                                               "/"
                                               fname))]
         [result (port->bytes port)])
    (close-input-port port)
    result))

;;; Procedure:
;;;   request-path
;;; Parameters
;;;   req, an HTTP request
;;; Purpose:
;;;   Find the path from a request
;;; Produces:
;;;   path, a string
(define (request-path req)
  (let ([components (map path/param-path (url-path (request-uri req)))])
    (reduce (lambda (a b) (string-append a "/" b)) components)))

;;; Procedure:
;;;   serve
;;; Parameters:
;;;   req, an HTTP request (see https://docs.racket-lang.org/web-server/http.html)
;;; Purpose:
;;;   serve one page
;;; Produces:
;;;   resp, an HTTP response (see https://docs.racket-lang.org/web-server/http.html)
(define (serve request)
  (let* ([uri (request-uri request)]
         [path (request-path request)]
         [bindings (request-bindings request)])
    (newline)
    ; (display "URI: ") (write uri) (newline)
    ; (display "URL-PATH: ") (write (url-path uri)) (newline)
    ; (display "PATH: ") (write path) (newline)
    ; (display "BINDINGS: ") (write bindings) (newline)
    (let ([handler (hash-ref pages path 'default)])
      (cond
        ; Normal procedure handler
        [(and (pair? handler) (eq? (car handler) 'procedure))
         (let ([proc (cadr handler)])
           (display-line "Handling '" path "' with procedure" proc)
           (response 200 #"OK"
                     (current-seconds)
                     TEXT/HTML-MIME-TYPE
                     empty
                     (lambda (port) (write-html (xexp/cleanup (proc bindings)) port))))]
        ; Alternate procedure handler
        [(and (pair? handler) (eq? (car handler) 'proc))
         (let ([proc (cadr handler)])
           (display-line "Handling '" path "' with alternate procedure" proc)
           (response/xexpr
            (xexp->xexpr (xexp/cleanup (proc bindings)))))]
        ; String handler
        [(and (pair? handler) (eq? (car handler) 'string))
         (let ([contents (cadr handler)]
               [type (caddr handler)])
           (display-line "Handling '" path "' with a string")
           (response 200 #"OK"
                     (current-seconds)
                     (string->bytes/utf-8 (string-append "text/" type))
                     empty
                     (lambda (port) (display contents port))))]
        ; File handler
        [(and (pair? handler) (eq? (car handler) 'file))
         (let* ([filename (cadr handler)]
                [bytes (read-file filename)]
                [type (cond
                        [(regexp-match #rx"\\.html$" filename)
                         "html"]
                        [(regexp-match #rx"\\.css$" filename)
                         "css"]
                        [else
                         "plain"])])
           (display-line "Handling" path "with file" filename)
           (response 200 #"OK"
                     (current-seconds)
                     (string->bytes/utf-8 (string-append "text/" type))
                     empty
                     (lambda (port) (write-bytes bytes port))))]
        ; Default home page
        [(equal? path "")
         (display-line "Serving the default home page")
         (response/xexpr
          `(html (head (title "L&C Web Server"))
                 (body
                  (h1 "L&C Web Server - List of Available Pages")
                  ,(pages->xexpr))))]
        ; Missing pages
        [else
         (display-line "Could not find handler for \"" path "\"")
         (response/xexpr
          `(html (head (title "Page not found"))
                 (body
                  (p "Could not find the page '" ,path "'"))))]))))

; +-----------------------+------------------------------------------
; | Tests and experiments |
; +-----------------------+

;;; Procedure:
;;;   experiment
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Conduct a simple experiment
;;; Produces:
;;;   [Nothing, I think]
;;; Problems:
;;;   Arguably, this should be in a separate file.  But I'm keeping it here during development.
(define (experiment)
  (let ([first-page (lambda (bindings)
                      (html->xexp
                       "<html><head><title>First</title><body><p class='one'>First page</body></html>"))]
        [second-page (lambda (bindings)
                       `(*TOP*
                         (html
                          (head (title "An xexp")
                                (link (@ (rel "stylesheet")
                                         (href "styles.css"))))
                          (body (@ (class "Sam"))
                                (p "Generated with an xexp")
                                (p 17)
                                (p hello)
                                (p "Test")))))])
    
    (serve-procedure "first" first-page)
    (serve-procedure "second" second-page)
    
    (serve-string "html"
                  "<html><head><title>HTML</title><body><p class='one'>This is HTML</body></html>"
                  "html")
    (serve-string "text"
                  "<html><head><title>HTML</title><body><p class='one'>This is HTML</body></html>"
                  "plain")
    (serve-string "styles.css"
                  "body { background-color: red; }"
                  "css")
    (serve-file "file/one" "Desktop/LanguageAndCode/file.txt")
    (serve-file "file/two" "Desktop/LanguageAndCode/file.html")
    (serve-file "file/three" "Desktop/LanguageAndCode/html.txt")
    (start-server)))
