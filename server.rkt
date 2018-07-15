#lang racket
(provide (all-defined-out))
(require html-parsing)
(require web-server/servlet
         web-server/servlet-env)
(require web-server/http/bindings)

;;; Value:
;;;   pages
;;; Type:
;;;   Hash table
;;; Summary:
;;;   A list of all page->thing mappings.
(define pages (make-hash))

;;; Procedure:
;;;   pages->xexpr
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Makes an xexpr for all the pages we have.
;;; Produces:
;;;   xexpr, an xexpr that represents all the pages.
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
;;;   serve-procedure
;;; Parameters:
;;;   path, a string
;;;   proc, a unary procedure
;;; Purpose:
;;;   Handle path with proc
;;; Produces:
;;;   [Nothing; called for the side effect]
(define (serve-procedure path proc)
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

(define (serve-file path file)
  (hash-set! pages path (list 'file file)))

;;; Procedure:
;;;   xexp->xexpr
;;; Parameters:
;;;   xexp, an xexp
;;; Purpose:
;;;   Convert xexp to an xexpr
;;; Produces:
;;;   xexpr, a corresponding xexpr
(define (xexp->xexpr xexp)
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
     (xexp->xexpr (last xexp))]
    ; Drop the @ for parameters
    [(eq? (car xexp) '@)
     (cdr xexp)]
    [else
     (cons (car xexp)
           (map xexp->xexpr (cdr xexp)))]))

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
    (for-each (lambda (val)
                (display val)
                (display " "))
              vals)
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
;;;   serve
;;; Parameters:
;;;   req, an HTTP request (see https://docs.racket-lang.org/web-server/http.html)
;;; Purpose:
;;;   serve one page
;;; Produces:
;;;   resp, an HTTP response (see https://docs.racket-lang.org/web-server/http.html)
(define (serve request)
  (let ([uri (url->string (request-uri request))]
        [bindings (request-bindings/raw request)])
    (newline)
    ; (display "URI: ") (write uri) (newline)
    ; (display "BINDINGS: ") (write bindings) (newline)
    (let ([handler (hash-ref pages (substring uri 1) 'default)])
      (cond
        [(and (pair? handler) (eq? (car handler) 'proc))
         (let ([proc (cadr handler)])
           (display-line "Handling" uri "with procedure" proc)
           (response/xexpr
            (xexp->xexpr (proc bindings))))]
        [(and (pair? handler) (eq? (car handler) 'string))
         (let ([contents (cadr handler)]
               [type (caddr handler)])
           (display-line "Handling" uri "with a string")
           (response 200 #"OK"
                     (current-seconds)
                     (string->bytes/utf-8 (string-append "text/" type))
                     empty
                     (lambda (port) (display contents port))))]
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
           (display-line "Handling" uri "with file" filename)
           (response 200 #"OK"
                     (current-seconds)
                     (string->bytes/utf-8 (string-append "text/" type))
                     empty
                     (lambda (port) (write-bytes bytes port))))]
        [(equal? uri "/")
         (display-line "Serving the default home page")
         (response/xexpr
          `(html (head (title "L&C Web Server"))
                 (body
                  (h1 "L&C Web Server - List of Available Pages")
                  ,(pages->xexpr))))]
        [else
         (display-line "Could not find handler for" uri)
         (response/xexpr
          `(html (head (title "Page not found"))
                 (body
                  (p "Could not find the page "
                     ,uri))))]))))

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

;;; Procedure:
;;;   experiment
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Conduct a simple experiment
;;; Produces:
;;;   [Nothing, I think]
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
