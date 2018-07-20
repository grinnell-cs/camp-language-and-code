#lang racket

;;; File:
;;;   files.rkt
;;; Summary:
;;;   A variety of useful procedures for dealing with files.
;;; Author:
;;;   Samuel A. Rebelsky

(provide
 (contract-out
  [file->chars (-> string? (listof char?))]
  [file->chunks (-> string? (listof string?))]
  [file->lines (-> string? (listof string?))]
  [file->sentences (-> string? (listof string?))]
  [file->tweets (-> string? (listof (listof string?)))]
  [file->words (-> string? (listof string?))]
  [read-chunk (-> input-port? string?)]
  [read-sentence (-> input-port? string?)]
  [read-tweet (-> input-port? (listof string?))]
  [read-word (-> input-port? string?)]
  [skip-char (-> input-port? char? boolean?)]
  [read-until (-> input-port? (or/c procedure? char? string?) string?)]))

; +---------------------+--------------------------------------------
; | Exported procedures |
; +---------------------+

;;; Package:
;;;   lac-camp/files
;;; Procedure:
;;;   file->chars
;;; Parameters:
;;;   fname, a string
;;; Purpose:
;;;   Reads and returns all of the characters in the file
;;; Produces:
;;;   chars, a list of characters
;;; Preconditions:
;;;   fname names a valid file
;;; Postconditions:
;;;   chars contains all of the characters in the file.
(define file->chars
  (lambda (fname)
    (file->stuff fname read-char eof-object?)))

;;; Package:
;;;   lac-camp/files
;;; Procedure:
;;;   file->lines
;;; Parameters:
;;;   fname, a string
;;; Purpose:
;;;   Reads and returns all of the lines in the file
;;; Produces:
;;;   lines, a list of strings
;;; Preconditions:
;;;   fname names a valid file
;;; Postconditions:
;;;   lines contains all of the lines in the file, in the order they appear,
;;;   but without newlines.
(define file->lines
  (lambda (fname)
    (file->stuff fname read-line eof-object?)))

;;; Package:
;;;   lac-camp/files
;;; Procedure:
;;;   file->sentences
;;; Parameters:
;;;   fname, a string
;;; Purpose:
;;;   Reads and returns all of the sentences in the file
;;; Produces:
;;;   sentences, a list of strings
;;; Preconditions:
;;;   fname names a valid file
;;; Postconditions:
;;;   lines contains all of the sentences in the file, in the order they
;;;   appear.
(define file->sentences
  (lambda (fname)
    (file->stuff fname read-sentence (lambda (stuff) (equal? stuff "")))))

;;; Package:
;;;   lac-camp/files
;;; Procedure:
;;;   file->chunks
;;; Parameters:
;;;   fname, a string
;;; Purpose:
;;;   Reads and returns all of the "chunks" (things separated by spaces)
;;;   in the file.
;;; Produces:
;;;   words, a list of strings
;;; Preconditions:
;;;   fname names a valid file
;;; Postconditions:
;;;   words contains all of the words in the file, in the order they appear,
;;;   but without punctuation.
(define file->chunks
  (lambda (fname)
    (file->stuff fname read-chunk (lambda (stuff) (equal? stuff "")))))

;;; Package:
;;;   lac-camp/files
;;; Procedure:
;;;   file->tweets
;;; Parameters:
;;;   fname, a string
;;; Purpose:
;;;   Reads and returns all of the lines in the file
;;; Produces:
;;;   lines, a list of strings
;;; Preconditions:
;;;   fname names a valid file
;;; Postconditions:
;;;   lines contains all of the lines in the file, in the order they appear,
;;;   but without newlines.
(define file->tweets
  (lambda (fname)
    (file->stuff fname read-tweet eof-object?)))

;;; Package:
;;;   lac-camp/files
;;; Procedure:
;;;   file->words
;;; Parameters:
;;;   fname, a string
;;; Purpose:
;;;   Reads and returns all of the words in the file
;;; Produces:
;;;   words, a list of strings
;;; Preconditions:
;;;   fname names a valid file
;;; Postconditions:
;;;   words contains all of the words in the file, in the order they appear,
;;;   but without punctuation.
(define file->words
  (lambda (fname)
    (file->stuff fname read-word (lambda (stuff) (equal? stuff "")))))

;;; Package:
;;;   lac-camp/files
;;; Procedure:
;;;   read-charseq
;;; Parameters:
;;;   port, an input port
;;;   pred?, a character predicate
;;; Purpose:
;;;   Read a sequence of characters that meet pred and return
;;;   them as a string.
;;; Produces:
;;;   str, a string
;;; Preconditions:
;;;   port is open for reading.
;;; Postconditions:
;;;   * str represents the next sequence of characters for which
;;;     pred? holds.
;;;   * If there is no such sequence, str is ""
;;; Philosophy:
;;;   DRY out read-word, read-chunk, and read-sentence.
(define read-charseq
  (lambda (port pred?)
    (let kernel ([chars null])
      (let ([ch (peek-char port)])
        (cond
          [(eof-object? ch)
           (list->string (reverse chars))]
          [(pred? ch)
           (read-char port)
           (kernel (cons ch chars))]
          [(null? chars)
           (read-char port)
           (kernel null)]
          [else
           (list->string (reverse chars))])))))

;;; Package:
;;;   lac-camp/files
;;; Procedure:
;;;   read-chunk
;;; Parameters:
;;;   port, an input port
;;; Purpose:
;;;   Reads the next chunk from the port
;;; Produces:
;;;   chunk, a string
;;; Preconditions:
;;;   port is open for reading
;;; Postconditions:
;;;   word is the next word in the port.
(define read-chunk
  (lambda (port)
    (read-charseq port (lambda (ch) (not (char-whitespace? ch))))))

;;; Package:
;;;   lac-camp/files
;;; Procedure:
;;;   read-sentence
;;; Parameters:
;;;   port, an input port
;;; Purpose:
;;;   Reads the next sentence from the port
;;; Produces:
;;;   sentence , a string
;;; Preconditions:
;;;   port is open for reading
;;; Postconditions:
;;;   word is the next sentence (or reasonable variant thereof) in 
;;;   the port.
;;; Problems:
;;;   Our view of "sentence" is fairly simplistic.  It's something
;;;   that ends with a period, question mark, or exclamation point.
(define read-sentence
  (let ([punctuation (vector #\. #\? #\!)])
    (lambda (port)
      (let* ([all-but-punctuation
             (read-charseq port
                           (lambda (ch)
                             (not (vector-member ch punctuation))))]
             [punctuation (read-char port)]
             [sentence  (if (char? punctuation)
                             (string-append all-but-punctuation
                                            (string punctuation))
                             all-but-punctuation)])
        (regexp-replace #rx"^ *"
                        (regexp-replace* #rx"\n" sentence " ")
                        "")))))

;;; Package:
;;;   lac-camp/files
;;; Procedure:
;;;   read-tweet
;;; Parameters:
;;;   port, an input port
;;; Purpose:
;;;   Reads the next tweet from the port
;;; Produces:
;;;   tweet, a string
;;; Preconditions:
;;;   * port is open for reading
;;;   * port contains tweets in standard form
;;; Postconditions:
;;;   * tweet is the next tweet in the port.
;;;   * If there is no next tweet, tweet is the eof object
(define read-tweet
  (lambda (port)
    (let kernel ([tweet null])
      (let [(line (read-line port))]
        (if (eof-object? line)
            (if (not (null? tweet))
                (cons "TWEET" (reverse tweet))
                line)
            (let ([datum (regexp-replace #rx"[\r\n ]*$" line "")])
              (if (or (string=? datum "")
                      (regexp-match #rx"^-*$" datum))
                  (if (null? tweet)
                      (kernel tweet)
                      (cons "TWEET" (reverse tweet)))
                  (kernel (cons datum tweet)))))))))

;;; Package:
;;;   lac-camp/files
;;; Procedure:
;;;   read-word
;;; Parameters:
;;;   port, an input port
;;; Purpose:
;;;   Reads the next word from the port
;;; Produces:
;;;   word, a string
;;; Preconditions:
;;;   port is open for reading
;;; Postconditions:
;;;   word is the next word in the port.
(define read-word
  (lambda (port)
    (read-charseq port word-char?)))

;;; Package:
;;;   lac-camp/files
;;; Procedure:
;;;   read-until
;;; Parameters:
;;;   port, an input port
;;;   terminator, a character, string, or predicate
;;; Purpose:
;;;   Read the characters until you reach terminator (or eof) (or
;;;   the predicate holds..
;;; Produces:
;;;   str, a string
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   * str represents the sequence of characters from the file pointer
;;;     of port at the beginning until (a) the first occurence of terminator,
;;;     if terminator is a character, (b) a character that appears in terminator,
;;;     it terminator is a string, or (c) a character for which terminator
;;;     holds, if terminator is a predicate.  str does not include that
;;;     character.
;;;   * the file pointer has been advanced appropriately.
(define read-until
  (lambda (port terminator)
    (let [(pred? (cond
                   [(char? terminator)
                     (lambda (ch) (char=? ch terminator))]
                   [(string? terminator)
                    (lambda (ch) (string-contains? terminator (string ch)))]
                   [(procedure? terminator)
                    terminator]
                   [else
                    (error "Invalid parameter"
                           terminator
                           "expected a character, string, or unary predicate")]))]
      (let kernel ([chars null])
        (let ([ch (peek-char port)])
          (if (or (eof-object? ch) (pred? ch))
              (list->string (reverse chars))
              (kernel (cons (read-char port) chars))))))))

;;; Package:
;;;   lac-camp/files
;;; Procedure:
;;;   skip-char
;;; Parameters:
;;;   port, an open input port
;;;   ch, a character
;;; Purpose:
;;;   Skip over the next character in the input file if it is ch.
;;; Produces:
;;;   skipped?, a Boolean
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   * If (peek-char port) is ch, reads over the character and returns #t
;;;   * Otherwise, leaves the port unchanged and returns #f
(define skip-char
  (lambda (port ch)
    (let ([next (peek-char port)])
      (cond
        [(and (not (eof-object? next))
              (char=? next ch))
         (read-char port)
         #t]
        [else
         #f]))))

; +------------------+-----------------------------------------------
; | Local Procedures |
; +------------------+

;;; Procedure:
;;;   file->stuff
;;; Parameters:
;;;   fname, a string that names a file
;;;   read-thing, a procedure that reads from an input port
;;;   end?, a predicate
;;; Purpose:
;;;   Repeatedly reads from the file named by fname until it
;;;   encounters a value for which end? holds.
;;; Produces:
;;;   stuff, a list of things.
;;; Philosophy:
;;;   read-chars, read-lines, and read-words all had
;;;   the same structure.  This procedure attempts to
;;;   unify those structures.
(define file->stuff
  (lambda (fname read-thing end?)
    (let ([port (open-input-file fname)])
      (let kernel ()
        (let ([thing (read-thing port)])
          (cond
            [(end? thing)
             (close-input-port port)
             null]
            [else
             (cons thing (kernel))]))))))

;;; Procedure:
;;;   word-char?
;;; Parameters:
;;;   ch, a character
;;; Purpose:
;;;   Determine if ch is "word character".
;;; Produces:
;;;   ok?, a Boolean
(define word-char?
  (lambda (ch)
    (or (char-alphabetic? ch)
        (char-numeric? ch))))

