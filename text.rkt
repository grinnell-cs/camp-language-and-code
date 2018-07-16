#lang racket
(provide (all-defined out))

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

