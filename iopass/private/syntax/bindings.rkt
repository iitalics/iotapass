#lang racket/base
(provide
 terminals-definition
 terminals-definition-binding
 get-terminals

 language-definition
 language-definition-binding
 get-language)

(require
 (prefix-in τ/ "../types.rkt")
 syntax/parse)

;; =======================================================================================

(define (not-an-expression-procedure self stx)
  (raise-syntax-error #f "this form cannot be used an expression" stx))

;; -----------
;; (terminals-definition τ/terminal-set)
;; for syntax bindings created by (define-terminals ....)

(struct terminals-definition
  [terminals]
  #:property prop:procedure not-an-expression-procedure)

(define-syntax-class terminals-definition-binding
  #:description "terminal set binding"
  #:attributes (terminals)
  [pattern x:id
           #:do [(define lv (syntax-local-value #'x (λ () #f)))]
           #:when (terminals-definition? lv)
           #:attr terminals (terminals-definition-terminals lv)])

;; identifier -> τ/terminal-set
(define (get-terminals id)
  (terminals-definition-terminals
   (syntax-local-value id)))

;; -----------
;; (language-definition τ/language)
;; for syntax bindings created by (define-language ....)

(struct language-definition
  [language]
  #:property prop:procedure not-an-expression-procedure)

(define-syntax-class language-definition-binding
  #:description "language binding"
  #:attributes (language)
  [pattern x:id
           #:do [(define lv (syntax-local-value #'x (λ () #f)))]
           #:when (language-definition? lv)
           #:attr language (language-definition-language lv)])

;; identifier -> τ/language
(define (get-language id)
  (language-definition-language
   (syntax-local-value id)))
