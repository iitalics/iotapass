#lang racket/base
(provide
 parse-mt/form
 parse-mt/terminal
 parse-mt/nonterminal)

(require
 (prefix-in mt: "../ast/metaterm.rkt")
 (prefix-in c: "classes.rkt")
 "../ast/decl.rkt"
 (rename-in syntax/parse [attribute @])
 racket/match)

;; =======================================================================================

;; form syntax -> mt:metaterm
(define (parse-mt/form fm stx)
  (match fm
    [(form-list _ before repeat after)
     (parse-mt/list before repeat after stx)]
    [(metavar _ x)
     (raise-syntax-error #f
       "parsing metavar forms unimplemented"
       stx)]))

;; nonterminal-spec syntax -> mt:metaterm
(define (parse-mt/nonterminal nt stx)
  (syntax-parse stx
    [({~datum unquote} e)
     (mt:unquoted #'e)]

    [(head . body)
     #:declare head (c:known-production-id nt)
     (mt:prod (@ head.production)
              (parse-mt/form (production-form (@ head.production))
                             #'body))]))

;; terminal-spec syntax -> mt:metaterm
(define (parse-mt/terminal tm stx)
  (syntax-parse stx
    [({~datum unquote} e)
     (mt:unquoted #'e)]

    [d
     #:fail-when (or (null? (syntax-e #'d)) (pair? (syntax-e #'d)))
     "list datum not allowed"
     (mt:datum #'d)]))

;; [listof form] (or #f ellipsis) [listof form] syntax
;;   -> [listof mt:metaterm]
(define (parse-mt/list before repeat after stx)
  (syntax-parse stx
    [() '()]))
