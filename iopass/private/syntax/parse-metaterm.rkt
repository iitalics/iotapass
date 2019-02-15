#lang racket/base
(provide
 parse-mt
 parse-mt/terminal
 parse-mt/nonterminal)

(require
 (prefix-in mt: "../ast/metaterm.rkt")
 (prefix-in c: "classes.rkt")
 "../ast/decl.rkt"
 (rename-in syntax/parse [attribute @])
 racket/match
 racket/list)

;; =======================================================================================

;; spec language syntax -> mt:metaterm
(define (parse-mt s lang stx)
  (match s
    [(? nonterminal-spec? nt)
     (parse-mt/nonterminal nt lang stx)]
    [(? terminal-spec? tm)
     (parse-mt/terminal tm stx)]))

;; form language syntax -> [listof mt:metaterm]
(define (parse-mt/form fm lang stx)
  (match fm
    [(form-list _ before repeat after)
     (parse-mt/list before repeat after lang stx)]
    [(metavar mv-orig-stx x)
     (define (mv-fail)
       (raise-syntax-error #f
         "reference to undefined metavar: SHOULD BE IMPOSSIBLE"
         mv-orig-stx))
     (list (parse-mt (language-lookup-metavar lang x mv-fail)
                     lang
                     stx))]))

;; nonterminal-spec language syntax -> mt:metaterm
(define (parse-mt/nonterminal nt lang stx)
  (syntax-parse stx
    [({~datum unquote} e)
     (mt:unquoted #'e)]

    [(head . body)
     #:declare head (c:known-production-id nt)
     (mt:prod (@ head.production)
              (parse-mt/form (production-form (@ head.production))
                             lang
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

;; [listof form] (or #f ellipsis) [listof form] language syntax
;;   -> [listof mt:metaterm]
(define (parse-mt/list before repeat after lang stx)
  (syntax-parse stx
    [(e ...)
     ; implement this later..
     #:fail-when repeat
     "unimplemented: metaterms for forms with ellipsis"

     ; check arity
     #:do [(define n-args (length (@ e)))
           (define n-min (+ (length before)
                            (length after)))]
     #:fail-unless (or repeat (= n-args n-min))
     (format "expected ~a argument~a, got ~a"
             n-min (plural n-min) n-args)
     #:fail-when (and repeat (< n-args n-min))
     (format "expected at least ~a argument~a, got ~a"
             n-min (plural n-min) n-args)

     (append-map (λ (fm stx)
                   (parse-mt/form fm lang stx))
                 (append before after)
                 (@ e))]))

(define (plural n)
  (if (= n 1) "" "s"))
