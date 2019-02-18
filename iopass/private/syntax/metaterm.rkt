#lang racket/base
(provide
 parse-mt
 parse-mt/terminal
 parse-mt/nonterminal)

(require
 (prefix-in c: "classes.rkt")
 (prefix-in mt: "../ast/metaterm.rkt")
 (only-in "../repr/structs.rkt" form-field-count)
 "../ast/decl.rkt"
 (rename-in syntax/parse [attribute @])
 racket/match
 racket/list)

;; =======================================================================================

;; spec language syntax -> mt:metaterm
(define (parse-mt s lang stx)
  (match s
    [(? terminal-spec? tm)
     (parse-mt/terminal tm stx)]
    [(? nonterminal-spec? nt)
     (parse-mt/nonterminal nt lang stx)]))

;; terminal-spec syntax -> mt:metaterm
(define (parse-mt/terminal tm stx)
  (syntax-parse stx
    [({~datum unquote} e)
     (mt:unquoted #'e tm)]

    [d
     #:fail-when (or (null? (syntax-e #'d)) (pair? (syntax-e #'d)))
     "list datum not allowed"
     (mt:datum #'d tm)]))

;; nonterminal-spec language syntax -> mt:metaterm
(define (parse-mt/nonterminal nt lang stx)
  (syntax-parse stx
    [({~datum unquote} e)
     (mt:unquoted #'e nt)]

    [(head . body)
     #:declare head (c:known-production-id nt)
     (mt:prod (@ head.production)
              (parse-mt/form (production-form (@ head.production))
                             lang
                             #'body))]))

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

;; [listof form] (or #f ellipsis) [listof form] language syntax
;;   -> [listof mt:metaterm]
(define (parse-mt/list before repeat after lang stx)
  (define (parse/form fm stx)
    (parse-mt/form fm lang stx))

  (syntax-parse stx
    [(e ...)
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

     #:do [; split syntax into before/middle/after
           (define-values [before-stxs next-stxs]
             (split-at (@ e) (length before)))
           (define-values [middle-stxs after-stxs]
             (split-at next-stxs (- n-args n-min)))]

     (append (append-map parse/form before before-stxs)
             (match repeat
               [#f '()]
               [(ellipsis rep-fm)
                (map/transposed (form-field-count rep-fm)
                                (λ (stx) (parse-mt/form rep-fm lang stx))
                                middle-stxs)])
             (append-map parse/form after after-stxs))]))

(define (plural n)
  (if (= n 1) "" "s"))

;; (n : nat) [X -> (list Y ...n)] [listof X] -> (list [listof Y] ...n)
(define (map/transposed n f l)
  (if (null? l)
    (make-list n '())
    (map cons
         (f (car l))
         (map/transposed n f (cdr l)))))

(module+ test
  (require
   rackunit
   "../util/example-language-decls.rkt"
   "../util/syntax.rkt")

  (check-equal? (map/transposed 0 (λ (i) '()) (range 4))
                '())
  (check-equal? (map/transposed 2 (λ (i) (list i i)) '())
                '[() ()])
  (check-equal? (map/transposed 2 (λ (i) (list i (* i i))) (range 4))
                (list (list 0 1 2 3)
                      (list 0 1 4 9)))

  (check-match (parse-mt/form fm-x-c
                              L
                              #'[foo (C) ,c2 ,c3])
               (list
                (mt:datum (app syntax-e (== 'foo)) (== tm-xy eq?))
                (list (mt:prod (== pr-C eq?) '())
                      (mt:unquoted (free-id= c2) (== nt-c eq?))
                      (mt:unquoted (free-id= c3) (== nt-c eq?))))))
