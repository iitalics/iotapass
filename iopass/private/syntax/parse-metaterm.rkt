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
 racket/list
 threading)

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
     (mt:unquoted #'e)]

    [d
     #:fail-when (or (null? (syntax-e #'d)) (pair? (syntax-e #'d)))
     "list datum not allowed"
     (mt:datum #'d)]))

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

     (match repeat
       [#f
        (append-map parse/form
                    (append before after)
                    (@ e))]

       [(ellipsis rep-fm)
        (define (parse/rep-form stx)
          (parse-mt/form rep-fm lang stx))
        (define n-before (length before))
        (define n-mid (- n-args n-min))
        (let*-values (; split syntax into (before ... middle ... after ...)
                      [(before-stxs next-stxs) (split-at (@ e) n-before)]
                      [(middle-stxs after-stxs) (split-at next-stxs n-mid)]
                      ; parse syntax recursively; middle needs to be "transposed"
                      [(before-mts) (append-map parse/form before before-stxs)]
                      [(middle-mts) (map/transposed parse/rep-form middle-stxs)]
                      [(after-mts) (append-map parse/form after after-stxs)])
          (append before-mts
                  middle-mts
                  after-mts))])]))

(define (plural n)
  (if (= n 1) "" "s"))

;; [X -> [List Y ...]] [Listof X] -> [List [Listof Y] ...]
(define/match (map/transposed f l)
  [{_ '()}         '()]
  [{_ (list x)}    (map list (f x))]
  [{_ (cons x xs)} (map cons (f x) (map/transposed f xs))])

(module+ test
  (require
   rackunit
   "util.rkt")

  (define-syntax-rule (check-null? x) (check-equal? x '()))
  (check-null? (map/transposed (λ (i) '()) (range 4)))
  (check-null? (map/transposed (λ (i) (list i i)) '()))
  (check-equal? (map/transposed (λ (i) (list i (* i i))) (range 4))
                (list (list 0 1 2 3)
                      (list 0 1 4 9))))
