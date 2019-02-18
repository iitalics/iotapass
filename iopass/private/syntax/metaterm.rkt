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

;; form language syntax -> mt:metaterm
(define (parse-mt/form fm lang stx)
  (match fm
    [(form-list _ before repeat after)
     (parse-mt/list before repeat after lang stx)]
    [(metavar mv-orig-stx x)
     (define (mv-fail)
       (raise-syntax-error #f
         "reference to undefined metavar: SHOULD BE IMPOSSIBLE"
         mv-orig-stx))
     (parse-mt (language-lookup-metavar lang x mv-fail)
               lang
               stx)]))

;; [listof form] (or #f ellipsis) [listof form] language syntax
;;   -> mt:metaterm (specifically: mt:multiple)
(define (parse-mt/list before repeat after lang stx)
  (syntax-parse stx
    [({~datum unquote} _)
     #:cut
     #:fail-when #t "cannot unquote here, expected a parenthesized list"
     (void)]

    [{~not (_ ...)}
     #:cut
     #:fail-when #t "expected a parenthesized list"
     (void)]

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
             (split-at next-stxs (- n-args n-min)))

           ; recur on subexpressions
           (define (parse/form fm stx) (parse-mt/form fm lang stx))
           (define before-mts
             (map parse/form before before-stxs))
           (define middle-mts
             (match repeat
               [#f '()]
               [(ellipsis rep-fm)
                (define (parse/rep stx) (parse-mt/form rep-fm lang stx))
                (list (mt:build (form-field-count rep-fm)
                                (map parse/rep middle-stxs)))]))
           (define after-mts
             (map parse/form after after-stxs))]

     (mt:multiple
      (append before-mts
              middle-mts
              after-mts))]))

(define (plural n)
  (if (= n 1) "" "s"))

(module+ test
  (require
   rackunit
   "../util/example-language-decls.rkt"
   "../util/syntax.rkt")

  ;; ------------
  ;; test 'parse-mt/X'
  ;; ------------

  (define fm-i^ (form-list #'[i]
                           (list (metavar #'i 'i))
                           #f
                           '()))
  (define fm-x-y (form-list #'[x y]
                            (list (metavar #'x 'x) (metavar #'y 'y))
                            #f
                            '()))
  (define fm-x-y^ (form-list #'[x y]
                             (list (metavar #'x 'x))
                             #f
                             (list (metavar #'y 'y))))

  (check-exn #px"foo: expected a parenthesized list"
             (λ () (parse-mt/form fm-x-c L #'foo)))
  (check-exn #px"cannot unquote here, expected a parenthesized list\n  at: \\(unquote foo\\)"
             (λ () (parse-mt/form fm-x-c L #',foo)))
  (check-exn #px"list datum not allowed"
             (λ () (parse-mt/terminal tm-i #'(1 2))))
  (check-exn #px"list datum not allowed"
             (λ () (parse-mt/terminal tm-i #'())))
  (check-exn #px"expected 2 arguments, got 1"
             (λ () (parse-mt/form fm-x-y L #'[foo])))
  (check-exn #px"expected 2 arguments, got 1"
             (λ () (parse-mt/form fm-x-y^ L #'[foo])))
  (check-exn #px"expected 1 argument, got 2"
             (λ () (parse-mt/form fm-i^ L #'[8 8])))
  (check-exn #px"expected at least 1 argument, got 0"
             (λ () (parse-mt/form fm-x-c L #'())))

  (check-match
   (parse-mt/nonterminal nt-ab L #'(A . ,(mk-a)))
   (mt:prod (== pr-A)
            (mt:unquoted (stx: (mk-a))
                         (== nt-ab))))

  (check-match
   (parse-mt/form fm-x-c L #'[foo (C) ,c2 ,c3])
   (mt:multiple
    (list (mt:datum (free-id= foo)
                    (== tm-xy))
          (mt:build 1
                    (list (mt:prod (== pr-C eq?) (mt:multiple '()))
                          (mt:unquoted (stx: c2) (== nt-c))
                          (mt:unquoted (stx: c3) (== nt-c))))))))
