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
 "../util/syntax.rkt"
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
;;   -> mt:metaterm  (specifically mt:multiple)
(define (parse-mt/list before repeat after lang stx)

  ;; form syntax -> mt:metaterm
  (define (parse/form fm stx)
    (parse-mt/form fm lang stx))

  ;; form [listof (or syntax e)] -> mt:metaterm  (specifically mt:build)
  (define (parse/rep rep-fm terms)
    (mt:build
     (form-field-count rep-fm)
     (for/list ([t (in-list terms)])
       (match t
         [(e stx) (mt:e (parse/form rep-fm stx))]
         [stx (parse/form rep-fm stx)]))))

  (syntax-parse stx
    [({~datum unquote} _)
     #:cut
     #:fail-when #t "cannot unquote here, expected a parenthesized list"
     (void)]

    [{~not (_ ...)}
     #:cut
     #:fail-when #t "expected a parenthesized list"
     (void)]

    [(raw ...)
     ; parse terms followed by ellipsis
     #:do [(define terms (parse-list+ellipsis (@ raw)))]

     ; check arity
     #:do [(define n-args   (length terms))
           (define n-min    (+ (length before) (length after)))
           (define n-middle (- n-args n-min))]
     #:fail-unless (or repeat (= n-args n-min))
     (format "expected ~a argument~a, got ~a"
             n-min (plural n-min) n-args)
     #:fail-when (and repeat (< n-args n-min))
     (format "expected at least ~a argument~a, got ~a"
             n-min (plural n-min) n-args)

     ; split syntax into before/middle/after
     #:do [(define-values [before-terms next-terms]
             (split-at terms (length before)))
           (define-values [middle-terms after-terms]
             (split-at next-terms n-middle))]

     ; disallow ellipsis in before/after terms
     #:fail-when (for/or ([t (in-sequences
                              (in-list before-terms)
                              (in-list after-terms))])
                   (and (e? t) (e-syntax t)))
     "term may not be a repetition"

     (mt:multiple
      (append (map parse/form before before-terms)
              (map (λ (r) (parse/rep r middle-terms)) (ellipsis->list repeat))
              (map parse/form after after-terms)))]))

;; e ::= (e syntax)
; Indicates that the inner syntax is followed by an [e]llipsis. See 'parse-list+ellipsis'
; return value.
(struct e [syntax] #:transparent)

;; [listof syntax] -> [listof (or syntax e)]
; Wrap each syntax in the list which is followed by '...' in the (e ..) struct. Raises
; error for ellipsis in initial position, or two ellipsis in a row.
;   (parse-list+ellipsis (list #'x #'y #'... #'z]) =
;   (list #'x (e #'y) #'z)
(define (parse-list+ellipsis stxs)
  (let loop ([stxs stxs] [preceding #f])
    (match* {stxs preceding}
      ; end of list
      [{'() #f}    '()]
      [{'() thing} (list thing)]

      ; invalid ellipsis
      [{(cons (stx: ...) tl) #f}
       (raise-syntax-error #f "nothing preceding ellipsis" (car stxs))]
      [{(cons (stx: ...) tl) (e stx)}
       (raise-syntax-error #f "may not be followed by two ellipsis" stx)]

      ; valid ellipsis
      [{(cons (stx: ...) tl) stx}
       (loop tl (e stx))]

      ; element
      [{(cons hd tl) #f}
       (loop tl hd)]
      [{(cons hd tl) pre}
       (cons pre (loop tl hd))])))

(define (plural n)
  (if (= n 1) "" "s"))

(module+ test
  (require
   rackunit
   "../util/example-language-decls.rkt")

  ;; ------------
  ;; test 'parse-list+ellipsis'
  ;; ------------

  (define/syntax-parse ooo (quote-syntax ...))

  (check-match (parse-list+ellipsis (syntax->list #'[x y ooo z]))
               (list (stx: x)
                     (e (stx: y))
                     (stx: z)))

  (check-match (parse-list+ellipsis (syntax->list #'[y ooo z ooo]))
               (list (e (stx: y))
                     (e (stx: z))))

  (check-exn #px"...: nothing preceding ellipsis"
             (λ () (parse-list+ellipsis (list #'ooo #'x))))
  (check-exn #px"x: may not be followed by two ellipsis"
             (λ () (parse-list+ellipsis (list #'y #'x #'ooo #'ooo #'z))))
  (check-exn #px"unquote: may not be followed by two ellipsis"
             (λ () (parse-list+ellipsis (list #'y #',x #'ooo #'ooo #'z))))

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
  (define fm-x-x-y* (form-list #'[x x y ooo]
                               (list (metavar #'x 'x)
                                     (metavar #'x 'x))
                               (ellipsis (metavar #'y 'y))
                               '()))

  (check-exn #px"foo: expected a parenthesized list"
             (λ () (parse-mt/form fm-x-c L #'foo)))
  (check-exn #px"cannot unquote here, expected a parenthesized list\n  at: \\(unquote foo\\)"
             (λ () (parse-mt/form fm-x-c L #',foo)))
  (check-exn #px"list datum not allowed\n  at: \\(1 2\\)"
             (λ () (parse-mt/terminal tm-i #'(1 2))))
  (check-exn #px"list datum not allowed\n  at: \\(\\)"
             (λ () (parse-mt/terminal tm-i #'())))
  (check-exn #px"expected 2 arguments, got 1"
             (λ () (parse-mt/form fm-x-y L #'[foo])))
  (check-exn #px"expected 2 arguments, got 1"
             (λ () (parse-mt/form fm-x-y^ L #'[foo])))
  (check-exn #px"expected 1 argument, got 2"
             (λ () (parse-mt/form fm-i^ L #'[8 8])))
  (check-exn #px"expected at least 1 argument, got 0"
             (λ () (parse-mt/form fm-x-c L #'())))
  (check-exn #px"expected at least 2 arguments, got 1"
             (λ () (parse-mt/form fm-x-x-y* L #'[,foo ooo])))
  (check-exn #px"term may not be a repetition\n  at: foo"
             (λ () (parse-mt/form fm-x-x-y* L #'[foo ooo bar])))
  (check-exn #px"term may not be a repetition\n  at: bar"
             (λ () (parse-mt/form fm-x-y^ L #'[foo bar ooo])))

  (check-match
   (parse-mt/nonterminal nt-ab L #'(A . ,(mk-a)))
   (mt:prod (== pr-A)
            (mt:unquoted (stx: (mk-a))
                         (== nt-ab))))

  (check-match
   (parse-mt/form fm-x-c L #'[foo (C) ,c2 ,c3])
   (mt:multiple
    (list (mt:datum (free-id= foo) (== tm-xy))
          (mt:build 1
                    (list (mt:prod (== pr-C) (mt:multiple '()))
                          (mt:unquoted (stx: c2) (== nt-c))
                          (mt:unquoted (stx: c3) (== nt-c)))))))

  (check-match
   (parse-mt/form fm-x-c L #'[,x ,cs ooo])
   (mt:multiple
    (list (mt:unquoted (stx: x) _)
          (mt:build 1
                    (list (mt:e (mt:unquoted (stx: cs) _)))))))

  (check-match
   (parse-mt/form fm-x-c L #'[,x ,cs ooo ,d ,es ooo])
   (mt:multiple
    (list (mt:unquoted (stx: x) _)
          (mt:build 1
                    (list (mt:e (mt:unquoted (stx: cs) _))
                          (mt:unquoted (stx: d) _)
                          (mt:e (mt:unquoted (stx: es) _))))))))
