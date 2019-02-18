#lang racket/base
(provide
 (all-defined-out))

(require
 (for-template racket/base)
 "../ast/decl.rkt")

;; (define-terminals T
;;   [x y ::= symbol?]
;;   [i ::= exact-integer?])

(define tm-xy (terminal-spec #'[x y ::=] '(x y) #'symbol? #'eq?))
(define tm-i  (terminal-spec #'[i ::=] '(i) #'exact-integer? #'=))

;; (define-language L
;;   [a b ::= (A . a) (B x c ...)]
;;   [c   ::= (C)])

(define fm-x-c (form-list #'(x y)
                          (list (metavar #'x 'x))
                          (ellipsis (metavar #'c 'c))
                          '()))
(define pr-A  (production #'(A a) 'A (metavar #'a 'a)))
(define pr-B  (production #'(B x c) 'B fm-x-c))
(define pr-C  (production #'(C) 'C (form-list #'() '() #f '())))
(define nt-ab (nonterminal-spec #'[a b ::=] '(a b) (list pr-A pr-B)))
(define nt-c  (nonterminal-spec #'[c ::=] '(c) (list pr-C)))

(define L
  (make-language #'L
                 'L
                 (list tm-xy tm-i)
                 (list nt-ab nt-c)))
