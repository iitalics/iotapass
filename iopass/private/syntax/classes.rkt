#lang racket/base
(provide
 terminal-spec
 nonterminal-spec
 production
 form)

(require
 (prefix-in t: "../types.rkt")
 (for-template racket/base)
 (rename-in syntax/parse [attribute @]))

;; =======================================================================================

;; <terminal-spec> ::= [<id> ... ::= <pred-id>]
;;                   | [<id> ... ::= <pred-id> #:compare <eq-id>]

(define-syntax-class terminal-spec
  #:datum-literals (::=)
  #:attributes ([mv 1] contract equal value)
  [pattern [mv:id ...+ ::= ~!
                  contract:id
                  {~optional {~seq #:compare equal:id}
                             #:defaults ([equal #'equal?])}]
           #:attr value (t:terminal-spec this-syntax
                                         (map syntax-e (@ mv))
                                         #'contract
                                         #'equal)])
;; <form> ::= <id>
;;          | (<form> ...)
;;          | (<form> ...
;;             <form> ellipsis
;;             <form> ...)

(define-syntax-class ooo
  [pattern {~datum ...}])

(define-syntax-class form
  #:attributes (value)
  [pattern mv:id
           #:fail-when (eq? (syntax-e #'mv) '...)
           "unexpected ellipsis"
           #:attr value (t:metavar this-syntax (syntax-e #'mv))]

  [pattern (a:form ... b:form :ooo c:form ...)
           #:attr value (t:form-list this-syntax
                                     (@ a.value)
                                     (t:ellipsis (@ b.value))
                                     (@ c.value))]

  [pattern (a:form ...)
           #:attr value (t:form-list this-syntax
                                     (@ a.value)
                                     #f
                                     '())]

  [pattern (_ ... :ooo _ ... :ooo . _)
           #:fail-when #t
           "cannot have multiple ellipsis in list form"
           #:attr value #f])

;; (<id> . <form>)
(define-syntax-class production
  #:attributes (value)
  [pattern (head:id . form:form)
           #:attr value (t:production this-syntax
                                      (syntax-e #'head)
                                      (@ form.value))])

;; <nonterminal-spec> ::=
;;   [<id> ... ::= <production> ...]
(define-syntax-class nonterminal-spec
  #:datum-literals (::=)
  #:attributes ([mv 1] value)
  [pattern [mv:id ...+ ::= prod:production ...]
           #:attr value (t:nonterminal-spec this-syntax
                                            (map syntax-e (@ mv))
                                            (@ prod.value))])

;; <nonterminal-δ-spec> ::=
;;   [<id> ... += <production> ...
;;             -= <production> ...
;;             ...]

(define-syntax-class +=/-=
  [pattern {~datum +=}]
  [pattern {~datum -=}])

(define-syntax-class nonterminal-delta-spec
  #:attributes ([mv 1] add del)
  [pattern [mv:id ...+
                  {~seq pm:+=/-=
                        prod:production
                        ...+}
                  ...+]
           #:do [(define-values [ps ms]
                   (for/fold ([ps '()]
                              [ms '()])
                             ([p/m (in-list (@ pm))]
                              [prods (in-list (@ prod.value))])
                     (case (syntax-e p/m)
                       [(+=) (values (append ps prods) ms)]
                       [(-=) (values ps (append ms prods))]
                       [else (error 'nonterminal-delta-spec
                                    "expected += or -=, got "
                                    (syntax-e p/m))])))]
           #:attr add ps
           #:attr del ms])
