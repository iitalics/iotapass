#lang racket/base
(provide
 terminal-spec
 nonterminal-spec
 production
 form)

(require
 (prefix-in types: "../types.rkt")
 (for-template racket/base)
 (rename-in syntax/parse [attribute @]))

;; =======================================================================================

;; <terminal-spec> ::=
;;     [<id> ... ::= <pred-id>]
;;   | [<id> ... ::= <pred-id> #:compare <eq-id>]
;;   | [<id> ... ::= [<symbol> ...]]

(define-syntax-class terminal-spec
  #:datum-literals (::=)
  #:attributes (generate [definitions 1])
  [pattern [mv:id ...+ ::=
                  contract:id
                  {~optional {~seq #:compare equal:id}
                             #:defaults ([equal #'equal?])}]
           #:with [definitions ...] #'[]
           #:with generate
           #`(types:terminal-spec (quote-syntax #,this-syntax)
                                  '(mv ...)
                                  (quote-syntax contract)
                                  (quote-syntax equal))]

  [pattern [mv:id ...+ ::=
                  [symbol:id ...]]
           #:with [predicate-id] (generate-temporaries #'[∈?])
           #:with [definitions ...]
           #'[(define (predicate-id x)
                (and (symbol? x)
                     (or (eq? x 'symbol)
                         ...)))]
           #:with generate
           #`(types:terminal-spec (quote-syntax #,this-syntax)
                                  '(mv ...)
                                  (quote-syntax predicate-id)
                                  #'eq?)])
;; <form> ::= <id>
;;          | (<form> ...)
;;          | (<form> ...
;;             <form> ellipsis
;;             <form> ...)

(define-syntax-class ooo
  #:description "ellipsis"
  [pattern {~datum ...}])

(define-syntax-class form
  #:attributes (generate)
  [pattern mv:id
           #:fail-when (eq? (syntax-e #'mv) '...)
           "unexpected ellipsis"
           #:with generate
           #`(types:metavar (quote-syntax #,this-syntax) 'mv)]

  [pattern (a:form ... b:form :ooo c:form ...)
           #:with generate
           #`(types:form-list (quote-syntax #,this-syntax)
                              (list a.generate ...)
                              (types:ellipsis b.generate)
                              (list c.generate ...))]

  [pattern (a:form ...)
           #:with generate
           #`(types:form-list (quote-syntax #,this-syntax)
                              (list a.generate ...)
                              #f
                              '())]

  [pattern (:ooo . _)
           #:fail-when #t
           "form must precede ellipsis"
           #:with generate #f]

  [pattern (_ ... :ooo _ ... :ooo . _)
           #:fail-when #t
           "cannot have multiple ellipsis in list form"
           #:with generate #f])

;; (<id> . <form>)
(define-syntax-class production
  #:attributes (generate)
  [pattern (head:id . form:form)
           #:with generate
           #`(types:production (quote-syntax #,this-syntax)
                           'head
                           form.generate)])

;; <nonterminal-spec> ::=
;;   [<id> ... ::= <production> ...]
(define-syntax-class nonterminal-spec
  #:datum-literals (::=)
  #:attributes ([mv 1]              ; metavars
                generate            ; types:nonterminal-spec generation syntax
                pred-repr-id        ; predicate id in representation
                [prod-repr-ids 1])  ; ids in representation of each production
  [pattern [mv:id ...+ ::= ~! prod:production ...]
           #:with generate
           #`(types:nonterminal-spec (quote-syntax #,this-syntax)
                                     '(mv ...)
                                     (list prod.generate
                                           ...))
           #:with [pred-repr-id] (generate-temporaries #'[nt.pred])
           #:with [prod-repr-ids ...] (for/list ([_ (in-list (@ prod))])
                                        (generate-temporaries #'[pr.ctor pr.pred pr.proj]))])

;; <nonterminal-δ-spec> ::=
;;   [<id> ... += <production> ...
;;             -= <production> ...
;;             ...]
;; (TODO LATER)
