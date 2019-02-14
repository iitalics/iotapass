#lang racket/base
(provide
 ; for definitions
 terminal-spec
 nonterminal-spec
 production
 form
 ; for templates or patterns
 known-metavar-id
 nonterminal-metavar-id
 terminal-metavar-id
 known-production-id
 ; for templates
 terminal-template
 nonterminal-template
 #;
 form-template)

(require
 (prefix-in ast: "../ast/decl.rkt")
 (prefix-in ast:t: "../ast/template.rkt")
 (for-template racket/base)
 (rename-in syntax/parse [attribute @])
 racket/match)

;; -------------------------------------------
;; patterns for define- macros
;; -------------------------------------------

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
           #`(ast:terminal-spec (quote-syntax #,this-syntax)
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
           #`(ast:terminal-spec (quote-syntax #,this-syntax)
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
           #`(ast:metavar (quote-syntax #,this-syntax) 'mv)]

  [pattern (a:form ... b:form :ooo c:form ...)
           #:with generate
           #`(ast:form-list (quote-syntax #,this-syntax)
                            (list a.generate ...)
                            (ast:ellipsis b.generate)
                            (list c.generate ...))]

  [pattern (a:form ...)
           #:with generate
           #`(ast:form-list (quote-syntax #,this-syntax)
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
           #`(ast:production (quote-syntax #,this-syntax)
                             'head
                             form.generate)])

;; <nonterminal-spec> ::=
;;   [<id> ... ::= <production> ...]
(define-syntax-class nonterminal-spec
  #:datum-literals (::=)
  #:attributes ([mv 1]              ; metavars
                generate            ; nonterminal-spec generation syntax
                pred-repr-id        ; predicate id in representation
                [prod-repr-ids 1])  ; ids in representation of each production
  [pattern [mv:id ...+ ::= ~! prod:production ...]
           #:with generate
           #`(ast:nonterminal-spec (quote-syntax #,this-syntax)
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

;; -------------------------------------------
;; patterns for 'template' or 'pattern' macros
;; -------------------------------------------

(define-syntax-class (known-metavar-id lang)
  #:attributes (spec)
  #:description "known metavariable"
  [pattern x:id
           #:attr spec (ast:language-lookup-metavar lang
                                                    (syntax-e #'x))
           #:fail-unless (@ spec)
           "metavariable not defined by language"])

(define-syntax-class (nonterminal-metavar-id lang)
  #:attributes (spec)
  #:description "nonterminal metavariable"
  [pattern {~var || (known-metavar-id lang)}
           #:fail-unless (ast:nonterminal-spec? (@ spec)) #f])

(define-syntax-class (terminal-metavar-id lang)
  #:attributes (spec)
  #:description "terminal metavariable"
  [pattern {~var || (known-metavar-id lang)}
           #:fail-unless (ast:terminal-spec? (@ spec)) #f])

(define-syntax-class (known-production-id nt)
  #:description "known production head symbol"
  #:attributes (production)
  [pattern head:id
           #:attr production (ast:nonterminal-production nt (syntax-e #'head))
           #:fail-unless (@ production)
           (format "not a production of nonterminal '~a'"
                   (ast:spec-description nt))])

(module+ test
  (require rackunit)

  (let* ([tm-x (ast:terminal-spec #'x '(x) #'symbol? #'eq?)]
         [nt-e (ast:nonterminal-spec #'e '(e) '())]
         [L (ast:make-language
             #'L
             'L
             (list tm-x)
             (list nt-e))])

    (syntax-parse #'[x e]
      [[{~var X (known-metavar-id L)}
        {~var E (known-metavar-id L)}]
       (check-eq? (@ X.spec) tm-x)
       (check-eq? (@ E.spec) nt-e)])

    (check-exn #px"metavariable not defined"
               (λ () (syntax-parse #'y
                       [{~var Y (known-metavar-id L)} 0])))

    (check-exn #px"expected nonterminal metavariable"
               (λ () (syntax-parse #'x
                       [{~var X (nonterminal-metavar-id L)} 0])))

    (check-exn #px"expected terminal metavariable"
               (λ () (syntax-parse #'e
                       [{~var E (terminal-metavar-id L)} 0])))))

;; -------------------------------------------
;; patterns for 'template' macros
;; -------------------------------------------

;; <terminal-template> ::=
;;   (unquote <expr>)
;;   <datum>
;;
;; tm : ast:terminal-spec
;; (@ value) : ast:t:template-node
(define-syntax-class (terminal-template tm)
  #:description (format "terminal '~a' template"
                        (ast:spec-description tm))
  #:attributes (value)
  [pattern ({~datum unquote} e)
           #:attr value (ast:t:unquoted #'e)]
  [pattern d
           #:fail-when (or (null? (syntax-e #'d)) (pair? (syntax-e #'d)))
           "list datum not allowed"
           #:attr value (ast:t:datum #'d)])

;; <nonterminal-template> ::=
;;   (unquote <expr>)
;;   (<head-sym> . <form-template>)
;;
;; nt : ast:nonterminal-spec
;; (@ value) : ast:t:template
(define-syntax-class (nonterminal-template nt)
  #:description (format "nonterminal '~a' template"
                        (ast:spec-description nt))
  #:attributes (value)
  [pattern ({~datum unquote} e)
           #:attr value (ast:t:unquoted #'e)]

  #;
  [pattern (head:id . body-tm)
           #:attr value ('???)])

;; <form-list-template> ::=
;;   (<form-template> ...)
;;   (<form-template> ... <form-template> ooo <form-template> ...)
;;
;; lang : ast:language
;; nt : ast:nonterminal-spec
;; (@ value) : [listof ast:t:template]
(define-syntax-class (form-list-template lang fl)
  #:description "parenthesized list template"
  #:attributes (value)
  [pattern ()
           #:attr value '()])

#;
;; <form-template> ::=
;;   <terminal-template>     if 'fm' is a terminal metavar
;;   <nonterminal-template>  if 'fm' is a nonterminal metavar
;;   <form-list-template>    if 'fm' is a form-list
;;
;; lang : ast:language
;; fm : ast:form
;; (@ value) : [listof ast:t:template]
(define-syntax-class (form-template lang fm)
  #:description "template"
  #:attributes (value)
  [pattern _
           #:attr value
           (match fm
             [(ast:metavar _ mv)
              ('???)]
             [(ast:form-list _ before repeat after)
              ('???)])
           #:fail-when (form-template-error? (@ value))
           (form-template-error-msg (@ value))])
