#lang racket/base
(provide
 terminal-spec
 nonterminal-spec
 nonterminal-delta-spec
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
  #:attributes (value generate [definitions 1])
  [pattern [mv:id ...+ ::=
                  contract:id
                  {~optional {~seq #:compare equal:id}
                             #:defaults ([equal #'equal?])}]
           #:with [definitions ...] #'[]
           #:attr value (types:terminal-spec this-syntax
                                             (map syntax-e (@ mv))
                                             #'contract
                                             #'equal)
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
           #:attr value (types:terminal-spec this-syntax
                                             (map syntax-e (@ mv))
                                             #'predicate-id
                                             #'eq?)
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
  #:attributes (value generate)
  [pattern mv:id
           #:fail-when (eq? (syntax-e #'mv) '...)
           "unexpected ellipsis"
           #:attr value (types:metavar this-syntax (syntax-e #'mv))
           #:with generate
           #`(types:metavar (quote-syntax #,this-syntax) 'mv)]

  [pattern (a:form ... b:form :ooo c:form ...)
           #:attr value (types:form-list this-syntax
                                         (@ a.value)
                                         (types:ellipsis (@ b.value))
                                         (@ c.value))
           #:with generate
           #`(types:form-list (quote-syntax #,this-syntax)
                              (list a.generate ...)
                              (types:ellipsis b.generate)
                              (list c.generate ...))]

  [pattern (a:form ...)
           #:attr value (types:form-list this-syntax
                                         (@ a.value)
                                         #f
                                         '())
           #:with generate
           #`(types:form-list (quote-syntax #,this-syntax)
                              (list a.generate ...)
                              #f
                              '())]

  [pattern (:ooo . _)
           #:fail-when #t
           "form must precede ellipsis"
           #:attr value #f #:with generate #f]

  [pattern (_ ... :ooo _ ... :ooo . _)
           #:fail-when #t
           "cannot have multiple ellipsis in list form"
           #:attr value #f #:with generate #f])

;; (<id> . <form>)
(define-syntax-class production
  #:attributes (value generate)
  [pattern (head:id . form:form)
           #:attr value (types:production this-syntax
                                      (syntax-e #'head)
                                      (@ form.value))
           #:with generate
           #`(types:production (quote-syntax #,this-syntax)
                           'head
                           form.generate)])

;; <nonterminal-spec> ::=
;;   [<id> ... ::= <production> ...]
(define-syntax-class nonterminal-spec
  #:datum-literals (::=)
  #:attributes ([mv 1]              ; metavars
                value               ; types:nonterminal-spec value
                generate            ; types:nonterminal-spec generation syntax
                pred-repr-id        ; predicate id in representation
                [prod-repr-ids 1])  ; ids in representation of each production
  [pattern [mv:id ...+ ::= ~! prod:production ...]
           #:attr value (types:nonterminal-spec this-syntax
                                                (map syntax-e (@ mv))
                                                (@ prod.value))
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
                   (for/fold ([ps '()] [ms '()])
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

;; =======================================================================================

(module+ test
  (require
   rackunit
   racket/match
   (for-syntax racket/base))

  (define-syntax-rule (check-parse syntax-class
                                   template
                                   output-pattern)
    (check-match (syntax-parse (quote-syntax template)
                   [{~var x syntax-class}
                    (@ x.value)])
                 output-pattern))

  (define-syntax-rule (check-parse-exn syntax-class
                                       template
                                       error-regex)
    (check-exn error-regex
               (λ () (syntax-parse (quote-syntax template)
                       [{~var || syntax-class} (void)]))))

  (define-match-expander free-id=
    (λ (stx)
      (syntax-case stx ()
        [(_ id) #'(== #'id free-identifier=?)])))

  ;; --------------------------------------
  ;; Test parsing

  (check-parse terminal-spec
               [i j ::= integer?]
               (types:terminal-spec _
                                    '(i j)
                                    (free-id= integer?)
                                    (free-id= equal?)))

  (check-parse terminal-spec
               [i j ::= integer? #:compare =]
               (types:terminal-spec _
                                    '(i j)
                                    (free-id= integer?)
                                    (free-id= =)))

  (check-parse form
               (n [x i] ... m r)
               (types:form-list _
                                (list (types:metavar _ 'n))
                                (types:ellipsis (types:form-list _
                                                                 (list (types:metavar _ 'x)
                                                                       (types:metavar _ 'i))
                                                                 #f
                                                                 '()))
                                (list (types:metavar _ 'm)
                                      (types:metavar _ 'r))))

  (check-parse terminal-spec
               [binary ::= [+ - * /]]
               (types:terminal-spec _
                                    '(binary)
                                    (? identifier?)
                                    (free-id= eq?)))

  (check-parse form
               ()
               (types:form-list _ '() #f '()))

  (check-parse-exn form
                   (a ... b ...)
                   #px"cannot have multiple ellipsis")

  (check-parse-exn form
                   (... b)
                   #px"form must precede ellipsis")

  (check-parse production
               (app C x ...)
               (types:production _
                                 'app
                                 (types:form-list _
                                                  (list (types:metavar _ 'C))
                                                  (types:ellipsis (types:metavar _ 'x))
                                                  '())))

  (check-parse nonterminal-spec
               [e f ::= (let [x e] e) (@ atom)]
               (types:nonterminal-spec _
                                       '(e f)
                                       (list (types:production _ 'let _)
                                             (types:production _ '@ _)))))
