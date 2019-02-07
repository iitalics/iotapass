#lang racket/base
(provide
 terminal-spec
 nonterminal-spec
 nonterminal-delta-spec
 production
 form)

(require
 (prefix-in τ/ "../types.rkt")
 (for-template racket/base)
 (rename-in syntax/parse [attribute @]))

;; =======================================================================================

;; <terminal-spec> ::= [<id> ... ::= <pred-id>]
;;                   | [<id> ... ::= <pred-id> #:compare <eq-id>]

(define-syntax-class terminal-spec
  #:datum-literals (::=)
  #:attributes (value generate)
  [pattern [mv:id ...+ ::= ~!
                  contract:id
                  {~optional {~seq #:compare equal:id}
                             #:defaults ([equal #'equal?])}]
           #:attr value (τ/terminal-spec this-syntax
                                         (map syntax-e (@ mv))
                                         #'contract
                                         #'equal)
           #:with generate
           #`(τ/terminal-spec (quote-syntax #,this-syntax)
                              '(mv ...)
                              (quote-syntax contract)
                              (quote-syntax equal))])

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
           #:attr value (τ/metavar this-syntax (syntax-e #'mv))
           #:with generate
           #`(τ/metavar (quote-syntax #,this-syntax) 'mv)]

  [pattern (a:form ... b:form :ooo c:form ...)
           #:attr value (τ/form-list this-syntax
                                     (@ a.value)
                                     (τ/ellipsis (@ b.value))
                                     (@ c.value))
           #:with generate
           #`(τ/form-list (quote-syntax #,this-syntax)
                          (list a.generate ...)
                          (τ/ellipsis b.generate)
                          (list c.generate ...))]

  [pattern (a:form ...)
           #:attr value (τ/form-list this-syntax
                                     (@ a.value)
                                     #f
                                     '())
           #:with generate
           #`(τ/form-list (quote-syntax #,this-syntax)
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
           #:attr value (τ/production this-syntax
                                      (syntax-e #'head)
                                      (@ form.value))
           #:with generate
           #`(τ/production (quote-syntax #,this-syntax)
                           'head
                           form.generate)])

;; <nonterminal-spec> ::=
;;   [<id> ... ::= <production> ...]
(define-syntax-class nonterminal-spec
  #:datum-literals (::=)
  #:attributes ([mv 1] value generate)
  [pattern [mv:id ...+ ::= ~! prod:production ...]
           #:attr value (τ/nonterminal-spec this-syntax
                                            (map syntax-e (@ mv))
                                            (@ prod.value))
           #:with generate
           #`(τ/nonterminal-spec (quote-syntax #,this-syntax)
                                 '(mv ...)
                                 (list prod.generate
                                       ...))])

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
               (τ/terminal-spec _
                                '(i j)
                                (free-id= integer?)
                                (free-id= equal?)))

  (check-parse terminal-spec
               [i j ::= integer? #:compare =]
               (τ/terminal-spec _
                                '(i j)
                                (free-id= integer?)
                                (free-id= =)))

  (check-parse form
               (n [x i] ... m r)
               (τ/form-list _
                            (list (τ/metavar _ 'n))
                            (τ/ellipsis (τ/form-list _
                                                     (list (τ/metavar _ 'x)
                                                           (τ/metavar _ 'i))
                                                     #f
                                                     '()))
                            (list (τ/metavar _ 'm)
                                  (τ/metavar _ 'r))))

  (check-parse form
               ()
               (τ/form-list _ '() #f '()))

  (check-parse-exn form
                   (a ... b ...)
                   #px"cannot have multiple ellipsis")

  (check-parse-exn form
                   (... b)
                   #px"form must precede ellipsis")

  (check-parse production
               (app C x ...)
               (τ/production _
                             'app
                             (τ/form-list _
                                          (list (τ/metavar _ 'C))
                                          (τ/ellipsis (τ/metavar _ 'x))
                                          '())))

  (check-parse nonterminal-spec
               [e f ::= (let [x e] e) (@ atom)]
               (τ/nonterminal-spec _
                                   '(e f)
                                   (list (τ/production _ 'let _)
                                         (τ/production _ '@ _)))))
