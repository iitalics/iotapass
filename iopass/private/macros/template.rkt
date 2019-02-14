#lang racket/base
(provide
 raw-prod)

(require
 (for-syntax
  racket/base
  racket/match
  (rename-in syntax/parse [attribute @])
  (prefix-in c: "../syntax/classes.rkt")
  "../syntax/bindings.rkt"
  "../repr/ids.rkt"
  "../ast/decl.rkt"
  (prefix-in t: "../ast/template.rkt")))

(begin-for-syntax
  (define-syntax-class lang+mv
    #:description "language-scoped metavariable name"
    #:attributes (language repr-ids spec)
    [pattern (:language-definition-binding mv)
             #:declare mv (c:known-metavar-id (@ language))
             #:attr spec (@ mv.spec)])

  (define-syntax-class lang+nt
    #:description "nonterminal name"
    #:attributes (language repr-ids nonterminal)
    [pattern (:language-definition-binding mv)
             #:declare mv (c:nonterminal-metavar-id (@ language))
             #:attr nonterminal (@ mv.spec)])

  (define-splicing-syntax-class raw-function
    #:attributes (fun)
    [pattern {~seq} #:attr fun 'ctor]
    [pattern {~seq #:pred} #:attr fun 'pred]
    [pattern {~seq #:proj i} #:attr fun (list #'i)]))

;; -----------
;; raw-prod
;; -----------

(define-syntax raw-prod
  (syntax-parser
    [(_ :lang+nt
        f:raw-function
        (head arg ...))
     #:declare head (c:known-production-id (@ nonterminal))
     #:with [ct pr pj] (hash-ref (language-repr-ids-productions (@ repr-ids))
                                 (@ head.production))
     (match (@ f.fun)
       ['ctor #'(ct arg ...)]
       ['pred #'(pr arg ...)]
       [(list i) #`(pj arg ... #,i)])]))

;; -----------
;; template
;; -----------
(define-syntax template
  (syntax-parser
    [(_ :lang+mv tmpl)
     (compile-template
      (match (@ spec)
        [(? terminal-spec? tm)
         (syntax-parse #'tmpl
           [{~var || (c:terminal-template tm)}
            (@ value)])]
        [(? nonterminal-spec? nt)
         (syntax-parse #'tmpl
           [{~var || (c:nonterminal-template nt)}
            (@ value)])]))]))

(begin-for-syntax
  ;; template -> syntax
  (define (compile-template tmp)
    (match tmp
      [(t:unquoted stx)
       stx]
      [(t:datum stx)
       #`'#,stx])))

;; =======================================================================================

(module+ test
  (require
   rackunit
   syntax/macro-testing
   "define.rkt")

  (define-terminals T
    [i j ::= integer? #:compare =]
    [x y ::= symbol? #:compare eq?]
    [s ::= string?])

  (define-language L
    #:terminals T
    [e ::= (num . i) (op s e ...)]
    [d ::= (def x e)])

  ;; ---------------
  ;; 'raw-prod' tests
  ;; ---------------

  (define e-5 (raw-prod (L e) (num 5)))
  (define e-7 (raw-prod (L e) (num 7)))
  (define e-+ (raw-prod (L e) (op "+" (list e-5 e-7))))
  (define d-twelve (raw-prod (L d) (def 'twelve e-+)))

  (check-true (raw-prod (L e) #:pred (num e-5)))
  (check-true (raw-prod (L e) #:pred (num e-7)))
  (check-true (raw-prod (L e) #:pred (op e-+)))
  (check-true (raw-prod (L d) #:pred (def d-twelve)))
  (check-false (raw-prod (L e) #:pred (num e-+)))
  (check-false (raw-prod (L e) #:pred (num 5)))
  (check-false (raw-prod (L e) #:pred (op "5")))

  (check-equal? (raw-prod (L e) #:proj 0 (num e-5)) 5)
  (check-equal? (raw-prod (L e) #:proj 0 (num e-7)) 7)
  (check-equal? (raw-prod (L e) #:proj 0 (op e-+)) "+")
  (check-equal? (raw-prod (L e) #:proj 1 (op e-+)) (list e-5 e-7))
  (check-eq? (raw-prod (L d) #:proj 1 (def d-twelve)) e-+)

  ;; ---------------
  ;; 'template' tests
  ;; ---------------

  ; unquoted
  (check-eqv? (template (L i) ,(+ 5 6)) 11)
  (check-eq? (template (L e) ,e-5) e-5)
  ; datum
  (check-eqv? (template (L i) 5)  5)
  (check-eq? (template (L x) foo) 'foo)
  (check-exn #px"list datum not allowed"
             (λ () (convert-compile-time-error
                    (template (L i) (1 2)))))
  (check-exn #px"list datum not allowed"
             (λ () (convert-compile-time-error
                    (template (L i) ())))))
