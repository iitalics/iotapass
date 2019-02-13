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
  "../ast/decl.rkt"))

(begin-for-syntax
  (define-syntax-class lang+nt
    #:description "nonterminal name"
    #:attributes (language repr-ids nonterminal)
    [pattern (:language-definition-binding mv)
             #:declare mv (c:nonterminal-metavar-id (@ language))
             #:attr nonterminal (@ mv.spec)])

  (define-syntax-class (known-production-id nt)
    #:description "known production head symbol"
    #:attributes (production)
    [pattern head:id
             #:attr production
             (findf (λ (pr) (eq? (production-head-symbol pr)
                                 (syntax-e #'head)))
                    (nonterminal-spec-productions nt))
             #:fail-unless (@ production)
             (format "not a production of nonterminal '~a'"
                     (spec-description nt))])

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
     #:declare head (known-production-id (@ nonterminal))
     #:with [ct pr pj] (hash-ref (language-repr-ids-productions (@ repr-ids))
                                 (@ head.production))
     (match (@ f.fun)
       ['ctor #'(ct arg ...)]
       ['pred #'(pr arg ...)]
       [(list i) #`(pj arg ... #,i)])]))

;; =======================================================================================

(module+ test
  (require
   rackunit
   "define.rkt")

  (define-terminals T
    [i j ::= integer? #:compare =]
    [x y ::= symbol? #:compare eq?]
    [s ::= string?])

  (define-language L
    #:terminals T
    [e ::= (num i) (op s e ...)]
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
  (check-eq? (raw-prod (L d) #:proj 1 (def d-twelve)) e-+))
