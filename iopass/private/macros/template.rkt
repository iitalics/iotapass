#lang racket/base
(provide
 raw-prod)

(require
 (for-syntax
  racket/base
  racket/match
  (rename-in syntax/parse [attribute @])
  "../syntax/bindings.rkt"
  "../repr/ids.rkt"
  "../types.rkt"))

(begin-for-syntax
  ;; language id id -> production
  (define (get-prod lang mv-id head-id)
    (define nt
      (match (language-lookup-metavar lang
                                      (syntax-e mv-id))
        [(? nonterminal-spec? nt) nt]
        [_ (raise-syntax-error #f
             "metavar does not refer to a nonterminal"
             mv-id)]))
    (match (findf (λ (pr) (eq? (production-head-symbol pr)
                               (syntax-e head-id)))
                  (nonterminal-spec-productions nt))
      [#f (raise-syntax-error #f
            "production not defined by nonterminal"
            head-id)]
      [pr pr]))

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
    [(_ (b:language-definition-binding mv:id)
        f:raw-function
        (head:id arg ...))
     #:do [(define pr=>ids (language-repr-ids-productions (@ b.repr-ids)))
           (define pr (get-prod (@ b.language) #'mv #'head))]
     #:with [ct pr pj] (hash-ref pr=>ids pr)
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
