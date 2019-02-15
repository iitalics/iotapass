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
  "../syntax/parse-metaterm.rkt"
  "../repr/ids.rkt"
  "../ast/decl.rkt"
  (prefix-in mt: "../ast/metaterm.rkt")))

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
    #:attributes (fun head [arg 1])
    [pattern {~seq (head:id arg ...)}
             #:attr fun 'ctor]
    [pattern {~seq #:pred head:id obj}
             #:with [arg ...] #'[obj]
             #:attr fun 'pred]
    [pattern {~seq #:proj (head i) obj}
             #:with [arg ...] #'[obj i]
             #:attr fun 'proj]))

;; -----------
;; raw-prod
;; -----------

(define-syntax raw-prod
  (syntax-parser
    [(_ :lang+nt
        f:raw-function)
     #:with {~var || (c:known-production-id (@ nonterminal))} (@ f.head)
     #:with [ct pr pj] (hash-ref (language-repr-ids-productions (@ repr-ids))
                                 (@ production))
     (match (@ f.fun)
       ['ctor #'(ct f.arg ...)]
       ['pred #'(pr f.arg ...)]
       ['proj #'(pj f.arg ...)])]))

;; -----------
;; template
;; -----------

(define-syntax template
  (syntax-parser
    [(_ :lang+mv tmpl)
     (compile-template (@ repr-ids)
                       (parse-mt (@ spec)
                                 (@ language)
                                 #'tmpl))]))

(begin-for-syntax
  ;; language-repr-ids metaterm -> syntax
  (define (compile-template repr-ids mt)
    (define pr=>ids (language-repr-ids-productions repr-ids))
    (let compile1 ([mt mt])
      (match mt
        [(mt:unquoted stx) stx]
        [(mt:prod pr args)
         (define/syntax-parse [ctor _ _] (hash-ref pr=>ids pr))
         #`(ctor #,@(map compile1 args))]))))

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
    [e ::=
       (pi)
       (num . i)
       (if e e e)
       (op s e ...)]
    [d ::= (def x e)])

  ;; ---------------
  ;; 'raw-prod' tests
  ;; ---------------

  (define e-pi (raw-prod (L e) (pi)))
  (define e-5 (raw-prod (L e) (num 5)))
  (define e-7 (raw-prod (L e) (num 7)))
  (define e-+ (raw-prod (L e) (op "+" (list e-5 e-7))))
  (define d-twelve (raw-prod (L d) (def 'twelve e-+)))

  (check-true (raw-prod (L e) #:pred num e-5))
  (check-true (raw-prod (L e) #:pred num e-7))
  (check-true (raw-prod (L e) #:pred op e-+))
  (check-true (raw-prod (L d) #:pred def d-twelve))
  (check-false (raw-prod (L e) #:pred num e-+))
  (check-false (raw-prod (L e) #:pred num 5))
  (check-false (raw-prod (L e) #:pred op "5"))

  (check-equal? (raw-prod (L e) #:proj [num 0] e-5) 5)
  (check-equal? (raw-prod (L e) #:proj [num 0] e-7) 7)
  (check-equal? (raw-prod (L e) #:proj [op 0] e-+) "+")
  (check-equal? (raw-prod (L e) #:proj [op 1] e-+) (list e-5 e-7))
  (check-eq? (raw-prod (L d) #:proj [def 1] d-twelve) e-+)

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
                    (template (L i) ()))))

  ; productions
  (check-equal? (template (L e) (num . 5)) e-5)
  (check-equal? (template (L e) (pi)) e-pi)
  (check-equal? (template (L d) (def twelve ,e-+)) d-twelve))
