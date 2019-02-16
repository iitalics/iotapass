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
    [(hd :lang+mv tmpl)
     (compile-template #'hd
                       (@ repr-ids)
                       (parse-mt (@ spec)
                                 (@ language)
                                 #'tmpl))]))

(begin-for-syntax
  ;; syntax language-repr-ids metaterm -> syntax
  (define (compile-template macro-head repr-ids mt)
    (define pr=>ids (language-repr-ids-productions repr-ids))
    (let compile1 ([mt mt])
      (match mt
        [(mt:unquoted stx s)
         (make-spec-protect repr-ids macro-head s stx)]
        [(mt:datum stx s)
         (make-spec-protect repr-ids macro-head s #`'#,stx)]
        [(mt:prod pr args)
         (define/syntax-parse [ctor _ _] (hash-ref pr=>ids pr))
         #`(ctor #,@(map compile1 args))]
        [(list mts ...)
         #`(vector-immutable #,@(map compile1 mts))])))

  ;; language-repr-ids syntax spec syntax -> syntax
  (define (make-spec-protect repr-ids macro-head s value-stx)
    #`(protect-value '#,macro-head
                     #,(spec-predicate s repr-ids)
                     '#,(spec-expectation-string s)
                     #,value-stx))

  ;; spec language-repr-ids -> identifier
  (define (spec-predicate s repr-ids)
    (match s
      [(? nonterminal-spec? nt)
       (hash-ref (language-repr-ids-predicates repr-ids) nt)]
      [(? terminal-spec? tm)
       (terminal-spec-contract-id tm)]))

  ;; spec -> string
  (define (spec-expectation-string s)
    (match s
      [(? nonterminal-spec? nt)
       (format "nonterminal '~a'" (spec-description nt))]
      [(? terminal-spec? tm)
       (symbol->string (syntax-e (terminal-spec-contract-id tm)))])))

;; any [any -> bool | X] string any -> X
(define (protect-value macro-head
                       predicate
                       expect-string
                       value)
  (if (predicate value)
    value
    (raise-argument-error macro-head expect-string value)))

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
       (op s e ...)
       (print e)
       (let ([x e] ...) e)]
    [d ::= (def x e)])

  ;; ---------------
  ;; 'raw-prod' tests
  ;; ---------------

  (define e-pi (raw-prod (L e) (pi)))
  (define e-5 (raw-prod (L e) (num 5)))
  (define e-7 (raw-prod (L e) (num 7)))
  (define e-+ (raw-prod (L e) (op "+" (vector e-5 e-7))))
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
  (check-equal? (raw-prod (L e) #:proj [op 1] e-+) (vector e-5 e-7))
  (check-eq? (raw-prod (L d) #:proj [def 1] d-twelve) e-+)

  ;; ---------------
  ;; 'template' tests
  ;; ---------------

  ; unquoted
  (check-eqv? (template (L i) ,(+ 5 6)) 11)
  (check-eq? (template (L e) ,e-5) e-5)
  (check-exn #px"template: contract violation\n  expected: integer\\?"
             (λ () (template (L i) ,"not a number")))
  (check-exn #px"template: contract violation\n  expected: nonterminal 'e'"
             (λ () (template (L e) ,'|not an expr|)))

  ; datum
  (check-eqv? (template (L i) 5)  5)
  (check-eq? (template (L x) foo) 'foo)
  (check-exn #px"list datum not allowed"
             (λ () (convert-compile-time-error
                    (template (L i) (1 2)))))
  (check-exn #px"list datum not allowed"
             (λ () (convert-compile-time-error
                    (template (L i) ()))))
  (check-exn #px"template: contract violation\n  expected: symbol\\?"
             (λ () (template (L x) "not a symbol")))

  ; productions
  ; - basic forms
  (check-equal? (template (L e) (num . 5)) e-5)
  ; - form-list (no arguments)
  (check-equal? (template (L e) (pi)) e-pi)
  ; - form-list (constant arguments)
  (check-equal? (template (L d) (def twelve ,e-+)) d-twelve)
  ; - form-list (variadic arguments; no ellipsis in template)
  (check-equal? (template (L e) (op "+" (num . 5) ,e-7)) e-+)
  (check-equal? (template (L e) (let ([x ,e-5] [y ,e-7]) ,e-+))
                (raw-prod (L e) (let #(x y) (vector e-5 e-7) e-+)))
  ; - arity errors
  (check-exn #px"expected 2 arguments, got 1"
             (λ () (convert-compile-time-error
                    (template (L d) (def twelve)))))
  (check-exn #px"expected 1 argument, got 2"
             (λ () (convert-compile-time-error
                    (template (L e) (print (num . 1) (num . 2))))))
  (check-exn #px"expected at least 1 argument, got 0"
             (λ () (convert-compile-time-error
                    (template (L e) (op))))))
