#lang racket/base
(provide
 raw-prod
 template)

(require
 (for-syntax
  racket/base
  (rename-in syntax/parse [attribute @])
  (prefix-in c: "../syntax/classes.rkt")
  "../syntax/bindings.rkt"
  "../syntax/metaterm.rkt"
  "../repr/template.rkt"
  "../repr/ids.rkt"))

(begin-for-syntax
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
    [(_ :c:language+metavar
        f:raw-function)
     #:with {~var nt (c:nonterminal-metavar-id (@ language))} #'metavar
     #:with {~var || (c:known-production-id (@ nt.spec))} #'f.head
     #:with [ct pr pj] (hash-ref (language-repr-ids-productions (@ repr-ids))
                                 (@ production))
     (case (@ f.fun)
       [(ctor) #'(ct f.arg ...)]
       [(pred) #'(pr f.arg ...)]
       [(proj) #'(pj f.arg ...)])]))

;; -----------
;; template
;; -----------

(define-syntax template
  (syntax-parser
    [(hd :c:language+metavar tmpl)
     (compile-template #'tmpl
                       #'hd
                       (@ repr-ids)
                       (parse-mt (@ spec)
                                 (@ language)
                                 #'tmpl))]))

;; =======================================================================================

(module+ test
  (require
   "define.rkt"
   rackunit
   racket/port)

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
    [d ::= (def x e)]
    [mat ::= (Matrix [i ...] ...)])

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
  (check-exn #px"template: contract violation\n  expected: integer\\?"
             (λ () (template (L i) ,"not a number")))
  (check-exn #px"template: contract violation\n  expected: <nonterminal:e>"
             (λ () (template (L e) ,'|not an expr|)))

  ; datum
  (check-eqv? (template (L i) 5)  5)
  (check-eq? (template (L x) foo) 'foo)
  (check-exn #px"template: contract violation\n  expected: symbol\\?"
             (λ () (template (L x) "not a symbol")))

  ; productions
  (check-equal? (template (L e) (num . 5)) e-5)

  ; form-list: no arguments
  (check-equal? (template (L e) (pi)) e-pi)
  ; form-list: constant arguments
  (check-equal? (template (L d) (def twelve ,e-+)) d-twelve)

  ; form-list: variadic arguments w/ no ellipsis
  (check-equal? (template (L e) (op "+" (num . 5) ,e-7)) e-+)
  (check-equal? (template (L e) (op "nop"))
                (raw-prod (L e) (op "nop" '())))
  (check-equal? (template (L e) (let () ,e-+))
                (raw-prod (L e) (let '() '() e-+)))
  (check-equal? (template (L e) (let ([x ,e-5] [y ,e-7]) ,e-+))
                (raw-prod (L e) (let '(x y) (list e-5 e-7) e-+)))

  ; form-list: ellipsis
  (let ([x* '(x y)] [i* '(5 7)])
    (check-equal? (template (L e) (let ([,x* (num . ,i*)] ...) ,e-+))
                  (raw-prod (L e) (let '(x y) (list e-5 e-7) e-+)))
    (check-exn #px"template: contract violation\n  expected: \\(listof symbol\\?\\)"
               (λ () (template (L e) (let ([,i* (num . ,i*)] ...) ,e-+))))

    (define C '(1 2 3))
    (define M '((10 100) (20 200) (30 300)))
    (check-equal? (template (L mat) (Matrix [,C ,M ...] ...))
                  (template (L mat) (Matrix [1 10 100]
                                            [2 20 200]
                                            [3 30 300])))
    (check-exn #px"all lists must have same size"
               (λ () (template (L e) (let ([,x* (num . ,C)] ...) ,e-+)))))


  ; form-list: order of evaluation
  (check-equal? (with-output-to-string
                  (λ ()
                    (define (trace x [msg x]) (printf "[~s]" msg) x)
                    (template (L e)
                              (let ([,(trace 'x)
                                     ,(trace e-5 'e-5)]
                                    [,(trace 'y)
                                     ,(trace e-7 'e-7)])
                                ,(trace e-+ 'e-+)))))
                "[x][e-5][y][e-7][e-+]"))
