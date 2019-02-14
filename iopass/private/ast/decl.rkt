#lang racket/base
(provide
 (all-defined-out))

(require
 racket/set
 racket/match
 racket/string
 threading)

;; =======================================================================================

;; spec ::=
;;   terminal-spec
;;   nonterminal-spec
(struct spec
  [orig-stx
   metavar-symbols]
  #:methods gen:custom-write
  [(define (write-proc s port mode)
     (fprintf port "#<~a:~a>"
              (cond
                [(nonterminal-spec? s) 'nonterminal-spec]
                [(terminal-spec? s) 'terminal-spec]
                [else 'spec])
              (spec-description s)))])

;; spec -> string
(define (spec-description s)
  (string-join (map symbol->string
                    (spec-metavar-symbols s))
               "/"))

;; [listof spec] -> (or [setof symbol] spec-set-error)
(define (spec-set-metavars ss)
  (let/ec die
    (for*/fold ([mvs (seteq)])
               ([s (in-list ss)]
                [x (in-list (spec-metavar-symbols s))])
      (when (set-member? mvs x)
        (die (spec-repeated-metavar-error (spec-orig-stx s) x)))
      (set-add mvs x))))

(struct spec-set-error [stx] #:transparent)
(struct spec-repeated-metavar-error spec-set-error [symbol] #:transparent)

(module+ test
  (require
   rackunit
   racket/match)

  ;; ------------
  ;; Test spec-description

  (check-equal? (spec-description (terminal-spec 0 '(x) #'a #'b)) "x")
  (check-equal? (spec-description (terminal-spec 0 '(x y) #'a #'b)) "x/y")

  ;; ------------
  ;; Test spec-set-metavars

  (check-match (spec-set-metavars (list))
               (? set-empty?))
  (check-match (spec-set-metavars (list (terminal-spec 0 '(x y) #'a #'b)))
               (== (seteq 'x 'y)))
  (check-match (spec-set-metavars (list (terminal-spec 0 '(x) #'a #'b)
                                        (terminal-spec 1 '(y z) #'c #'d)))
               (== (seteq 'x 'y 'z)))

  (check-match (spec-set-metavars (list (terminal-spec 0 '(y y) #'a #'b)))
               (spec-repeated-metavar-error 0 'y))
  (check-match (spec-set-metavars (list (terminal-spec 0 '(x y) #'a #'b)
                                        (terminal-spec 1 '(x) #'c #'d)))
               (spec-repeated-metavar-error 1 'x)))

;; -------------------
;; Terminals
;; -------------------

;; terminal-set ::=
;;   [listof terminal-spec]
(define (terminal-set? ts)
  (and (list? ts)
       (andmap terminal-spec? ts)))

;; terminal-spec ::=
;;   (terminal-spec stx [listof symbol] identifier identifier)
(struct terminal-spec spec
  [contract-id
   equal?-id])

;; -------------------
;; Nonterminals
;; -------------------

;; nonterminal-spec ::=
;;   (nonterminal-spec stx
;;                     [listof symbol]
;;                     [listof production-spec])
(struct nonterminal-spec spec
  [productions])

;; production-spec ::=
;;   (production stx symbol form)
(struct production
  [orig-stx
   head-symbol
   form])

;; nonterminal symbol        -> (or #f production)
;; nonterminal symbol [-> X] -> (or X  production)
(define (nonterminal-production nt head-sym
                                [fail-proc (λ () #f)])
  (or (findf (λ (pr)
               (eq? (production-head-symbol pr)
                    head-sym))
             (nonterminal-spec-productions nt))
      (fail-proc)))

(module+ test
  ;; -------
  ;; Test nonterminal-production
  ;; -------
  (define tm-xy (terminal-spec #'xy '(x y) #'symbol? #'eq?))
  (define tm-ij (terminal-spec #'ij '(i j) #'integer? #'=))
  (define nt-e (nonterminal-spec #'e
                                 '(e)
                                 (list
                                  (production #'(num . i)
                                              'num
                                              (metavar #'i 'i)))))
  (check-false (nonterminal-production nt-e 'blah))
  (check-eq? (nonterminal-production nt-e 'blah (λ () 'nope)) 'nope)
  (check-eq? (production-head-symbol (nonterminal-production nt-e 'num)) 'num))

;; form ::=
;;   (metavar stx symbol)
;;   (form-list stx
;;              [listof form]
;;              (or (ellipsis form) #f)
;;              [listof form])
(struct form [orig-stx] #:transparent)
(struct metavar form [symbol] #:transparent)
(struct form-list form [before repeat after] #:transparent)
(struct ellipsis [repeated-form] #:transparent)

;; (or #f ellipsis) -> [listof form]
;; Helper for traversing optional ellipsis forms.
(define (ellipsis->list e)
  (match e
    [(ellipsis f) (list f)]
    [#f '()]))

;; form [setof symbol] -> (or #f metavar)
;; If any metavariable names in 'fm' are not in 'mvs', returns that metavar.
(define (form-unbound-metavar fm mvs)
  (match fm
    [(metavar _ x)
     (cond [(set-member? mvs x) #f]
           [else fm])]
    [(form-list _ as maybe-ellipsis bs)
     (define (form-unbound fm*)
       (form-unbound-metavar fm* mvs))
     (or (ormap form-unbound as)
         (ormap form-unbound (ellipsis->list maybe-ellipsis))
         (ormap form-unbound bs))]))

;; form [setof symbol] -> (or #f metavar)
;; If any metavariable names in the body of the nonterminal are not in 'mvs', returns that
;; metavar.
(define (nonterminal-unbound-metavar nt mvs)
  (for/or ([p (in-list (nonterminal-spec-productions nt))])
    (form-unbound-metavar (production-form p)
                          mvs)))

(module+ test
  ;; -------
  ;; Test {form,nonterminal}-unbound-metavar
  ;; -------
  (define mX (metavar #'x 'x))
  (define mY (metavar #'y 'y))
  (check-false (form-unbound-metavar (form-list #'_ (list mX) #f '())
                                     (seteq 'x)))
  (check-equal? (form-unbound-metavar (form-list #'_ (list mX) #f '())
                                      (seteq))
                mX)
  (check-false (form-unbound-metavar (form-list #'_ '() (ellipsis mY) '())
                                     (seteq 'y)))
  (check-equal? (form-unbound-metavar (form-list #'_ '() (ellipsis mY) (list mX))
                                      (seteq 'z))
                mY)
  (check-equal? (form-unbound-metavar (form-list #'_ '() (ellipsis mY) (list mX))
                                      (seteq 'y))
                mX))

;; -------------------
;; Languages
;; -------------------

;; language ::=
;;   (language stx
;;             symbol
;;             [listof terminal-spec]
;;             [listof nonterminal-spec])
;;             [hash symbol => spec]
(struct language
  [orig-stx
   name
   terminals
   nonterminals
   metavar-spec-mapping])

;; stx symbol [listof terminal-spec] [listof nonterminal-spec]
;;  -> language
(define (make-language stx name tms nts)

  (define (extend-mapping m specs)
    (for*/fold ([m m])
               ([s (in-list specs)]
                [x (in-list (spec-metavar-symbols s))])
      (hash-set m x s)))

  (language stx
            name
            tms
            nts
            (~>> (hasheq)
                 (extend-mapping _ tms)
                 (extend-mapping _ nts))))

;; language symbol -> (or #f spec)
(define (language-lookup-metavar lang sym)
  (hash-ref (language-metavar-spec-mapping lang)
            sym
            #f))

(module+ test
  ;; -----
  ;; Test make-language, language-lookup-metavar
  ;; -----
  (define L (make-language #'foo 'L (list tm-xy tm-ij) (list nt-e)))
  (check-eq? (language-lookup-metavar L 'x) tm-xy)
  (check-eq? (language-lookup-metavar L 'y) tm-xy)
  (check-eq? (language-lookup-metavar L 'i) tm-ij)
  (check-eq? (language-lookup-metavar L 'j) tm-ij)
  (check-eq? (language-lookup-metavar L 'e) nt-e))

;; language-delta ::=
;;   (language-delta stx
;;                   language
;;                   [listof nonterminal-spec]
;;                   [listof nonterminal-spec])
(struct language-delta
  [orig-stx
   extending
   add-nonterminals
   del-nonterminals])
