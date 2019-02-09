#lang racket/base
(provide
 (all-defined-out))

(require
 racket/set
 racket/match)

;; =======================================================================================

;; spec ::=
;;   terminal-spec
;;   nonterminal-spec
(struct spec
  [orig-stx
   metavar-symbols])

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

;; form ::=
;;   (metavar stx symbol)
;;   (form-list stx
;;              [listof form]
;;              (or (ellipsis form) #f)
;;              [listof form])
(struct form [orig-stx] #:transparent)
(struct metavar form [symbol] #:transparent)
(struct form-list form [before middle after] #:transparent)
(struct ellipsis [repeated-form] #:transparent)

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
         (and maybe-ellipsis
              (form-unbound (ellipsis-repeated-form maybe-ellipsis)))
         (ormap form-unbound bs))]))

;; form [setof symbol] -> (or #f metavar)
;; If any metavariable names in the body of the nonterminal are not in 'mvs', returns that
;; metavar.
(define (nonterminal-unbound-metavar nt mvs)
  (for/or ([p (in-list (nonterminal-spec-productions nt))])
    (form-unbound-metavar (production-form p)
                          mvs)))

(module+ test

  ;; ------------
  ;; Test {form,nonterminal}-unbound-metavar

  (define mX (metavar #'x 'x))
  (define mY (metavar #'y 'y))
  (check-equal? (form-unbound-metavar (form-list #'_ (list mX) #f '())
                                      (seteq 'x))
                #f)
  (check-equal? (form-unbound-metavar (form-list #'_ (list mX) #f '())
                                      (seteq))
                mX)
  (check-equal? (form-unbound-metavar (form-list #'_ '() (ellipsis mY) '())
                                      (seteq 'y))
                #f)
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
;;             terminal-set
;;             [listof nonterminal-spec])
(struct language
  [orig-stx
   name
   terminals
   nonterminals])

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
