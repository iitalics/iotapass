#lang racket/base
(provide
 (all-defined-out))

(require
 racket/set
 racket/format)

;; =======================================================================================

;; spec ::=
;;   terminal-spec
;;   nonterminal-spec
(struct spec
  [orig-stx
   metavar-symbols])

;; [listof spec] -> (or [listof symbol] spec-set-error)
(define (spec-set-metavars ss)
  (let/ec die
    (set->list
     (for*/fold ([mvs (seteq)])
                ([s (in-list ss)]
                 [x (in-list (spec-metavar-symbols s))])
       (when (set-member? mvs x)
         (die (repeated-metavar-error (spec-orig-stx s) x)))
       (set-add mvs x)))))

(struct spec-set-error [stx msg] #:transparent)
(define (repeated-metavar-error stx mv)
  (spec-set-error stx (~a "metavar " mv " defined multiple times")))

(module+ test
  (require rackunit)

  ;; ------------
  ;; Test spec-set-metavars

  (check-match (spec-set-metavars (list))
               '())
  (check-match (spec-set-metavars (list (terminal-spec 0 '(x y) #'a #'b)))
               (list-no-order 'x 'y))
  (check-match (spec-set-metavars (list (terminal-spec 0 '(x) #'a #'b)
                                        (terminal-spec 1 '(y z) #'c #'d)))
               (list-no-order 'x 'y 'z))

  (check-match (spec-set-metavars (list (terminal-spec 0 '(y y) #'a #'b)))
               (spec-set-error 0 "metavar y defined multiple times"))
  (check-match (spec-set-metavars (list (terminal-spec 0 '(x y) #'a #'b)
                                        (terminal-spec 1 '(x) #'c #'d)))
               (spec-set-error 1 "metavar x defined multiple times")))

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
(struct metavar [orig-stx symbol])
(struct form-list [orig-stx before middle after])
(struct ellipsis [repeated-form])

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
