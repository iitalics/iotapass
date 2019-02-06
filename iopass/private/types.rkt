#lang racket/base
(provide
 (all-defined-out))

;; =======================================================================================

;; spec ::=
;;   terminal-spec
;;   nonterminal-spec
(struct spec
  [orig-stx
   metavar-symbols])

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
