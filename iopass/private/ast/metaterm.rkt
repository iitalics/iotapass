#lang racket/base
(provide
 (all-defined-out))

;; =======================================================================================

;; A 'metaterm' generalizes the syntax of a pattern and a template. Both positions contain
;; a 'metaterm', representing an instantiation of a 'form' (in decl.rkt).

;; The difference between a pattern and a template is basically just how they are
;; compiled. In particular, (unquoted ..) are interpreted very differently in patterns and
;; templates.

;; metaterm ::=
;;   (unquoted syntax spec)
;;   (datum syntax terminal-spec)
;;   (prod production [listof metaterm])
;;   [listof metaterm]
(struct unquoted [stx spec] #:transparent)
(struct datum [stx spec] #:transparent)
(struct prod [prod args] #:transparent)
