#lang racket/base
(provide
 (all-defined-out))

(require
 (for-template
  (only-in racket/base quote)))

;; =======================================================================================

;; A 'metaterm' generalizes the syntax of a pattern and a template. Both positions contain
;; a 'metaterm', representing an instantiation of a 'form' (in decl.rkt).

;; The difference between a pattern and a template is basically just how they are
;; compiled. In particular, (unquoted ..) are interpreted very differently in patterns and
;; templates.

;; metaterm ::=
;;   (unquoted syntax)
;;   (prod production [listof metaterm])
;;   [listof metaterm]
(struct unquoted [stx] #:transparent)
(struct prod [prod args] #:transparent)

(define (datum stx)
  (unquoted #`(quote #,stx)))
