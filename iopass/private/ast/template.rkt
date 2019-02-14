#lang racket/base
(provide
 (all-defined-out))

;; =======================================================================================

;; template ::=
;;   (unquoted expr-syntax)
;;   (datum datum-syntax)
(struct unquoted [stx] #:transparent)
(struct datum [stx] #:transparent)
