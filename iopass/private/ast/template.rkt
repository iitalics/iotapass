#lang racket/base
(provide
 (all-defined-out))

;; =======================================================================================

;; template ::=
;;   (unquoted expr-syntax)
;;   (datum datum-syntax)
;;   (prod production [listof template])
(struct unquoted [stx] #:transparent)
(struct datum [stx] #:transparent)
(struct prod [prod temps] #:transparent)
