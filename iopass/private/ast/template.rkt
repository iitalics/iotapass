#lang racket/base
(provide
 (all-defined-out))

;; =======================================================================================

;; template ::=
;;   (unquoted expr-syntax)
;;   (datum datum-syntax)
;;   (ellipsis template)
(struct unquoted [stx])
(struct datum [stx])
(struct ellipsis [template])
