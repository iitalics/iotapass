#lang racket/base
(provide
 free-id=)

(require
 (for-syntax racket/base)
 racket/match)

(define-match-expander free-id=
  (λ (stx)
    (syntax-case stx ()
      [(_ id) #'(== #'id free-identifier=?)])))
