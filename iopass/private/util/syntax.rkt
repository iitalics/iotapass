#lang racket/base
(provide
 stx:
 free-id=)

(require
 (for-syntax racket/base)
 syntax/stx
 racket/match)

(define-match-expander stx:
  (syntax-rules ()
    [(_ quasi-pat)
     (app syntax->datum `quasi-pat)]))

(define-match-expander free-id=
  (λ (stx)
    (syntax-case stx ()
      [(_ id) #'(== #'id free-identifier=?)])))
