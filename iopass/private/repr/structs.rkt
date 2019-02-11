#lang racket/base
(provide
 (struct-out language-repr-ids)
 (struct-out production-repr-ids))

(require
 "../types.rkt"
 racket/match
 racket/match)

;; =======================================================================================

;; language-repr-ids ::=
;;   (language-repr-ids
;;    [hasheq nonterminal-spec => identifier]
;;    [hasheq production => production-repr-ids])
;; production-repr-ids ::=
;;   (production-repr-ids
(struct language-repr-ids
  [nonterminal-predicates
   productions])

(struct production-repr-ids
  [constructor-id
   predicate-id
   accessor-id])

