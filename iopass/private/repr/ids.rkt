#lang racket/base
(provide
 (struct-out language-repr-ids)
 (struct-out production-repr-ids)
 make-language-repr-ids)

(require
 "../types.rkt"
 "../syntax/bindings.rkt"
 syntax/parse
 racket/match)

;; =======================================================================================

;; language-repr-ids ::=
;;   (language-repr-ids
;;    [hasheq nonterminal-spec => identifier]
;;    [hasheq production => production-repr-ids])
;; production-repr-ids ::=
;;   (production-repr-ids identifier identifier identifier)
(struct language-repr-ids
  [nonterminal-predicates
   productions])
(struct production-repr-ids
  [constructor-id
   predicate-id
   accessor-id])

;; [listof nonterminal-spec]
;; [hasheq nonterminal-spec => identifier]
;; [hasheq nonterminal-spec => [listof #'[<id> <id> <id>]]]
;; -> language-repr-ids
(define (make-language-repr-ids nts
                                nt=>pred-id
                                nt=>repr-ids)
  (define pr=>ids
    (for*/hasheq ([nt (in-list nts)]
                  [(pr pr-ids) (in-parallel
                                (in-list (nonterminal-spec-productions nt))
                                (in-list (hash-ref nt=>repr-ids nt)))])
      (syntax-parse pr-ids
        [[ctor pred proj]
         (values pr
                 (production-repr-ids #'ctor #'pred #'proj))])))

  (language-repr-ids nt=>pred-id pr=>ids))
