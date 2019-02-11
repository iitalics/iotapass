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
   productions]
  #:transparent)
(struct production-repr-ids
  [constructor-id
   predicate-id
   accessor-id]
  #:transparent)

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

;; =======================================================================================

(module+ test
  (require
   rackunit
   racket/match
   "../syntax/util.rkt")

  (define pr-a (production #'a 'a '...))
  (define pr-b (production #'b 'b '...))
  (define pr-c (production #'c 'c '...))
  (define nt-x (nonterminal-spec #'x '(x) (list pr-a pr-b)))
  (define nt-y (nonterminal-spec #'y '(y) (list pr-c)))

  (define lr-ids
    (make-language-repr-ids
     (list nt-x nt-y)
     (hasheq nt-x #'x?
             nt-y #'y?)
     (hasheq nt-x (list #'[a.ctor a.pred a.acc]
                        #'[b.ctor b.pred b.acc])
             nt-y (list #'[c.ctor c.pred c.acc]))))

  (check-match (language-repr-ids-nonterminal-predicates lr-ids)
               (hash-table [nt-x (free-id= x?)]
                           [nt-y (free-id= y?)]))

  (check-match (language-repr-ids-productions lr-ids)
               (hash-table [pr-a (production-repr-ids
                                  (free-id= a.ctor)
                                  (free-id= a.pred)
                                  (free-id= a.acc))]
                           [pr-b (production-repr-ids
                                  (free-id= b.ctor)
                                  (free-id= b.pred)
                                  (free-id= b.acc))]
                           [pr-c (production-repr-ids
                                  (free-id= c.ctor)
                                  (free-id= c.pred)
                                  (free-id= c.acc))])))
