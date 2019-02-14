#lang racket/base
(provide
 (struct-out language-repr-ids)
 make-language-repr-ids)

(require
 "../ast/decl.rkt"
 "../syntax/bindings.rkt"
 syntax/parse
 racket/match
 (only-in racket/stream for/stream for*/stream)
 (only-in racket/sequence sequence->list sequence-append))

;; =======================================================================================

;; language-repr-ids ::=
;;   (language-repr-ids
;;    [listof nonterminal-spec]
;;    [hasheq nonterminal-spec => identifier]
;;    [hasheq production => [list ctor-id pred-id proj-id]])
;; ctor : [field-type ... -> production-type]
;; pred : [any -> bool]
;; proj : [production-type nat -> field-type]
(struct language-repr-ids
  [nonterminals
   predicates
   productions]
  #:transparent)

;; [listof nonterminal-spec]
;; [hasheq nonterminal-spec => identifier]
;; [hasheq nonterminal-spec => [list #'[<id> <id> <id>]]]
;; -> language-repr-ids
(define (make-language-repr-ids nts
                                nt=>pred-id
                                nt=>repr-ids)
  (define pr=>ids
    (for*/hasheq ([nt (in-list nts)]
                  [(pr pr-ids) (in-parallel
                                (in-list (nonterminal-spec-productions nt))
                                (in-list (hash-ref nt=>repr-ids nt)))])
      (values pr (syntax->list pr-ids))))

  (language-repr-ids nts nt=>pred-id pr=>ids))

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

  (check-match (language-repr-ids-predicates lr-ids)
               (hash-table [(== nt-x eq?) (free-id= x?)]
                           [(== nt-y eq?) (free-id= y?)]))

  (check-match (language-repr-ids-productions lr-ids)
               (hash-table
                [(== pr-a eq?) (list (free-id= a.ctor)
                                     (free-id= a.pred)
                                     (free-id= a.acc))]
                [(== pr-b eq?) (list (free-id= b.ctor)
                                     (free-id= b.pred)
                                     (free-id= b.acc))]
                [(== pr-c eq?) (list (free-id= c.ctor)
                                     (free-id= c.pred)
                                     (free-id= c.acc))])))
