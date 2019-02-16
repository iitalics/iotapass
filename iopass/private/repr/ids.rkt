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
   "../syntax/util.rkt"
   "../util/example-language-decls.rkt")

  (define lr-ids
    (make-language-repr-ids
     (list nt-ab nt-c)
     (hasheq nt-ab #'ab?
             nt-c  #'c?)
     (hasheq nt-ab (list #'[A.c A? A.ref]
                         #'[B.c B? B.ref])
             nt-c  (list #'[C.c C? C.ref]))))

  (check-match (language-repr-ids-predicates lr-ids)
               (hash-table [(== nt-ab eq?) (free-id= ab?)]
                           [(== nt-c  eq?) (free-id= c?)]))

  (check-match (language-repr-ids-productions lr-ids)
               (hash-table
                [(== pr-A eq?) (list (free-id= A.c)
                                     (free-id= A?)
                                     (free-id= A.ref))]
                [(== pr-B eq?) (list (free-id= B.c)
                                     (free-id= B?)
                                     (free-id= B.ref))]
                [(== pr-C eq?) (list (free-id= C.c)
                                     (free-id= C?)
                                     (free-id= C.ref))])))
