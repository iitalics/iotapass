#lang racket/base
(provide
 (struct-out field-repr)
 form-field-count
 form-field-repr)

(require
 "../types.rkt"
 racket/match
 threading)

;; --------------------
;; representation of forms as struct fields
;; --------------------

;; field-repr ::= (field-repr symbol nat)
(struct field-repr
  [metavar-symbol
   ellipsis-depth]
  #:transparent)

;; form -> exact-integer
;; Returns the number of fields in the representation of the given form.
(define (form-field-count fm)
  ;; [listof form] -> exact-integer
  (define (count* fms)
    (for/sum ([fm (in-list fms)])
      (form-field-count fm)))
  (match fm
    [(metavar _ _) 1]
    [(form-list _ before repeat after)
     (+ (count* before)
        (count* (ellipsis->list repeat))
        (count* after))]))

;; form -> [listof field-repr]
;; Returns list of fields to represent the given form.
(define (form-field-repr fm)
  (reverse
   (let repr/depth ([fm fm]
                    [lst '()]
                    [ed 0])

     (define (repr fm lst)
       (repr/depth fm lst ed))
     (define (repr+1 fm lst)
       (repr/depth fm lst (add1 ed)))

     (match fm
       [(metavar _ x)
        (cons (field-repr x ed)
              lst)]

       [(form-list _ before repeat after)
        (~> lst
            (foldl repr   _ before)
            (foldl repr+1 _ (ellipsis->list repeat))
            (foldl repr   _ after))]))))

;; --------------------
;; generate-structs macro
;; --------------------

;; put these helper functions in a submodule so they can be tested easily without
;; compromising visibility.
(module* generate-structs-helpers #f
  (provide (all-defined-out))
  (require
   "ids.rkt"
   "../syntax/bindings.rkt"
   (only-in racket/list append-map)
   (only-in racket/syntax format-symbol)
   syntax/parse)

  ;; language language-repr-ids
  ;; -> [stxlistof let*-values-binding-syntax]
  (define (production-bindings lang lang-repr-ids)
    (define lang-name (language-name lang))
    (define nts (language-nonterminals lang))
    (define pr=>ids (language-repr-ids-productions lang-repr-ids))

    (define/syntax-parse [[ctor-id pred-id proj-id] ...]
      (for*/list ([nt (in-list nts)]
                  [pr (in-list (nonterminal-spec-productions nt))])
        (hash-ref pr=>ids pr)))

    (define/syntax-parse [[name-sym field-count] ...]
      (for*/list ([nt (in-list nts)]
                  [pr (in-list (nonterminal-spec-productions nt))])
        (match-define (production _ head-sym fm) pr)
        (list (format-symbol "~a.~a" lang-name head-sym)
              (form-field-count fm))))

    #'([(_1 ctor-id pred-id proj-id _2)
        (make-struct-type 'name-sym
                          #f
                          'field-count
                          0)]
       ...))

  ;; language language-repr-ids
  ;; -> [stxlistof let-binding-syntax]
  (define (nonterminal-bindings lang lang-repr-ids)
    (define/syntax-parse [pred-id ...]
      (for/list ([nt (in-list (language-nonterminals lang))])
        (hash-ref (language-repr-ids-predicates lang-repr-ids)
                  nt)))

    #'([pred-id (λ (x) 'WIP #f)]
       ...)))

;; the generate-structs macro is in a submodule so that it can access above functions at
;; phase 1
(module* generate-structs-macro racket/base
  (provide
   generate-structs)

  (require
   (for-syntax
    racket/base
    (rename-in syntax/parse [attribute @])
    (submod ".." generate-structs-helpers)
    "../syntax/bindings.rkt"
    "ids.rkt"))

  ;; generates a big (values ...) containing struct functions for each id for each
  ;; nonterminal, then each production, in order.
  ;; e.g. if (get-language-repr-ids #'L0) =
  ;;         (language-repr-ids #{ <e> -> e?
  ;;                               <d> -> d? }
  ;;                            #{ <p> -> (p.c p.p p.a)
  ;;                               <q> -> (q.c q.p q.a) })
  ;; then
  ;;   (generate-struct-repr-values L0)
  ;;     ==>
  ;;   (values e? d? p.c p.p p.a q.c q.p q.a)
  ;; in a context where all those id's are bound to the result of (make-struct-type ...)
  ;; calls.

  (define-syntax generate-structs
    (syntax-parser
      [(_ b:language-definition-binding)
       #:with [pr-bind ...] (production-bindings (@ b.language)
                                                 (@ b.repr-ids))
       #:with [nt-bind ...] (nonterminal-bindings (@ b.language)
                                                  (@ b.repr-ids))
       #:with [id ...] (in-language-repr-ids-all-ids (@ b.repr-ids))
       #'(let*-values (pr-bind ...)
           (let (nt-bind ...)
             (values id ...)))])))

;; =======================================================================================

(module+ test
  (require
   rackunit
   racket/match
   (submod ".." generate-structs-helpers)
   "ids.rkt"
   "../syntax/util.rkt")

  ;; (x y z ... [w])
  (let ([fm (form-list 0
                       (list (metavar 1 'x)
                             (metavar 2 'y))
                       (ellipsis (metavar 3 'z))
                       (list (form-list 4
                                        (list (metavar 5 'w))
                                        #f
                                        '())))])
    (check-equal? (form-field-count fm) 4)
    (check-equal? (form-field-repr fm)
                  (list (field-repr 'x 0)
                        (field-repr 'y 0)
                        (field-repr 'z 1)
                        (field-repr 'w 0))))

  ;; ([x ...] ...)
  (let ([fm (form-list 0
                       '()
                       (ellipsis (form-list 1
                                            '()
                                            (ellipsis (metavar 2 'x))
                                            '()))
                       '())])
    (check-equal? (form-field-count fm) 1)
    (check-equal? (form-field-repr fm)
                  (list (field-repr 'x 2))))

  ;; (define-language L
  ;;   [x y ::= (A x) (B x y)]
  ;;   [z   ::= (C)])
  (let* ([fm-x-y (form-list #'(x y)
                            (list (metavar #'x 'x)
                                  (metavar #'y 'y))
                            #f
                            '())]
         [pr-A (production #'(A x) 'A (metavar #'x 'x))]
         [pr-B (production #'(B x y) 'B fm-x-y)]
         [pr-C (production #'(C) 'C (form-list #'() '() #f '()))]
         [nt-xy (nonterminal-spec #'xy '(x y) (list pr-A pr-B))]
         [nt-z (nonterminal-spec #'z '(z) (list pr-C))]
         [L (make-language #'L 'L '() (list nt-xy nt-z))]
         [L-ids (make-language-repr-ids (list nt-xy nt-z)
                                        (hasheq nt-xy #'xy?
                                                nt-z  #'z?)
                                        (hasheq nt-xy (list #'[A.c A? A.ref]
                                                            #'[B.c B? B.ref])
                                                nt-z  (list #'[C.c C? C.ref])))])

    (check-match
     (syntax->datum (production-bindings L L-ids))
     '([(_1 A.c A? A.ref _2) (make-struct-type 'L.A #f '1 0)]
       [(_1 B.c B? B.ref _2) (make-struct-type 'L.B #f '2 0)]
       [(_1 C.c C? C.ref _2) (make-struct-type 'L.C #f '0 0)]))

    (check-match
     (syntax->datum (nonterminal-bindings L L-ids))
     ; TODO: fill in implementation
     `([xy? (λ (,_) ,_ ...)]
       [z?  (λ (,_) ,_ ...)]))))
