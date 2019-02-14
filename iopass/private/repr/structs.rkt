#lang racket/base
(provide
 (struct-out field-repr)
 form-field-count
 form-field-repr)

(require
 "../ast/decl.rkt"
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
   (for-template racket/base)
   "ids.rkt"
   "../syntax/bindings.rkt"
   (only-in racket/list append-map)
   (only-in racket/syntax format-symbol)
   syntax/parse)

  ;; language [listof [listof #'[<ctor-id> <pred-id> <proj-id>]]
  ;; -> [listof stx]
  (define (production-structure-defs lang pr-id-tripless)
    (define lang-name (language-name lang))

    (for/list ([nt (in-list (language-nonterminals lang))]
               [pr-id-triples (in-list pr-id-tripless)])
      (define/syntax-parse [(ctor-id pred-id proj-id) ...] pr-id-triples)
      (define/syntax-parse [(name-sym field-count) ...]
        (for/list ([pr (in-list (nonterminal-spec-productions nt))])
          (match-define (production _ head-sym fm) pr)
          (define nt-name (spec-description nt))
          (list (format-symbol "~a.~a.~a" lang-name nt-name head-sym)
                (form-field-count fm))))

      #'(define-values [ctor-id ... pred-id ... proj-id ...]
          (let*-values ([(_1 ctor-id pred-id proj-id _2)
                         (make-struct-type 'name-sym     ; name
                                           #f            ; super
                                           'field-count  ; # fields
                                           0 #f '()      ; # auto, auto-v, props
                                           #f            ; inspector (#f = transparent)
                                           #f '() #f     ; proc-spec, immutables, guard
                                           'name-sym)]   ; ctor name
                        ...)
            (values ctor-id ...
                    pred-id ...
                    proj-id ...)))))

  ;; language [listof id] [listof [listof id]] -> [listof stx]
  (define (nonterminal-predicate-defs nt-predicate-ids
                                      pr-predicate-idss)
    (for/list ([nt-pred-id (in-list nt-predicate-ids)]
               [pr-pred-ids (in-list pr-predicate-idss)])
      (define/syntax-parse nt? nt-pred-id)
      (define/syntax-parse [pr? ...] pr-pred-ids)
      #'(define (nt? x)
          (or (pr? x)
              ...)))))


;; the generate-structs macro is in a submodule so that it can access above functions at
;; phase 1
(module* generate-structs-macro racket/base
  (provide
   with-generate-structs)

  (require
   (for-syntax
    racket/base
    (rename-in syntax/parse [attribute @])
    (submod ".." generate-structs-helpers)
    "../syntax/bindings.rkt"
    "ids.rkt"))

  (define-syntax with-generate-structs
    (syntax-parser
      [(_ [#:lang :language-definition-binding
           #:nt-ids [nt?:id ...]
           #:pr-ids [({~and pr-triple [pr-c:id pr-p:id pr-j:id]}
                      ...)
                     ...]]
          body ...)
       #`(let-values ()
           #,@(production-structure-defs (@ language)
                                         (@ pr-triple))
           #,@(nonterminal-predicate-defs (@ nt?)
                                          (@ pr-c))
           body ...)])))

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
         [L (make-language #'L 'L '() (list nt-xy nt-z))])

    (check-match
     (map syntax->datum
          (production-structure-defs
           L
           (list (list #'[A.c A? A.ref]
                       #'[B.c B? B.ref])
                 (list #'[C.c C? C.ref]))))
     `{(define-values [A.c B.c A? B? A.ref B.ref]
         (let*-values ([(_1 A.c A? A.ref _2) (make-struct-type 'L.x/y.A #f '1 ,_ ...)]
                       [(_1 B.c B? B.ref _2) (make-struct-type 'L.x/y.B #f '2 ,_ ...)])
           (values A.c B.c A? B? A.ref B.ref)))
       (define-values [C.c C? C.ref]
         (let*-values ([(_1 C.c C? C.ref _2) (make-struct-type 'L.z.C #f '0 ,_ ...)])
           (values C.c C? C.ref)))})

    (check-match
     (map syntax->datum
          (nonterminal-predicate-defs (list #'xy? #'z?)
                                      (list (list #'A? #'B?)
                                            (list #'C?))))
     `((define (xy? x) (or (A? x) (B? x)))
       (define (z?  x) (or (C? x)))))))
