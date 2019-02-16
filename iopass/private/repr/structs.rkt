#lang racket/base
(provide
 form-field-count)

(require
 "../ast/decl.rkt"
 racket/match
 threading)

;; --------------------
;; number of fields in the struct representation of a production
;; --------------------

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
   generate-structs)

  (require
   (for-syntax
    racket/base
    (rename-in syntax/parse [attribute @])
    (submod ".." generate-structs-helpers)
    "../syntax/bindings.rkt"))

  (define-syntax generate-structs
    (syntax-parser
      [(_ #:lang :language-definition-binding
          #:nt-ids [nt?:id ...]
          #:pr-ids [({~and pr-triple [pr-c:id pr-p:id pr-j:id]}
                     ...)
                    ...])
       #`(begin
           #,@(production-structure-defs (@ language)
                                         (@ pr-triple))
           #,@(nonterminal-predicate-defs (@ nt?)
                                          (@ pr-p)))])))

;; =======================================================================================

(module+ test
  (require
   rackunit
   racket/match
   (submod ".." generate-structs-helpers)
   "ids.rkt"
   "../syntax/util.rkt")

  ;; (x y z ... [w])
  (check-equal? (form-field-count
                 (form-list 0
                            (list (metavar 1 'x)
                                  (metavar 2 'y))
                            (ellipsis (metavar 3 'z))
                            (list (form-list 4
                                             (list (metavar 5 'w))
                                             #f
                                             '()))))
                4)

  ;; ([x ...] ...)
  (check-equal? (form-field-count
                 (form-list 0
                            '()
                            (ellipsis (form-list 1
                                                 '()
                                                 (ellipsis (metavar 2 'x))
                                                 '()))
                            '()))
                1)


  (require "../util/example-language-decls.rkt")

  (check-match
   (map syntax->datum
        (production-structure-defs
         L
         (list (list #'[A.c A? A.ref]
                     #'[B.c B? B.ref])
               (list #'[C.c C? C.ref]))))
   `{(define-values [A.c B.c A? B? A.ref B.ref]
       (let*-values ([(_1 A.c A? A.ref _2) (make-struct-type 'L.a/b.A #f '1 ,_ ...)]
                     [(_1 B.c B? B.ref _2) (make-struct-type 'L.a/b.B #f '2 ,_ ...)])
         (values A.c B.c A? B? A.ref B.ref)))
     (define-values [C.c C? C.ref]
       (let*-values ([(_1 C.c C? C.ref _2) (make-struct-type 'L.c.C #f '0 ,_ ...)])
         (values C.c C? C.ref)))})

  (check-match
   (map syntax->datum
        (nonterminal-predicate-defs (list #'ab? #'c?)
                                    (list (list #'A? #'B?)
                                          (list #'C?))))
   `((define (ab? x) (or (A? x) (B? x)))
     (define (c?  x) (or (C? x))))))
