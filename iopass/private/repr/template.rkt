#lang racket/base
(provide
 compile-template)

(require
 (prefix-in mt: "../ast/metaterm.rkt")
 "../ast/decl.rkt"
 "../repr/ids.rkt"
 (for-template racket/base)
 (only-in racket/syntax generate-temporary)
 racket/list
 racket/match
 syntax/parse)

;; =======================================================================================

;; ----------
;; compile-template
;; ----------

;; syntax syntax language-repr-ids metaterm -> [listof syntax]
; Compile a metaterm into an expression producing a language term; for use
; in (template ..) or related macros.
(define (compile-template src-stx macro-head repr-ids init-mt)

  ;; metaterm -> [listof binding] [listof syntax]
  ; Perform "ANF" conversion. The list of bindings may contain side effects, while the
  ; list of syntax will be the pure result values.
  (define (mt->bindings+exprs mt)
    (match mt
      ; base cases: unquoted or datum
      [(or (mt:unquoted stx spec)
           (mt:datum stx spec))
       (define id (generate-temporary stx))
       (define expr (make-spec-protect repr-ids
                                       src-stx
                                       macro-head
                                       spec
                                       (if (mt:datum? mt)
                                         (quasisyntax/loc src-stx (quote #,stx))
                                         stx)))
       (values (list (b id expr))
               (list id))]

      ; compound: production
      [(mt:prod pr body)
       (define/syntax-parse [ctor _ _]
         (hash-ref (language-repr-ids-productions repr-ids) pr))
       (define-values [bindings args]
         (mt->bindings+exprs body))
       (values bindings
               (list (quasisyntax/loc src-stx
                       (ctor #,@args))))]

      ; compound: multiple results
      [(mt:multiple mts)
       (let mts->bindings+exprs ([mts mts])
         (if (null? mts)
           (values '() '())
           (let-values ([(bs1 es1) (mt->bindings+exprs (car mts))]
                        [(bs2 es2) (mts->bindings+exprs (cdr mts))])
             (values (append bs1 bs2)
                     (append es1 es2)))))]

      ; compound: combine results
      [(mt:build n-cols row-mts)
       (error 'compile-template "build UNIMPLEMENTED")
       #;
       (define/syntax-parse [(val ...) ...]
         (map/transposed n-cols compile-mt row-mts))
       #;
       (syntax->list
        (quasisyntax/loc src-stx
          {(vector-immutable val ...)
           ...}))]))

  ;; -----

  (when (or (mt:multiple? init-mt)
            (mt:build? init-mt))
    (error 'compile-template
           (format "metaterm may have multiple results: ~s" init-mt)))

  (let-values ([(bindings exprs) (mt->bindings+exprs init-mt)])
    (make-let* bindings
               (car exprs))))

;; ----------
;; helpers for 'compile-template'
;; ----------

;; binding ::= (b identifier syntax)
(struct b [name rhs] #:transparent)

(define (binding->syntax binding)
  #`[#,(b-name binding)
     #,(b-rhs binding)])

(define (make-let* bindings body)
  #`(let* (#,@(map binding->syntax bindings))
      #,body))

;; ----------------
;; helpers for protecting expressions with contracts
;; ----------------

(module protect-value-function racket/base
  (provide protect-value)
  ;; symbol [any -> bool | X] string any -> X
  (define (protect-value macro-head
                         predicate
                         expect-string
                         value)
    (if (predicate value)
      value
      (raise-argument-error macro-head expect-string value))))

(require
 (for-template
  racket/base
  'protect-value-function))

;; language-repr-ids syntax syntax spec syntax -> syntax
(define (make-spec-protect repr-ids src-stx macro-head s value-stx)
  (quasisyntax/loc src-stx
    (protect-value '#,macro-head
                   #,(spec-predicate s repr-ids)
                   '#,(spec-expectation-string s)
                   #,value-stx)))

;; spec language-repr-ids -> identifier
(define (spec-predicate s repr-ids)
  (match s
    [(? nonterminal-spec? nt)
     (hash-ref (language-repr-ids-predicates repr-ids) nt)]
    [(? terminal-spec? tm)
     (terminal-spec-contract-id tm)]))

;; spec -> string
(define (spec-expectation-string s)
  (match s
    [(? nonterminal-spec? nt)
     (format "nonterminal '~a'" (spec-description nt))]
    [(? terminal-spec? tm)
     (symbol->string (syntax-e (terminal-spec-contract-id tm)))]))

;; ----------------
;; utils
;; ----------------

;; (n : nat) [X -> (list Y ...n)] [listof X] -> (list [listof Y] ...n)
(define (map/transposed n f l)
  (if (null? l)
    (make-list n '())
    (map cons
         (f (car l))
         (map/transposed n f (cdr l)))))

(module+ test
  (require
   rackunit)

  (check-equal? (map/transposed 0 (λ (i) '()) (range 4))
                '())
  (check-equal? (map/transposed 2 (λ (i) (list i i)) '())
                '[() ()])
  (check-equal? (map/transposed 2 (λ (i) (list i (* i i))) (range 4))
                (list (list 0 1 2 3)
                      (list 0 1 4 9))))
