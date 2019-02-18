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
       (values (list (binding id expr))
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
      [(mt:build n-cols rows)
       (define-values [bindings exprss]
         (let mts->bindings+exprss ([rows rows])
           (if (null? rows)
             (values '() (make-list n-cols '()))
             (let-values ([(bs1 es) (mt->bindings+exprs (car rows))]
                          [(bs2 ess) (mts->bindings+exprss (cdr rows))])
               (values (append bs1 bs2)
                       (map cons es ess))))))
       (define vector-exprs
         (for/list ([exprs (in-list exprss)])
           (quasisyntax/loc src-stx
             (vector-immutable #,@exprs))))
       (values bindings vector-exprs)]))

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

;; (binding identifier syntax)
(struct binding [name rhs] #:transparent)

;; binding -> syntax
(define (binding->syntax b)
  #`[#,(binding-name b)
     #,(binding-rhs b)])

;; [listof binding] syntax -> let*-syntax
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
