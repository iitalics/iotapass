#lang racket/base
(provide
 compile-template)

(require
 (prefix-in mt: "../ast/metaterm.rkt")
 "../ast/decl.rkt"
 "../repr/ids.rkt"
 racket/list
 racket/match
 syntax/parse)

;; =======================================================================================

;; Compile a metaterm into an expression producing a language term; for use
;; in (template ..) or related macros.

;; syntax syntax language-repr-ids metaterm -> [listof syntax]
(define (compile-template src-stx macro-head repr-ids init-mt)
  (define pr=>ids (language-repr-ids-productions repr-ids))

  (when (or (mt:multiple? init-mt)
            (mt:build? init-mt))
    (error 'compile-template
           (format "metaterm may have multiple results: ~s" init-mt)))

  (car
   ;; metaterm -> [listof syntax]
   (let compile-mt ([mt init-mt])
     (match mt
       ; atomic
       [(mt:unquoted stx s)
        (list (make-spec-protect repr-ids src-stx macro-head s stx))]
       [(mt:datum stx s)
        (list (make-spec-protect repr-ids src-stx macro-head s #`'#,stx))]

       ; compound: production
       [(mt:prod pr body)
        (define/syntax-parse [ctor _ _] (hash-ref pr=>ids pr))
        (define/syntax-parse [arg ...] (compile-mt body))
        (list (quasisyntax/loc src-stx
                (ctor arg ...)))]

       ; compound: multiple results
       [(mt:multiple mts)
        (append-map compile-mt mts)]

       ; compound: combine results
       [(mt:build n-cols row-mts)
        (define/syntax-parse [(val ...) ...]
          (map/transposed n-cols compile-mt row-mts))
        (syntax->list
         (quasisyntax/loc src-stx
           {(vector-immutable val ...)
            ...}))]))))

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
