#lang racket/base
(provide
 compile-template)

(require
 (prefix-in mt: "../ast/metaterm.rkt")
 "../ast/decl.rkt"
 "../repr/ids.rkt"
 racket/match
 syntax/parse)

;; =======================================================================================

(module protect-value-function racket/base
  (provide protect-value)
  ;; any [any -> bool | X] string any -> X
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

;; ------------------------------------

;; syntax syntax language-repr-ids metaterm -> syntax
(define (compile-template src-stx macro-head repr-ids mt)
  (define pr=>ids (language-repr-ids-productions repr-ids))
  (let compile1 ([mt mt])
    (match mt
      [(mt:unquoted stx s)
       (make-spec-protect repr-ids src-stx macro-head s stx)]
      [(mt:datum stx s)
       (make-spec-protect repr-ids src-stx macro-head s #`'#,stx)]
      [(mt:prod pr args)
       (define/syntax-parse [ctor _ _] (hash-ref pr=>ids pr))
       (quasisyntax/loc src-stx
         (ctor #,@(map compile1 args)))]
      [(list mts ...)
       (quasisyntax/loc src-stx
         (vector-immutable #,@(map compile1 mts)))])))

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
