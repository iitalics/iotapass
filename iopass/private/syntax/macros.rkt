#lang racket/base
(provide
 define-terminals
 define-language)

(require
 (for-syntax
  (prefix-in τ/ "../types.rkt")
  "bindings.rkt"
  "classes.rkt"
  racket/base
  racket/match
  (rename-in syntax/parse [attribute @])))

;; =======================================================================================

(begin-for-syntax
  ;; [listof spec] -> [listof symbol]
  ;; Raises syntax error if any metavars are repeated
  (define (check-spec-set-metavars ss)
    (match (τ/spec-set-metavars ss)
      [(τ/spec-repeated-metavar-error stx x)
       (raise-syntax-error #f
         (format "metavariable '~a' defined multiple times" x)
         stx)]
      [mvs
       mvs]))

  ;; [listof nonterminal-spec] [setof metavar] -> void
  ;; Raises syntax error if any nonterminals are malformed
  (define (check-nonterminals nts metavars)
    (for ([nt (in-list nts)])
      (match (τ/nonterminal-unbound-metavar nt metavars)
        [#f (void)]
        [(τ/metavar stx x)
         (raise-syntax-error #f
           (format "metavariable unbound")
           stx)]))))

;; ----------------------
;; define-terminals
;; ----------------------

(define-syntax define-terminals
  (syntax-parser
    [(_ name:id
        tm:terminal-spec
        ...)
     #'(begin
         tm.definitions ... ...
         (define-syntax name
           (let ([tms (list tm.generate ...)])
             (check-spec-set-metavars tms)
             (terminals-definition tms))))]))

;; ----------------------
;; define-language
;; ----------------------

(define-syntax define-language
  (syntax-parser
    [(_ name:id
        #:terminals terminals-id:id
        nt:nonterminal-spec
        ...)
     #:cut
     #:with :terminals-definition-binding #'terminals-id
     #`(define-syntax name
         (let ([stx (quote-syntax #,this-syntax)]
               [tms (get-terminals (quote-syntax terminals-id))]
               [nts (list nt.generate ...)])
           (let ([mvs (check-spec-set-metavars (append tms nts))])
             (check-nonterminals nts mvs)
             (language-definition
              (τ/make-language stx 'name tms nts)))))]))

(module+ test

  (require
   (prefix-in r: racket/base)
   racket/match)

  (define equal?
    ; (for testing hygiene)
    'not-equal?)

  ;; Examples

  (define-terminals T0
    [i j ::= integer? #:compare =]
    [x y ::= symbol? #:compare eq?]
    [s ::= string?])

  (define-terminals T1
    [binary ::= [+ - * /]])

  #;
  (define-terminals T-bad1
    [i j i ::= integer?])

  (define-language L0
    #:terminals T0
    [e ::=
       (num i)
       (op s e ...)]
    [df ::=
        (def x e)])

  #;
  (define-language L-bad1
    #:terminals T0
    [e f ::= (num i)]
    [e ::= ($ s)])

  #;
  (define-language L-bad2
    #:terminals T0
    [e ::= (num k)])

  ;; =================================================================

  (begin-for-syntax
    (require
     rackunit
     (for-syntax racket/base))

    ;; -------------------------
    ;; Expansion time tests

    (check-match
     (get-terminals #'T0)
     (list (τ/terminal-spec _ '(i j) _ _)
           (τ/terminal-spec _ '(x y) _ _)
           (τ/terminal-spec _ '(s) _ _)))

    (check-match
     (get-language #'L0)
     (τ/language
      _
      'L0
      (== (get-terminals #'T0))
      (list
       ;; [e ::= (num i) (op s e ...)]
       (τ/nonterminal-spec
        _
        '(e)
        (list (τ/production _
                            'num
                            (τ/form-list _ (list (τ/metavar _ 'i)) #f '()))
              (τ/production _
                            'op
                            (τ/form-list _
                                         (list (τ/metavar _ 's))
                                         (τ/ellipsis (τ/metavar _ 'e))
                                         '()))))

       ;; [df ::= (def x e)]
       (τ/nonterminal-spec
        _
        '(df)
        (list (τ/production _
                            'def
                            (τ/form-list _
                                         (list (τ/metavar _ 'x)
                                               (τ/metavar _ 'e))
                                         #f
                                         '())))))
      _)))

  ;; =================================================================

  (require
   rackunit)

  (define-syntax terminal-def-stuff
    (syntax-parser
      [(_ b:terminals-definition-binding)
       #:with [e ...] (for/list ([t (in-list (@ b.terminals))])
                        (match t
                          [(τ/terminal-spec _ mvs p e)
                           #`(list '#,mvs #,p #,e)]))
       #'(list e ...)]))

  ;; ---------------------
  ;; Runtime tests

  (let (;; screw with hygiene:
        [integer? boolean?])

    ; Check T0 generates the right identifiers
    (check-equal? (terminal-def-stuff T0)
                  (list (list '(i j) r:integer? =)
                        (list '(x y) r:symbol? eq?)
                        (list '(s)   r:string? r:equal?)))

    ; Check that the predicate generated by T1 is correct
    (match (terminal-def-stuff T1)
      [(list (list '(binary) binary-op? (== eq?)))
       (check-true (binary-op? '+))
       (check-true (binary-op? '-))
       (check-true (binary-op? '*))
       (check-true (binary-op? '/))
       (check-false (binary-op? 'add1))
       (check-false (binary-op? "+"))])))
