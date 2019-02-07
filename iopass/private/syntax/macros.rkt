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
      [(τ/spec-set-error stx msg)
       (raise-syntax-error #f msg stx)]
      [mvs
       mvs]))

  ;; [listof nonterminal-spec] [listof metavar] -> void
  ;; Raises syntax error if any nonterminals are malformed
  (define (check-nonterminals nts metavars)
    (void)))

;; ----------------------
;; define-terminals
;; ----------------------

(define-syntax define-terminals
  (syntax-parser
    [(_ name:id
        tm:terminal-spec
        ...)
     #'(define-syntax name
         (let ([tms (list tm.generate
                          ...)])
           (check-spec-set-metavars tms)
           (terminals-definition tms)))]))

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
             (language-definition (τ/language stx 'name tms nts)))))]))

(module+ test

  ;; Examples

  (define-terminals T0
    [i j ::= integer? #:compare =]
    [x y ::= symbol? #:compare eq?]
    [s ::= string?])

  (define-language L0
    #:terminals T0
    [e ::=
       (num i)
       (op s e ...)]
    [df ::=
        (def x e)])

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
      _
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
                                         '()))))))))

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

  (let ([i? integer?]
        [s? string?]
        [y? symbol?]
        ;; fuck with hygiene:
        [integer? boolean?])

    (check-equal? (terminal-def-stuff T0)
                  (list (list '(i j) i? =)
                        (list '(x y) y? eq?)
                        (list '(s)   s? equal?)))))
