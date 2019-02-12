#lang racket/base
(provide
 define-terminals
 define-language)

(require
 (submod "../repr/structs.rkt" generate-structs-macro)
 (for-syntax
  (prefix-in types: "../types.rkt")
  "../syntax/bindings.rkt"
  "../syntax/classes.rkt"
  "../repr/ids.rkt"
  racket/base
  racket/match
  (rename-in syntax/parse [attribute @])))

;; =======================================================================================

(begin-for-syntax
  ;; [listof spec] -> [listof symbol]
  ;; Raises syntax error if any metavars are repeated
  (define (check-spec-set-metavars ss)
    (match (types:spec-set-metavars ss)
      [(types:spec-repeated-metavar-error stx x)
       (raise-syntax-error #f
         (format "metavariable '~a' defined multiple times" x)
         stx)]
      [mvs
       mvs])))

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
    [(_ language-name:id
        #:terminals terminals-id:id
        nt:nonterminal-spec
        ...)
     #:cut
     #:with :terminals-definition-binding #'terminals-id
     #:with [[every-prod-repr-id ...] ...] #'[nt.prod-repr-ids ... ...]
     #`(begin
         (define-syntax language-name
           (let ()
             (define stx (quote-syntax #,this-syntax))
             ; terminals
             (define tms (get-terminals (quote-syntax terminals-id)))
             ; nonterminals
             (define nts (list nt.generate ...))
             (define nt=>pred-id
               (make-hasheq (map cons nts (list #'nt.pred-repr-id ...))))
             (define nt=>repr-ids
               (make-hasheq (map cons nts (list (list #'nt.prod-repr-ids ...) ...))))
             ; all metavars
             (define mvs (check-spec-set-metavars (append tms nts)))
             ; ----
             (check-nonterminals nts mvs)
             (language-definition
              (types:make-language stx 'language-name tms nts)
              (make-language-repr-ids nts nt=>pred-id nt=>repr-ids))))

         (define-values [nt.pred-repr-id ... every-prod-repr-id ... ...]
           (generate-structs language-name)))]))

(begin-for-syntax
  ;; [listof nonterminal-spec] [setof metavar] -> void
  ;; Raises syntax error if any nonterminals are malformed
  (define (check-nonterminals nts metavars)
    (for ([nt (in-list nts)])
      (match (types:nonterminal-unbound-metavar nt metavars)
        [#f (void)]
        [(types:metavar stx x)
         (raise-syntax-error #f
           (format "metavariable unbound")
           stx)]))))

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
     (list (types:terminal-spec _ '(i j) _ _)
           (types:terminal-spec _ '(x y) _ _)
           (types:terminal-spec _ '(s) _ _)))

    (check-match
     (get-language #'L0)
     (types:language
      _
      'L0
      (== (get-terminals #'T0))
      (list
       ;; [e ::= (num i) (op s e ...)]
       (types:nonterminal-spec
        _
        '(e)
        (list (types:production _
                                'num
                                (types:form-list _ (list (types:metavar _ 'i)) #f '()))
              (types:production _
                                'op
                                (types:form-list _
                                                 (list (types:metavar _ 's))
                                                 (types:ellipsis (types:metavar _ 'e))
                                                 '()))))

       ;; [df ::= (def x e)]
       (types:nonterminal-spec
        _
        '(df)
        (list (types:production _
                                'def
                                (types:form-list _
                                                 (list (types:metavar _ 'x)
                                                       (types:metavar _ 'e))
                                                 #f
                                                 '())))))
      _))

    (define (check-lang-repr-ids-hash-tables lang-id)
      (define lang (get-language lang-id))
      (match-define (language-repr-ids _ nt=>pred-id pr=>ids)
        (get-language-repr-ids #'L0))
      (for ([nt (in-list (types:language-nonterminals lang))])
        (check-match (hash-ref nt=>pred-id nt)
                     (? identifier?))
        (for ([pr (in-list (types:nonterminal-spec-productions nt))])
          (check-match (hash-ref pr=>ids pr)
                       (list (? identifier?)
                             (? identifier?)
                             (? identifier?))))))

    (check-lang-repr-ids-hash-tables #'L0))

  ;; =================================================================

  (require
   rackunit)

  (define-syntax terminal-def-stuff
    (syntax-parser
      [(_ b:terminals-definition-binding)
       #:with [e ...] (for/list ([t (in-list (@ b.terminals))])
                        (match t
                          [(types:terminal-spec _ mvs p e)
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
