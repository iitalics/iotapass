#lang racket/base
(provide
 pattern)

(require
 racket/match
 (for-syntax
  (prefix-in c: "../syntax/classes.rkt")
  (prefix-in mt: "../ast/metaterm.rkt")
  "../syntax/metaterm.rkt"
  "../repr/ids.rkt"
  (rename-in syntax/parse [attribute @])
  racket/base
  racket/match))

(begin-for-syntax

  ;; -------------
  ;; compile-pattern
  ;; -------------

  ;; syntax language-repr-ids metaterm -> syntax
  (define (compile-pattern src-stx repr-ids mt)
    (match mt

      [(mt:unquoted stx spec)
       (syntax-parse stx
         [x:id #`(? #,(spec-predicate-id spec repr-ids) x)])]

      [(mt:datum stx spec)
       #`(quote #,stx)])))

;; -------------
;; pattern
;; -------------

(define-match-expander pattern
  (syntax-parser
    [(_ :c:language+metavar patn)
     (compile-pattern #'patn
                      (@ repr-ids)
                      (parse-mt (@ spec)
                                (@ language)
                                #'patn))]))

;; ------------------

(module+ test
  (require
   rackunit
   "define.rkt")

  ;; (check-pattern pat
  ;;   #:yes [yes-expr yes-additional-checks ...] ...
  ;;   #:no no-expr ...)
  (define-syntax check-pattern
    (syntax-parser
      [(_ pat
          {~optional {~seq #:yes [y-expr y-tests ...] ...}
                     #:defaults ([(y-expr 1) '()] [(y-tests 2) '()])}
          {~optional {~seq #:no n-expr ...}
                     #:defaults ([(n-expr 1) '()])})
       #'(begin
           (check-match y-expr
                        pat
                        (begin y-tests ... #t))
           ...
           (check-match n-expr (not pat))
           ...)]))

  ;; ---------------------

  (define-terminals T
    [i j ::= integer? #:compare =]
    [x y ::= symbol? #:compare eq?]
    [s ::= string?])

  (define-language L0
    #:terminals T)

  ;; test datum
  (check-pattern (pattern (L0 i) 3) #:yes [3] #:no 2)
  (check-pattern (pattern (L0 x) a) #:yes ['a] #:no 'b)

  ;; test unquote
  (check-pattern (pattern (L0 x) ,sym)
                 #:yes ['a] ['b] ['c (check-eq? sym 'c)]
                 #:no 3 "a"))
