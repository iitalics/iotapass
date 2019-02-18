#lang racket/base
(provide
 (all-defined-out))

;; =======================================================================================

;; A 'metaterm' generalizes the syntax of a pattern and a template. Both positions contain
;; a 'metaterm', representing an instantiation of a 'form' (in decl.rkt).

;; The difference between a pattern and a template is basically just how they are
;; compiled. In particular, (unquoted ..) are interpreted very differently in patterns and
;; templates.

;; metaterm ::=
;;   (unquoted syntax spec)            unquoted expression        (",e")
;;   (datum syntax terminal-spec)      implicitly-quoted datum    ("5")
;;   (multiple [listof metaterm])      multiple values            ("[x ,e]")
;;   (prod production metaterm)        production                 ("(+ 0 ,e)")
;;   (build nat metaterm-seq)          sequence of terms          ("(,x ,y)", "(,x ,x* ...)")
(struct unquoted [stx spec] #:transparent)
(struct datum [stx spec] #:transparent)
(struct multiple [mts] #:transparent)
(struct prod [prod body-mt] #:transparent)
(struct build [n-cols rows] #:transparent)

;; NOTE: The difference between "multiple" and "build" depends on the context (the form
;;       dictating how the metaterm is parsed). A "build" term is for combining multiple
;;       expressions into one sequence. A "multiple" term is when there are multiple
;;       values/positions represented by the term.
;;
;; FORM        SYNTAX        METATERM
;; [x y]       [hi world]    (multiple (list (datum #'hi <x>) (datum #'world <y>))
;; [x ...]     [hi world]    (build 1 (list (datum #'hi <x>) (datum #'world <x>)))
;; [(x i) ...] [(A 0) (B 1)] (build 2 (list (multiple (datum #'A <x>) (datum #'0 <i>))
;;                                          (multiple (datum #'B <x>) (datum #'1 <i>))))

;; metaterm-seq ::=
;;   [listof (or metaterm (e metaterm))]
(struct e [wrapped] #:transparent)
