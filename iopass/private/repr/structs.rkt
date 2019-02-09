#lang racket/base
(provide
 (struct-out language-repr-ids)
 (struct-out production-repr-ids)
 (struct-out field-repr))

(require
 "../types.rkt"
 racket/match
 racket/list
 threading)

;; =======================================================================================

;; language-repr-ids ::=
;;   (language-repr-ids
;;    [hasheq nonterminal-spec => identifier]
;;    [hasheq production => production-repr-ids])
;; production-repr-ids ::=
;;   (production-repr-ids
(struct language-repr-ids
  [nonterminal-predicates
   productions])

(struct production-repr-ids
  [constructor-id
   predicate-id
   accessor-id])

;; field-repr ::= (field-repr symbol nat)
(struct field-repr
  [metavar-symbol
   ellipsis-depth]
  #:transparent)

;; form -> exact-integer
;; Return the number of fields in the representation of the given form
(define (form-field-count fm)
  ;; [listof form] -> exact-integer
  (define (count* fms)
    (for/sum ([fm (in-list fms)])
      (form-field-count fm)))
  (match fm
    [(metavar _ x) 1]
    [(form-list _ before rep after)
     (+ (count* before)
        (count* after)
        (if rep
          (form-field-count (ellipsis-repeated-form rep))
          0))]))

;; form -> [listof field-repr]
;; Return list of fields to represent the given form
(define (form-field-repr fm)
  (reverse
   (let repr/depth ([fm fm]
                    [lst '()]
                    [ed 0])

     (define (repr fm lst)
       (repr/depth fm lst 0))
     (define (repr-ellipse repeat lst)
       (if repeat
         (repr/depth (ellipsis-repeated-form repeat)
                     lst
                     (add1 ed))
         lst))

     (match fm
       [(metavar _ x)
        (cons (field-repr x ed)
              lst)]

       [(form-list _ before repeat after)
        (~> lst
            (foldl repr _ before)
            (repr-ellipse repeat _)
            (foldl repr _ after))]))))


(module+ test
  (require rackunit)

  ;; (x y z ... [w])
  (let ([fm (form-list 0
                       (list (metavar 1 'x)
                             (metavar 2 'y))
                       (ellipsis (metavar 3 'z))
                       (list (form-list 4
                                        (list (metavar 5 'w))
                                        #f
                                        '())))])
    (check-equal? (form-field-count fm) 4)
    (check-equal? (form-field-repr fm)
                  (list (field-repr 'x 0)
                        (field-repr 'y 0)
                        (field-repr 'z 1)
                        (field-repr 'w 0))))

  ;; ([x ...] ...)
  (let ([fm (form-list 0
                       '()
                       (ellipsis (form-list 1
                                            '()
                                            (ellipsis (metavar 2 'x))
                                            '()))
                       '())])
    (check-equal? (form-field-count fm) 1)
    (check-equal? (form-field-repr fm)
                  (list (field-repr 'x 2)))))
