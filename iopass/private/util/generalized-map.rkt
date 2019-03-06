#lang racket/base
(require racket/list)
(provide (rename-out [gmap generalized-map]))

;; (n : nat) [X ... -> Y ...] [listof X] ... -> [listof Y] ...
;; {where the number of Y's must be equal to 'n'}
(define-syntax gmap
  (syntax-rules ()
    [(_ n f)       (apply values (make-list 'n '()))]
    [(_ 1 f l ...) (map f l ...)]
    [(_ 2 f l ...) (map/2 f l ...)]
    [(_ n f l ...) (apply values (map/* 'n f l ...))]))

;; [X ... -> Y Z] [listof X] ... -> [listof Y] [listof Z]
(define (map/2 f . ls)
  (let loop ([ls ls])
    (cond
      [(andmap null? ls)
       (values '() '())]
      [(andmap pair? ls)
       (let-values ([(hd1 hd2) (apply f (map car ls))]
                    [(tl1 tl2) (loop (map cdr ls))])
         (values (cons hd1 tl1)
                 (cons hd2 tl2)))]
      [else
       (error 'map "all lists must have same size")])))

;; nat [X -> Y ...] [listof X] -> [list [listof Y] ...]
(define (map/* n f . ls)
  (let loop ([ls ls])
    (cond
      [(andmap null? ls) (make-list n '())]
      [(andmap pair? ls)
       (call-with-values (λ () (apply f (map car ls)))
                         (λ xs (map cons xs (loop (map cdr ls)))))]
      [else
       (error 'map "all lists must have same size")])))

(module+ test
  (require rackunit)

  (define-syntax-rule (check-equal?/* lhs rhss ...)
    (check-equal? (call-with-values (λ () lhs) vector)
                  (vector rhss ...)))

  ; 1 -> 2
  (define (id&sqr x) (values x (* x x)))
  ; 2 -> 2
  (define (swap x y) (values y x))
  ; n -> 2
  (define (two x y . zs) (values x y))
  ; 1 -> 3
  (define (poly x) (values x (* x x) (* x x x)))
  ; 3 -> 3
  (define (rot x y z) (values z x y))

  (check-equal? (gmap 1 + '() '())            '())
  (check-equal? (gmap 1 + '(1 2 3) '(4 5 6))  '(5 7 9))
  (check-equal? (gmap 1 +)                    '())

  (check-equal?/* (gmap 2 id&sqr '(1 2 3))                '(1 2 3)   '(1 4 9))
  (check-equal?/* (gmap 2 swap '(a b) '("a" "b"))         '("a" "b") '(a b))
  (check-equal?/* (gmap 2 two '(1 2) '(11 22) '(111 222)) '(1 2)     '(11 22))
  (check-equal?/* (gmap 2 +)                              '()        '())

  (check-equal?/* (gmap 3 poly '(1 2 3))                '(1 2 3) '(1 4 9) '(1 8 27))
  (check-equal?/* (gmap 3 rot '(1 2) '(11 22) '(-1 -2)) '(-1 -2) '(1 2)   '(11 22))

  (check-exn #px"all lists must have same size"
             (λ () (gmap 1 + '(1 2 3) '(4 5))))
  (check-exn #px"all lists must have same size"
             (λ () (gmap 2 swap '(1 2 3) '(4))))
  (check-exn #px"all lists must have same size"
             (λ () (gmap 3 rot '(1 2 3) '(4 6 8) '(9 10 11 12)))))
