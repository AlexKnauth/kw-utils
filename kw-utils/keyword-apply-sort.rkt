#lang racket/base

(provide keyword-apply/sort)

(require "keyword-lambda.rkt")

(module+ test
  (require rackunit racket/local racket/math))

;; like keyword-apply, but without the constraint that the kws must be sorted
(define keyword-apply/sort
  (keyword-lambda (kws kw-args f other-kws other-kw-args . rest-args)
    (let* ([kw-lop (for/list ([kw     (in-list (append kws     other-kws))]
                              [kw-arg (in-list (append kw-args other-kw-args))])
                     (cons kw kw-arg))]
           [sorted-kw-lop (sort kw-lop keyword<? #:key car)]
           [sorted-kws     (map car sorted-kw-lop)]
           [sorted-kw-args (map cdr sorted-kw-lop)])
      (keyword-apply f sorted-kws sorted-kw-args (apply list* rest-args)))))

(module+ test
  (local []
    (define (kinetic-energy #:mass m #:velocity v)
      (* 1/2 m (sqr v)))
    (check-equal? (keyword-apply/sort kinetic-energy '(#:mass #:velocity) '(2 1) '())
                  1)
    (check-equal? (keyword-apply/sort kinetic-energy '(#:velocity #:mass) '(1 2) '())
                  1)
    ))

