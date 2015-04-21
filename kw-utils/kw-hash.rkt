#lang racket/base

(provide apply/kw-hash
         app/kw-hash
         make-kw-hash
         make-kw-hash+list
         keyword-app-make-kw-hash
         )

(require "keyword-lambda.rkt"
         "keyword-app.rkt"
         )
(module+ test
  (require rackunit racket/math))

;; (apply/kw-hash proc kw-hash arg ... rst-args)
(define apply/kw-hash
  (keyword-lambda (kws kw-args proc kw-hash . other-args)
    (define kw-lop
      (sort (hash->list kw-hash) keyword<? #:key car))
    (keyword-apply keyword-apply kws kw-args proc (map car kw-lop) (map cdr kw-lop) other-args)))

(define app/kw-hash
  (keyword-lambda (kws kw-args proc kw-hash . rst-args)
    (keyword-app apply/kw-hash kws kw-args proc kw-hash rst-args)))

;; equivalent to (keyword-app make-kw-hash kws kw-args)
(define (keyword-app-make-kw-hash kws kw-args)
  (make-immutable-hash
   (for/list ([kw     (in-list kws)]
              [kw-arg (in-list kw-args)])
     (cons kw kw-arg))))

(define make-kw-hash
  (keyword-lambda (kws kw-args)
    (keyword-app-make-kw-hash kws kw-args)))

(define make-kw-hash+list
  (keyword-lambda (kws kw-args . args)
    (define kw-hash
      (keyword-app-make-kw-hash kws kw-args))
    (values kw-hash args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (test-case "apply/kw-hash"
    (check-equal? (apply/kw-hash list (hash) 0 1 '(2 3))
                  '(0 1 2 3))
    (check-equal? (app/kw-hash list (hash) 0 1 '(2 3))
                  '(0 1 (2 3)))
    (define (kinetic-energy #:m m #:v v)
      (* 1/2 m (sqr v)))
    (check-equal? (apply/kw-hash kinetic-energy (hash '#:m 2 '#:v 1) '())
                  1)
    )
  )
