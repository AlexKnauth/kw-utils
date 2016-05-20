#lang racket/base

(provide keyword-lambda)

(require "kw-lists-lambda.rkt")
(module+ test
  (require rackunit
           racket/local))

(define-syntax-rule (keyword-lambda (kws kw-args . rest-args) body ...)
  (kw-lists-lambda kws kw-args rest-args body ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (local [(define proc
            (keyword-lambda (kws kw-args . rest-args)
              (list kws kw-args rest-args)))]
    (check-equal? (proc #:a 'a #:b 'b 0 1 2)
                  (list '(#:a #:b) '(a b) '(0 1 2)))
    (check-equal? (object-name proc) 'proc)
    )
  (local [(define proc0
            (keyword-lambda (kws kw-args)
              (list kws kw-args)))
          (define proc1
            (keyword-lambda (kws kw-args x)
              (list kws kw-args x)))]
    (check-equal? (proc0 #:a 'a #:b 'b)
                  (list '(#:a #:b) '(a b)))
    (check-equal? (proc1 #:a 'a 'x #:b 'b)
                  (list '(#:a #:b) '(a b) 'x))
    (check-equal? (object-name proc0) 'proc0)
    (check-equal? (object-name proc1) 'proc1)
    (check-equal? (procedure-arity proc0) 0)
    (check-equal? (procedure-arity proc1) 1)
    )
  )
