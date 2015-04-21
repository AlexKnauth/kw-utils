#lang racket/base

(provide kw-hash-lambda)

(require "keyword-lambda.rkt"
         "kw-hash.rkt"
         (for-syntax racket/base
                     syntax/parse
                     ))
(module+ test
  (require rackunit))

(define-syntax kw-hash-lambda
  (lambda (stx)
    (syntax-parse stx
      [(kw-hash-lambda rest-args #:kws kw-hash:id body:expr ...+)
       #'(keyword-lambda (kws kw-args . rest-args)
           (let ([kw-hash (keyword-app-make-kw-hash kws kw-args)])
             body ...))])))

(module+ test
  (test-case "kw-hash-lambda"
    (define proc
      (kw-hash-lambda rest-args #:kws kw-hash
        (list rest-args kw-hash)))
    (check-equal? (proc 0 1 2 #:a 'a #:b 'b)
                  (list '(0 1 2) (hash '#:a 'a '#:b 'b)))
    (check-equal? (object-name proc) 'proc)
    ))
