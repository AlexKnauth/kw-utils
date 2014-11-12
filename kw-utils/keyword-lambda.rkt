#lang racket/base

(provide keyword-lambda)

(require (for-syntax racket/base syntax/parse syntax/name))

(module+ test
  (require rackunit racket/local))

;; (keyword-lambda (kws kw-args . rest-args) body ...+)
(define-syntax keyword-lambda
  (lambda (stx)
    (syntax-parse stx
      [(keyword-lambda (kws:id kw-args:id . rest-args) body:expr ...+)
       (define name (syntax-local-infer-name stx))
       (cond [(or (symbol? name) (identifier? name))
              (with-syntax ([name name])
                #'(make-keyword-procedure
                   (lambda (kws kw-args . rest-args) body ...)
                   (let ([name (lambda rest-args
                                 (let ([kws '()] [kw-args '()])
                                   body ...))])
                     name)))]
             [else #'(make-keyword-procedure
                      (lambda (kws kw-args . rest-args) body ...)
                      (lambda rest-args
                        (let ([kws '()] [kw-args '()])
                          body ...)))])])))

(module+ test
  (local [(define proc
            (keyword-lambda (kws kw-args . rest-args)
              (list kws kw-args rest-args)))]
    (check-equal? (proc #:a 'a #:b 'b 0 1 2)
                  (list '(#:a #:b) '(a b) '(0 1 2)))
    (check-equal? (object-name proc) 'proc)
    ))
