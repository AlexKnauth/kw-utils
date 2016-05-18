#lang racket/base

(provide keyword-lambda)

(require (for-syntax racket/base racket/syntax syntax/parse syntax/name))

(module+ test
  (require rackunit racket/local))

(begin-for-syntax
  (define-syntax-class args
    [pattern (arg:id ... [opt-arg:id default:expr] ...)
             #:with apply-id #'#%app
             #:with [apply-arg ...] #'[arg ... opt-arg ...]]
    [pattern (arg:id ... [opt-arg:id default:expr] ... . rest:id)
             #:with apply-id #'apply
             #:with [apply-arg ...] #'[arg ... opt-arg ... rest]]
    ))

;; (keyword-lambda (kws kw-args . rest-args) body ...+)
(define-syntax keyword-lambda
  (lambda (stx)
    (syntax-parse stx
      [(keyword-lambda (kws:id kw-args:id . rest-args:args) body:expr ...+)
       #:with name (syntax-local-infer-name stx)
       #:with name* (generate-temporary #'name)
       #:with name*-expr #'(lambda (kws kw-args . rest-args) body ...)
       #:with plain-expr #'(lambda rest-args
                             (rest-args.apply-id name* '() '() rest-args.apply-arg ...))
       (cond [(identifier? #'name)
              #'(let ([name* name*-expr])
                  (make-keyword-procedure
                   name*
                   (let ([name plain-expr])
                     name)))]
             [else
              #'(let ([name* name*-expr])
                  (make-keyword-procedure
                   name*
                   plain-expr))])])))

;; (keyword-lists-case-lambda (kws kw-args . rest-args) body ...+)
(define-syntax keyword-lists-case-lambda
  (lambda (stx)
    (syntax-parse stx
      [(keyword-lists-case-lambda kws:id kw-args:id [rest-args:args body:expr ...+] ...)
       #:with name (syntax-local-infer-name stx)
       #:with name* (generate-temporary #'name)
       #:with name*-expr #'(case-lambda
                             [(kws kw-args . rest-args) body ...]
                             ...)
       #:with plain-expr #'(case-lambda
                             [rest-args
                              (rest-args.apply-id name* '() '() rest-args.apply-arg ...)]
                             ...)
       (cond [(identifier? #'name)
              #'(let ([name* name*-expr])
                  (make-keyword-procedure
                   name*
                   (let ([name plain-expr])
                     name)))]
             [else
              #'(let ([name* name*-expr])
                  (make-keyword-procedure
                   name*
                   plain-expr))])])))

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
