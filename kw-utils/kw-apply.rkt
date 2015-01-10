#lang racket/base

(provide apply/kw)

(require racket/match)

(module+ test
  (require rackunit (for-syntax racket/base syntax/parse)))

(define (apply/kw f args)
  (define-values (kws kw-args rest-args)
    (parse-args args))
  (keyword-apply f kws kw-args rest-args))

;; parse-args : (Listof Any) -> (values (Listof Keyword) (Listof Any) (Listof Any))
;; returns 3 values
;;   the first value is a list of the keywords
;;   the second values is a list of the keyword arguments
;;   the third value is a list of the by-position arguments
(define (parse-args args)
  (define (return #:kws kws #:kw-args kw-args #:rest-args rest-args)
    (values kws kw-args rest-args))
  (define-values (kw-hash bkwds-rst)
    (args->kw-hash+bkwds-rst args))
  (define kws (sort (hash-keys kw-hash) keyword<?))
  (define kw-args
    (for/list ([kw (in-list kws)])
      (hash-ref kw-hash kw)))
  (define rest-args (reverse bkwds-rst))
  (return #:kws kws
          #:kw-args kw-args
          #:rest-args rest-args))

;; args->hash+bwds-rst : (Listof Any) #:kw-hash (Hashof Keyword Any) #:bkwds-rst (Listof Any)
;;                       -> (values (Hashof Keyword Any) (Listof Any))
;; returns 2 values
;;   the first value is a hash-table containing the keywords and keyword-arguments
;;   the second value is a backwards list of the by-position arguments
;; both values are accumulated in the #:kw-hash and #:bkwds-rst arguments
(define (args->kw-hash+bkwds-rst args #:kw-hash [kw-hash #hash()] #:bkwds-rst [bkwds-rst '()])
  (define (return #:kw-hash [kw-hash kw-hash] #:bkwds-rst [bkwds-rst bkwds-rst])
    (values kw-hash bkwds-rst))
  (match args
    [(list)
     (return)]
    [(list arg)
     (return #:bkwds-rst (cons arg bkwds-rst))]
    [(list (? keyword? kw) kw-arg)
     (return #:kw-hash (hash-set kw-hash kw kw-arg))]
    [(list-rest (and arg (not (? keyword?))) rest)
     (args->kw-hash+bkwds-rst rest
                              #:kw-hash kw-hash
                              #:bkwds-rst (cons arg bkwds-rst))]
    [(list-rest (? keyword? kw) kw-arg rest)
     (args->kw-hash+bkwds-rst rest
                              #:kw-hash (hash-set kw-hash kw kw-arg)
                              #:bkwds-rst bkwds-rst)]))



(module+ test
  (define-syntax-rule (values->list expr)
    (call-with-values (λ () expr) list))
  (define-syntax check-match/values
    (lambda (stx)
      (syntax-parse stx #:literals (values)
        [(check-match/values expr (values pat ...))
         (syntax/loc stx
           (check-match (values->list expr) (list pat ...)))])))
  (test-case "apply/kw"
    (define proc (make-keyword-procedure list))
    (check-equal? (apply/kw proc '(1 2 3)) '(() () 1 2 3))
    (check-equal? (apply/kw proc '(1 #:kw2 kw-arg2 2 5 #:kw1 kw-arg1 3))
                  '((#:kw1 #:kw2) (kw-arg1 kw-arg2) 1 2 5 3)))
  (test-case "parse-args"
    (check-match/values (parse-args '())
                        (values '() '() '()))
    (check-match/values (parse-args '(0))
                        (values '() '() '(0)))
    (check-match/values (parse-args '(#:kw "kw-arg"))
                        (values '(#:kw) '("kw-arg") '()))
    (check-match/values (parse-args '(0 1 #:kw-1 "kw-arg-1" 2 #:kw-2 "kw-arg-2" 3 4))
                        (values '(#:kw-1 #:kw-2) '("kw-arg-1" "kw-arg-2") '(0 1 2 3 4)))
    (check-match/values (parse-args '(0 1 2 #:kw-2 "kw-arg-2" 3 4 #:kw-1 "kw-arg-1"))
                        (values '(#:kw-1 #:kw-2) '("kw-arg-1" "kw-arg-2") '(0 1 2 3 4))))
  )
