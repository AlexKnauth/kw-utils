#lang racket/base

(provide kw-hash->)

(require racket/contract/base
         racket/contract/combinator
         racket/function
         racket/list
         "../keyword-lambda.rkt"
         "../kw-hash.rkt"
         (for-syntax racket/base
                     syntax/parse
                     ))
(module+ test
  (require rackunit racket/contract/region))

(define-syntax kw-hash->
  (lambda (stx)
    (syntax-parse stx #:literals (any)
      [(kw-hash-> [arg/c ...] #:kws kw-hash/c any)
       #:declare arg/c (expr/c #'chaperone-contract? #:name "argument contract")
       #:declare kw-hash/c (expr/c #'chaperone-contract? #:name "kw-hash contract")
       (syntax/loc stx
         (make-kw-hash->any (list arg/c.c ...) #false kw-hash/c.c))]
      [(kw-hash-> [arg/c ...] #:rest rest/c #:kws kw-hash/c any)
       #:declare arg/c (expr/c #'chaperone-contract? #:name "argument contract")
       #:declare rest/c (expr/c #'chaperone-contract? #:name "rest contract")
       #:declare kw-hash/c (expr/c #'chaperone-contract? #:name "kw-hash contract")
       (syntax/loc stx
         (make-kw-hash->any (list arg/c.c ...) rest/c.c kw-hash/c.c))]
      )))

;; make-kw-hash->any :
;; (Listof Chaperone-Contract) (Maybe Chaperone-Contract) Chaperone-Contract -> Chaperone-Contract
;; The function that kw-hash-> expands into
(define (make-kw-hash->any arg-ctcs rest-ctc kw-hash-ctc)
  (make-chaperone-contract
   #:name `(kw-hash-> ,(map contract-name arg-ctcs)
                      ,@(if rest-ctc
                            `(#:rest ,(contract-name rest-ctc))
                            `())
                      #:kws ,(contract-name kw-hash-ctc)
                      any)
   #:first-order procedure?
   #:projection (make-kw-hash->any-proj
                 (map contract-projection arg-ctcs)
                 (and rest-ctc (contract-projection rest-ctc))
                 (contract-projection kw-hash-ctc))))

;; Proj is [Blame -> [Any -> Any]]

;; make-kw-hash->any-proj : (Listof Proj) (Maybe Proj) Proj -> Proj
;; Makes projections for kw-hash-> contracts
(define ((make-kw-hash->any-proj arg-projs rest-proj kw-hash-proj) blame)
  (define n (length arg-projs))
  ;; arg-wrappers : (Listof [Arg -> Arg])
  (define arg-wrappers
    (get-arg-wrappers blame arg-projs))
  ;; rest-wrapper : (Option [(Listof Any) -> (Listof Any)])
  (define rest-wrapper
    (and rest-proj (get-arg-wrapper blame rest-proj "the rest argument of")))
  ;; kws-wrapper : [Kws-Hash -> Kws-Hash]
  (define kws-wrapper
    (get-arg-wrapper blame kw-hash-proj "the keywords of"))
  (lambda (f)
    (check-procedure blame f)
    (chaperone-procedure
     f
     (keyword-lambda (kws kw-args . args)
       (with-continuation-mark
        contract-continuation-mark-key blame
        (let ()
          (check-length blame f (length args)
                        (if rest-wrapper
                            (arity-at-least n)
                            n))
          (define args*
            (map app arg-wrappers (take args n)))
          (define rest*
            (and rest-wrapper (rest-wrapper (drop args n))))
          (define args+rest*
            (if rest-wrapper
                (append args* rest*)
                args*))
          (define kw-hash*
            (kws-wrapper (keyword-app-make-kw-hash kws kw-args)))
          ;; kw-args* has to be in the same order as kw-args
          (define kw-args*
            (map-hash-ref kw-hash* kws))
          (if (null? kw-args*)
              ;; if no keywords were passed in, don't include them
              (apply values args+rest*)
              (apply values kw-args* args+rest*))))))))

;; check-procedure : Blame Any -> Void
(define (check-procedure blame f)
  (unless (procedure? f)
    (raise-blame-error blame f '(expected: "procedure?" given: "~e") f)))

;; check-length : Blame Any Natural Procedure-Arity -> Void
(define (check-length blame f actual-length expected-arity)
  (unless (arity-includes? expected-arity actual-length)
    (cond
      [(exact-nonnegative-integer? expected-arity)
       (raise-blame-error (blame-swap blame) f
                          '(expected: "~v arguments" given: "~v non-keyword arguments")
                          expected-arity actual-length)]
      [(arity-at-least? expected-arity)
       (raise-blame-error (blame-swap blame) f
                          '(expected: "at least ~v arguments" given: "~v non-keyword arguments")
                          (arity-at-least-value expected-arity) actual-length)]
      [else
       (raise-blame-error (blame-swap blame) f
                          '(expected: "arity ~v" given: "~v non-keyword arguments")
                          expected-arity actual-length)])))

;; app : [a -> b] a -> b
(define (app f a)
  (f a))

;; map-hash-ref : (Hashof a b) (Listof a) -> (Listof b)
(define (map-hash-ref hash lst)
  (for/list ([key (in-list lst)])
    (hash-ref hash key)))

;; get-arg-wrapper : Blame Proj String -> [Any -> Any]
(define (get-arg-wrapper blame proj context)
  (define arg-blame
    (blame-add-context blame context #:swap? #t))
  (proj arg-blame))

;; get-arg-wrappers : Blame (Listof Proj) -> (Listof [Any -> Any])
(define (get-arg-wrappers blame arg-projs)
  (for/list ([proj (in-list arg-projs)]
             [i (in-naturals)])
    (get-arg-wrapper blame proj (format "argument ~v of" i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (define c
    (kw-hash-> [number? (listof symbol?)] #:kws (hash/c keyword? string?) any))
  (define c2
    (kw-hash-> [number? (listof symbol?)] #:rest (listof 1) #:kws (hash/c keyword? string?) any))
  (check-pred chaperone-contract? c)
  (check-pred chaperone-contract? c2)
  (check-equal? (contract-name c)
                '(kw-hash-> [number? (listof symbol?)] #:kws (hash/c keyword? string?) any))
  (check-equal? (contract-name c2)
                '(kw-hash-> [number? (listof symbol?)]
                            #:rest (listof 1) #:kws (hash/c keyword? string?)
                            any))
  (define/contract (f x syms #:hello [hello "hello"])
    c
    x)
  (check-equal? (f 3 '(a b c)) 3)
  (check-equal? (f 3 '(a b c) #:hello "wirled") 3)
  (check-exn exn:fail:contract:blame?
             (λ () (f 'three '(a b c))))
  (check-exn exn:fail:contract:blame?
             (λ () (f 3 '(one two 5))))
  (check-exn exn:fail:contract:blame?
             (λ () (f 3 '(a b c) #:hello 'not-a-string)))
  (define/contract (f2 x syms #:hello [hello "hello"] . rst)
    c2
    x)
  (check-equal? (f2 3 '(a b c)) 3)
  (check-equal? (f2 3 '(a b c) #:hello "wirled") 3)
  (check-equal? (f2 3 '(a b c) 1 1 1 1) 3)
  (check-equal? (f2 3 '(a b c) 1 #:hello "wirled" 1 1 1) 3)
  (check-exn exn:fail:contract:blame?
             (λ () (f2 'three '(a b c))))
  (check-exn exn:fail:contract:blame?
             (λ () (f2 3 '(one two 5))))
  (check-exn exn:fail:contract:blame?
             (λ () (f2 3 '(a b c) #:hello 'not-a-string)))
  (check-exn exn:fail:contract:blame?
             (λ () (f2 3 '(a b c) 1 1 1 2)))
  )
