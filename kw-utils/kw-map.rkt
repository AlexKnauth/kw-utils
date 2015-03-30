#lang racket/base

(provide map)

(require (only-in racket/base [map rkt:map])
         racket/list
         "keyword-lambda.rkt"
         )
(module+ test
  (require rackunit racket/math))

(define map
  (keyword-lambda (kws kw-args f . args)
    (cond
      [(and (empty? args) (empty? kw-args))
       (error 'map "expected at least one list argument")]
      [(empty? kw-args)
       (apply rkt:map f args)]
      [(empty? args)
       (for/list ([kw-args (in-list (apply rkt:map list kw-args))])
         (keyword-apply f kws kw-args '()))]
      [else
       (define argss (apply rkt:map list args))
       (define kw-argss (apply rkt:map list kw-args))
       (unless (= (length argss) (length kw-argss))
         (error 'map "all lists must have same size, given ~v and ~v with different lengths"
                (first args) (first kw-args)))
       (for/list ([args (in-list argss)]
                  [kw-args (in-list kw-argss)])
         (keyword-apply f kws kw-args args))])))

(module+ test
  (define (KE #:m m #:v v)
    (* 1/2 m (sqr v)))
  (check-equal? (map KE #:m '(2 2 2 2) #:v '(0 1 2 3))
                '(0 1 4 9))
  (check-equal? (map KE #:m '(0 1 2 3) #:v '(0 1 2 3))
                '(0 1/2 4 27/2))
  (check-equal? (map KE #:m '(1 2 1/2 2/9) #:v '(0 1 2 3))
                '(0 1 1 1))
  )
