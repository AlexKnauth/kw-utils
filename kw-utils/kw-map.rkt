#lang sweet-exp racket/base

provide map

require (only-in racket/base [map rkt:map])
        racket/list
        my-cond/iffy
        "keyword-lambda.rkt"
module+ test
  require rackunit
          racket/math

define map
  keyword-lambda (kws kw-args f . args)
    my-cond
      if {empty?(args) and empty?(kw-args)}
        error('map "expected at least one list argument")
      else-if empty?(kw-args)
        apply(rkt:map f args)
      else-if empty?(args)
        for/list ([kw-args in-list(apply(rkt:map list kw-args))])
          keyword-apply(f kws kw-args '())
      else
        define argss apply(rkt:map list args)
        define kw-argss apply(rkt:map list kw-args)
        unless {length(argss) = length(kw-argss)}
          error('map "all lists must have same size, given ~v and ~v with different lengths"
                first(args) first(kw-args))
        for/list ([args in-list(argss)]
                  [kw-args in-list(kw-argss)])
          keyword-apply(f kws kw-args args)

module+ test
  check-equal? (map (λ (#:x x) {x + 1}) #:x '(1 2 3 4))
               '(2 3 4 5)
  check-equal? (map (λ (x #:y y) {x + y}) '(1 2 3 4) #:y '(10 100 1000 10000))
               '(11 102 1003 10004)
  define (KE #:m m #:v v)
    {1/2 * m * sqr(v)}
  check-equal? (map KE #:m '(2 2 2 2) #:v '(0 1 2 3))
               '(0 1 4 9)
  check-equal? (map KE #:m '(0 1 2 3) #:v '(0 1 2 3))
               '(0 1/2 4 27/2)
  check-equal? (map KE #:m '(1 2 1/2 2/9) #:v '(0 1 2 3))
               '(0 1 1 1)

