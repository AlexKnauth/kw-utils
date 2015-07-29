#lang sweet-exp racket/base

provide mapper

require "keyword-lambda.rkt"
        "kw-map.rkt"
        "partial.rkt"
module+ test
  require rackunit

define mapper
  keyword-lambda (kws kw-args . args)
    (partial map (keyword-apply partial kws kw-args args))

module+ test
  check-equal? ((mapper add1) '(1 2 3)) '(2 3 4)
  check-equal? ((mapper +) '(1 2 3) '(4 5 6)) '(5 7 9)
  check-equal? ((mapper + 3) '(1 2 3) '(4 5 6)) '(8 10 12)
  check-equal? ((mapper app + 3) '(1 2 3) '(4 5 6)) '(8 10 12)
  check-equal? ((mapper) (list add1 sub1) '(0 0)) '(1 -1)
  
