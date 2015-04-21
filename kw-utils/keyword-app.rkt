#lang racket/base

(provide keyword-app
         keyword-app/sort
         )

(require "keyword-apply-sort.rkt"
         )

(define (keyword-app f kws kw-args . rst-args)
  (keyword-apply f kws kw-args rst-args))

(define (keyword-app/sort f kws kw-args . rst-args)
  (keyword-apply/sort f kws kw-args rst-args))

