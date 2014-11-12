#lang scribble/manual
@(require kw-utils/keyword-apply-sort
          racket/base
          scribble/eval
          (for-label kw-utils/keyword-apply-sort
                     racket/base
                     racket/contract/base
                     racket/math
                     ))

@title[#:tag "keyword-apply-sort.scrbl"]{keyword-apply/sort}

@defmodule[kw-utils/keyword-apply-sort]

@defproc[(keyword-apply/sort [f procedure?] [kws (listof keyword?)] [kw-args list?]
                             [v any/c] ... [lst list?] [#:<kw> kw-arg any/c] ...) any]{
like @racket[keyword-apply], but without the constraint that the keywords in @racket[kws] must be
sorted.  

@examples[
  (require kw-utils/keyword-apply-sort racket/math)
  (define (kinetic-energy #:mass m #:velocity v)
    (* 1/2 m (sqr v)))
  (keyword-apply/sort kinetic-energy '(#:mass #:velocity) '(2 1) '())
  (keyword-apply/sort kinetic-energy '(#:velocity #:mass) '(1 2) '())
]}
