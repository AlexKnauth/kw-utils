#lang scribble/manual

@(require scribble/eval
          (for-label kw-utils/lambda-kwrest
                     racket/base))

@title[#:tag "lambda-kwrest.scrbl"]{lambda/kwrest}

@defmodule[kw-utils/lambda-kwrest]

@defform[
  (lambda/kwrest (single-param ...) rest-param ... body ...)
  #:grammar ([single-param id
                           [id default-expr]
                           (code:line keyword id)
                           (code:line keyword [id default-expr])]
             [rest-param (code:line #:rest id)
                         (code:line #:kwrest id)])]{
Produces a procedure that may accept all keywords if
@racket[#:kwrest] is provided.

@examples[
  (require kw-utils/lambda-kwrest)
  (define proc
    (lambda/kwrest () #:rest positional-rest #:kwrest keyword-rest
      (list positional-rest keyword-rest)))
  (proc #:a 'a #:b 'b 0 1 2)
]}

@defform[
  (case-lambda/kwrest [(single-param ...) rest-param ... body ...] ...)
  #:grammar ([single-param id
                           [id default-expr]
                           (code:line keyword id)
                           (code:line keyword [id default-expr])]
             [rest-param (code:line #:rest id)
                         (code:line #:kwrest id)])]{
Produces a procedure that may accept all keywords if
@racket[#:kwrest] is provided.

@examples[
  (require kw-utils/lambda-kwrest)
  (define proc
    (case-lambda/kwrest
      [() #:rest positional-rest #:kwrest keyword-rest
       (list positional-rest keyword-rest)]))
  (proc #:a 'a #:b 'b 0 1 2)
]}

