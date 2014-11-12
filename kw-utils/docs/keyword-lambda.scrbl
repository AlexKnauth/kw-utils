#lang scribble/manual
@(require kw-utils/keyword-lambda
          racket/base
          scribble/eval
          (for-label kw-utils/keyword-lambda
                     racket/base))

@title[#:tag "keyword-lambda.scrbl"]{keyword-lambda}

@defmodule[kw-utils/keyword-lambda]

@defform[(keyword-lambda (kws kw-args . rest-args) body ...)]{
roughly equivalent to
@(racketblock
  (make-keyword-procedure
   (lambda (kws kw-args . rest-args) body ...))
  )

@examples[
  (require kw-utils/keyword-lambda)
  (define proc
    (keyword-lambda (kws kw-args . rest-args)
      (list kws kw-args rest-args)))
  (proc #:a 'a #:b 'b 0 1 2)
]}

