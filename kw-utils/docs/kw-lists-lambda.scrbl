#lang scribble/manual

@(require racket/base
          scribble/eval
          (for-label kw-utils/kw-lists-lambda
                     kw-utils/keyword-lambda
                     racket/base))

@title[#:tag "kw-lists-lambda.scrbl"]{kw-lists-lambda}

@defmodule[kw-utils/kw-lists-lambda]

@defform[(kw-lists-lambda kws kw-args args body ...)
         #:grammar ([args (arg ...)
                          (arg ... . rest-id)
                          rest-id]
                    [arg arg-id
                         [arg-id default-expr]])]{
roughly equivalent to
@(racketblock
  (make-keyword-procedure
   (lambda (kws kw-args . args) body ...))
  )

@examples[
  (require kw-utils/kw-lists-lambda)
  (define proc
    (kw-lists-lambda kws kw-args rest-args
      (list kws kw-args rest-args)))
  (proc #:a 'a #:b 'b 0 1 2)
]}

@defform[(kw-lists-case-lambda kws kw-args [args body ...] ...)
         #:grammar ([args (arg-id ...)
                          (arg-id ... . rest-id)
                          rest-id])]{
roughly equivalent to
@(racketblock
  (make-keyword-procedure
   (case-lambda [(kws kw-args . args) body ...] ...))
  )

@examples[
  (require kw-utils/kw-lists-lambda)
  (define proc
    (kw-lists-case-lambda kws kw-args
      [(a)
       (list kws kw-args a)]
      [(a b)
       (list kws kw-args a b)]))
  (proc #:a 'a #:b 'b 0)
  (proc #:a 'a #:b 'b 0 1)
]}

