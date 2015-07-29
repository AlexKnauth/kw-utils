#lang scribble/manual

@(require scribble/eval
          (for-label kw-utils/mapper
                     kw-utils/partial
                     kw-utils/kw-map
                     (except-in racket/base map)
                     racket/contract/base
                     racket/function
                     ))

@title{Creating functions that map over lists}

@defmodule[kw-utils/mapper]

@defproc*[([(mapper [f procedure?] [arg any/c] ... [#:<kw> kw-arg any/c] ...) procedure?]
           [(mapper [#:<kw> kw-arg any/c] ...) procedure?])]{
The one-argument case is equivalent to a curried version of @racket[map].

@racket[(mapper f)] is equivalent to @racket[(partial map f)], and
@racket[(mapper arg ...)] is equivalent to
@racket[(partial map (partial arg ...))].
@examples[
  (require kw-utils/mapper)
  ((mapper add1) '(1 2 3))
  ((mapper +) '(1 2 3) '(4 5 6))
  ((mapper + 3) '(1 2 3))
  ((mapper + 3) '(1 2 3) '(4 5 6))
]}

