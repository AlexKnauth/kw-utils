#lang scribble/manual

@(require scribble/eval
          (for-label kw-utils/partial
                     racket/base
                     racket/contract/base
                     racket/function
                     ))

@title{Partial application with keywords}

@defmodule[kw-utils/partial]

@defproc*[([(partial [f procedure?] [arg any/c] ... [#:<kw> kw-arg any/c] ...) procedure?]
           [(partial [#:<kw> kw-arg any/c] ...) procedure?])]{
Similar to @racket[curry], but better thought of as a generalization of
@racketidfont{partial} from @racketmodname[rackjure]. Unlike @racket[curry],
(but like the @racketmodname[rackjure] version), it does not care about function
arity, and so has much simpler behavior, so that
@racket[((partial f in-arg ...) out-arg ...)] is equivalent to
@racket[(f in-arg ... out-arg ...)] no matter what arity @racket[f] has.
It also generalizes this idea to include keyword arguments.
@examples[
  (require kw-utils/partial)
  ((partial + 1 2))
  ((partial + 1) 2)
  ((partial +) 1 2)
  ((partial) + 1 2)
  (define (KE #:m m #:v v)
    (* 1/2 m v v))
  ((partial KE) #:m 2 #:v 1)
  ((partial KE #:m 2) #:v 1)
  ((partial KE #:m 2 #:v 1))
  ((partial) KE #:m 2 #:v 1)
  ((partial #:m 2) KE #:v 1)
]}

@defproc[(app [f procedure?] [arg any/c] ... [#:<kw> kw-arg any/c] ...) any]{
A procedure for normal function application.
@racket[(partial #:<kw> kw-arg ... ...)] is equivalent to
@racket[(partial app #:<kw> kw-arg ... ...)].
@examples[
  (require kw-utils/partial)
  (+ 1 2)
  (app + 1 2)
  (define (KE #:m m #:v v)
    (* 1/2 m v v))
  (KE #:m 2 #:v 1)
  (app KE #:m 2 #:v 1)
]}

