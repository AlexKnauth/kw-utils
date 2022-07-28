#lang scribble/manual
@(require scribble/eval
          (for-label kw-utils/kw-hash
                     kw-utils/kw-hash-lambda
                     kw-utils/kw-hash/contract
                     kw-utils/kw-lists-lambda
                     racket/base
                     racket/contract/base
                     racket/math
                     ))

@title{Grouping All Keywords into a Hash Table}

@section{kw-hash-lambda}

@defmodule[kw-utils/kw-hash-lambda]

@defform[(kw-hash-lambda formals #:kws kw-hash-id body-expr ...+)]{
roughly equivalent to
@(racketblock
  (kw-lists-lambda kws kw-args formals
    (let ([kw-hash-id (keyword-apply make-kw-hash kws kw-args '())])
      body ...)))

@examples[
  (require kw-utils/kw-hash-lambda)
  (define proc
    (kw-hash-lambda rest-args #:kws kw-hash
      (list rest-args kw-hash)))
  (proc 0 1 2 #:a 'a #:b 'b)
]}

@defform[(kw-hash-case-lambda #:kws kw-hash-id [formals body-expr ...+] ...)]{
roughly equivalent to
@(racketblock
  (kw-lists-case-lambda kws kw-args
    [formals
     (let ([kw-hash-id (keyword-apply make-kw-hash kws kw-args '())])
       body ...)]
    ...))

@examples[
  (require kw-utils/kw-hash-lambda)
  (define proc
    (kw-hash-case-lambda #:kws kw-hash
      [(a)
       (list a kw-hash)]
      [(a b)
       (list a b kw-hash)]))
  (proc 0 #:a 'a #:b 'b)
  (proc 0 1 #:a 'a #:b 'b)
]}

@section{Applying functions with keywords from a hash}

@defmodule[kw-utils/kw-hash]

@defproc[(apply/kw-hash [proc procedure?] [kw-hash (hash/c keyword? any/c)] [v any/c] ... [lst list?])
         any]{
like @racket[keyword-apply], but instead of taking the keywords and keyword
arguments as separate lists, @racket[apply/kw-hash] takes them in a hash-table.

Based on @url["https://gist.github.com/Metaxal/578b473bc48886f81123"].

@examples[
  (require kw-utils/kw-hash racket/math)
  (define (kinetic-energy #:m m #:v v)
    (* 1/2 m (sqr v)))
  (apply/kw-hash kinetic-energy (hash '#:m 2 '#:v 1) '())
]}

@defproc[(app/kw-hash [proc procedure?] [kw-hash (hash/c keyword? any/c)] [v any/c] ...)
         any]{
like @racket[apply/kw-hash], but doesn't take a list argument at the end.
}

@defproc[(make-kw-hash [#:<kw> kw-arg any/c] ...) (hash/c keyword? any/c)]{
returns a hash-table containing the given keyword arguments.
}

@section{Function contracts with kw-hash}

@defmodule[kw-utils/kw-hash/contract]

@defform*[#:literals (any)
          [(kw-hash-> [arg/c ...] #:kws kw-hash/c any)
           (kw-hash-> [arg/c ...] #:rest rest/c #:kws kw-hash/c any)]]{
Produces a contract for functions that can accept arbitrary keyword arguments.
The contract puts the keywords in a hash table and checks that against the
@racket[kw-hash/c] contract.
}

