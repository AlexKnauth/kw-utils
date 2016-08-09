#lang scribble/manual
@(require kw-utils/arity+keywords
          racket/base
          scribble/eval
          (for-label kw-utils/arity+keywords
                     kw-utils/arity+keywords/syntax
                     racket/base
                     racket/contract/base
                     racket/function
                     ))

@title[#:tag "arity+keywords.scrbl"]{arity+keywords}

@section{Procedure arities with keywords}

@defmodule[kw-utils/arity+keywords]

@defstruct*[arity+keywords ([arity procedure-arity?]
                            [required-kws (listof keyword?)]
                            [allowed-kws (or/c (listof keyword?) #f)])
                           #:transparent]{
represents a procedure's arity including the keywords required and keywords allowed.

The @racket[arity] field represents the arity produced by @racket[procedure-arity].

The next 2 fields (@racket[required-kws] and @racket[allowed-kws]) represent the 2 values produced by
@racket[procedure-keywords].  

A @racket[#f] value for @racket[allowed-kws] means that it accepts all keywords.

The guard procedure also sorts the keyword lists for you.
}

@defproc[(procedure-arity+keywords [proc procedure?]) arity+keywords?]{
returns an @racket[arity+keywords] instance representing the arity and keyword-arity of @racket[proc].

It is defined like this:
@(racketblock
  (define (procedure-arity+keywords proc)
    (define arity (procedure-arity proc))
    (define-values (req-kws allowed-kws)
      (procedure-keywords proc))
    (arity+keywords arity req-kws allowed-kws)))

@examples[
  (require kw-utils/arity+keywords)
  (define proc (make-keyword-procedure void))
  (procedure-arity+keywords proc)
  (procedure-arity+keywords (procedure-reduce-arity proc 5))
  (procedure-arity+keywords
   (procedure-reduce-keyword-arity/sort proc 3 '(#:kw #:other-kw) '(#:kw #:other-kw #:optional-kw)))
]}

@defproc[(procedure-reduce-arity+keywords [proc procedure?] [arity+kws arity+keywords?]) procedure?]{
like @racket[procedure-reduce-arity], except that it accepts an @racket[arity+keywords] and handles
the keyword-arity as well.  

It is defined like this:
@(racketblock
  (define (procedure-reduce-arity+keywords proc a)
    (procedure-reduce-keyword-arity
     proc
     (arity+keywords-arity a)
     (arity+keywords-required-kws a)
     (arity+keywords-allowed-kws a))))

@examples[
  (require kw-utils/arity+keywords)
  (define proc (make-keyword-procedure void))
  (procedure-arity proc)
  (procedure-keywords proc)
  (define proc-with-arity
    (procedure-reduce-arity+keywords
     proc
     (arity+keywords 5 '(#:kw #:other-kw) '(#:kw #:other-kw #:optional-kw))))
  (procedure-arity proc-with-arity)
  (procedure-keywords proc-with-arity)
]}

@defproc[(procedure-reduce-keyword-arity/sort [proc procedure?]
                                              [arity procedure-arity?]
                                              [required-kws (listof keyword?)]
                                              [allowed-kws (or/c (listof keyword?) #f)])
         procedure?]{
like @racket[procedure-reduce-keyword-arity], but without the constraint that the keywords in
@racket[required-kws] or @racket[allowed-kws] must be sorted.

It is equivalent to
@racket[(procedure-reduce-arity+keywords proc (arity+keywords arity required-kws allowed-kws))].
}

@defproc[(arity+keywords-matches? [arity+kws arity+keywords?]
                                  [n natural-number/c]
                                  [kws (listof keyword?)])
         boolean?]{
determines whether the given @racket[arity+kws] accepts the @racket[n] by-position arguments and the
keywords in @racket[kws].
}

@defproc[(procedure-arity+keywords-matches? [proc procedure?]
                                            [n natural-number/c]
                                            [kws (listof keyword?)])
         boolean?]{
equivalent to @racket[(arity+keywords-matches? (procedure-arity+keywords proc) n kws)].
}

@defproc[(procedure-arity+keywords-matches?/c [n natural-number/c]
                                              [kws (listof keyword?)])
         flat-contract?]{
produces a flat contract (also a predicate) that accepts procedures that accept @racket[n] by-position
arguments and accepts the keywords in @racket[kws].  
}

@defproc[(arity+keywords-includes? [a1 arity+keywords?] [a2 arity+keywords?]) boolean?]{
like @racket[arity-includes?], but for @racket[arity+keywords] instances.  But most of the time when
when you would use @racket[arity-includes?], you really want @racket[arity+keywords-matches?].
}

@defproc[(arity+keywords-combine/or [arity+kws arity+keywords?] ...) arity+keywords?]{
combines the @racket[arity+kws]es into one @racket[arity+keywords] instance in an or-like way.  

@examples[
  (require kw-utils/arity+keywords)
  (arity+keywords-combine/or (arity+keywords 1 '(#:a)     '(#:a #:b #:c))
                             (arity+keywords 2 '(#:a #:b) '(#:a #:b #:d)))
]}

@defproc[(arity+keywords-combine/and [arity+kws arity+keywords?] ...) arity+keywords?]{
combines the @racket[arity+kws]es into one @racket[arity+keywords] instance in an and-like way.  

@examples[
  (require kw-utils/arity+keywords)
  (arity+keywords-combine/and (arity+keywords '(1 2) '(#:a) '(#:a #:b #:c #:d))
                              (arity+keywords '(2 3) '(#:b) '(#:a #:b #:c #:e)))
]}

@section{Getting the arity from function arguments syntax}

@defmodule[kw-utils/arity+keywords/syntax]

@defproc[(kw-formals->arity+keywords [fmls-stx syntax?]) arity+keywords?]{
Given the @racket[args] in @racket[(lambda args body)], returns an
@racket[arity+keywords] value representing the arity and keywords of
the function.

@examples[
  (require kw-utils/arity+keywords/syntax)
  (kw-formals->arity+keywords #'())
  (kw-formals->arity+keywords #'(a b c))
  (kw-formals->arity+keywords #'(a b [c 2]))
  (kw-formals->arity+keywords #'(a b . rst))
  (kw-formals->arity+keywords #'(a #:b b))
  (kw-formals->arity+keywords #'(a #:b [b 1]))
  (kw-formals->arity+keywords #'(#:a a . rst))
]}

@deftogether[[
  @defproc[(kw-formals->arity [fmls-stx syntax?]) normalized-arity?]
  @defproc[(kw-formals->required-kws [fmls-stx syntax?]) (listof keyword?)]
  @defproc[(kw-formals->allowed-kws [fmls-stx syntax?]) (listof keyword?)]
]]{
Like @racket[kw-formals->arity+keywords], but just for the positional-argument
arity, required keywords, and allowed keywords.
}

