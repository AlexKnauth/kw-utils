#lang scribble/manual

@(require scribble/eval
          (for-label kw-utils/kw-map
                     racket/math))

@(module original-ids racket/base
   (require scribble/manual
            (for-label racket/base))
   (provide (all-defined-out))
   (define rkt:map @racket[map]))
@(require 'original-ids)

@title[#:tag "kw-map.scrbl"]{Map with keyword arguments}

@defmodule[kw-utils/kw-map]

@defproc[(map [proc procedure?] [lst list?] ... [#:<kw> lst2 list?] ...) list?]{
like @rkt:map from @racketmodname[racket/base], but accepts keyword arguments as
well as positional arguments.

@examples[
  (require kw-utils/kw-map racket/math)
  (map (λ (x) (+ x 1)) '(1 2 3 4))
  (map (λ (#:x x) (+ x 1)) #:x '(1 2 3 4))
  (map (λ (x y) (+ x y)) '(1 2 3 4) '(10 100 1000 10000))
  (map (λ (x #:y y) (+ x y)) '(1 2 3 4) #:y '(10 100 1000 10000))
  (define (KE #:m m #:v v)
    (* 1/2 m (sqr v)))
  (map KE #:m '(2 2 2 2) #:v '(0 1 2 3))
  (map KE #:m '(0 1 2 3) #:v '(0 1 2 3))
  (map KE #:m '(1 2 1/2 2/9) #:v '(0 1 2 3))
]}
