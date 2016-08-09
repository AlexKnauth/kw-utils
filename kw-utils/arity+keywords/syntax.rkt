#lang racket/base

(provide kw-formals->arity)

(require racket/list
         syntax/parse
         "../arity+keywords.rkt")
(module+ test
  (require rackunit))

;; kw-formals->arity : Syntax -> Arity+Keywords
(define (kw-formals->arity stx)
  (syntax-parse stx
    [((~or arg1:id
           (~seq kw2:keyword arg2:id)
           (~seq kw3:keyword [arg3:id default3:expr]))
      ...
      (~or [arg4:id default4:expr]
           (~seq kw5:keyword arg5:id)
           (~seq kw6:keyword [arg6:id default6:expr]))
      ...)
     (arity+keywords
      (range (length (syntax->list #'[arg1 ...]))
             (add1 (length (syntax->list #'[arg1 ... arg4 ...]))))
      (syntax->datum #'[kw2 ... kw5 ...])
      (syntax->datum #'[kw2 ... kw5 ... kw3 ... kw6 ...]))]
    [((~or arg1:id
           (~seq kw2:keyword arg2:id)
           (~seq kw3:keyword [arg3:id default3:expr]))
      ...
      (~or [arg4:id default4:expr]
           (~seq kw5:keyword arg5:id)
           (~seq kw6:keyword [arg6:id default6:expr]))
      ...
      . rst:id)
     (arity+keywords
      (arity-at-least (length (syntax->list #'[arg1 ...])))
      (syntax->datum #'[kw2 ... kw5 ...])
      (syntax->datum #'[kw2 ... kw5 ... kw3 ... kw6 ...]))]
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  ;; without keywords
  (check-equal? (kw-formals->arity #'()) (arity+keywords 0 '() '()))
  (check-equal? (kw-formals->arity #'(a)) (arity+keywords 1 '() '()))
  (check-equal? (kw-formals->arity #'(a b c d e)) (arity+keywords 5 '() '()))
  (check-equal? (kw-formals->arity #'(a b [c 2])) (arity+keywords (list 2 3) '() '()))
  (check-equal? (kw-formals->arity #'(a b [c 2] [d 3])) (arity+keywords (list 2 3 4) '() '()))
  (check-equal? (kw-formals->arity #'rst) (arity+keywords (arity-at-least 0) '() '()))
  (check-equal? (kw-formals->arity #'(a b . rst))
                (arity+keywords (arity-at-least 2) '() '()))
  (check-equal? (kw-formals->arity #'(a b [c 2] [d 3] . rst))
                (arity+keywords (arity-at-least 2) '() '()))
  ;; with keywords
  (check-equal? (kw-formals->arity #'(#:a a)) (arity+keywords 0 '(#:a) '(#:a)))
  (check-equal? (kw-formals->arity #'(#:a [a 0])) (arity+keywords 0 '() '(#:a)))
  (check-equal? (kw-formals->arity #'(a #:b b)) (arity+keywords 1 '(#:b) '(#:b)))
  (check-equal? (kw-formals->arity #'(a #:b [b 1])) (arity+keywords 1 '() '(#:b)))
  (check-equal? (kw-formals->arity #'(a #:b b #:c [c 2] d e))
                (arity+keywords 3 '(#:b) '(#:b #:c)))
  (check-equal? (kw-formals->arity #'(a #:b [b 1] c [d 3] #:e e))
                (arity+keywords (list 2 3) '(#:e) '(#:b #:e)))
  (check-equal? (kw-formals->arity #'(a #:b b c #:d [d 3] [e 2] #:f f [g 3]))
                (arity+keywords (list 2 3 4) '(#:b #:f) '(#:b #:d #:f)))
  (check-equal? (kw-formals->arity #'(#:a a . rst))
                (arity+keywords (arity-at-least 0) '(#:a) '(#:a)))
  (check-equal? (kw-formals->arity #'(a #:b [b 1] c #:d d . rst))
                (arity+keywords (arity-at-least 2) '(#:d) '(#:b #:d)))
  (check-equal? (kw-formals->arity #'(a #:b b c [d 2] [e 3] . rst))
                (arity+keywords (arity-at-least 2) '(#:b) '(#:b)))
  )

