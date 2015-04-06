#lang racket/base

(provide (struct-out arity+keywords)
         empty-arity+keywords
         any-arity+keywords
         procedure-arity+keywords
         procedure-reduce-arity+keywords
         procedure-reduce-keyword-arity/sort
         arity+keywords-matches?
         procedure-arity+keywords-matches?
         procedure-arity+keywords-matches?/c
         procedure-required-keywords
         procedure-allowed-keywords
         arity+keywords-combine/or  arity-combine/or  kws-combine/or
         arity+keywords-combine/and arity-combine/and kws-combine/and
         arity+keywords-combine
         arity+keywords-add      arity-add
         arity+keywords-subtract arity-subtract
         arity-map
         kws-sort
         )

(require racket/function
         racket/bool
         racket/contract/base
         racket/list
         racket/match
         (for-syntax racket/base
                     ))

(module+ test
  (require rackunit racket/local racket/math))

(define (arity+keywords-guard arity required-kws allowed-kws _)
  (unless (procedure-arity? arity)
    (error 'arity+keywords "expected procedure-arity? for first argument, given ~v" arity))
  (unless ((listof keyword?) required-kws)
    (error 'arity+keywords
           "expcetd (listof keyword?) for second argument, given ~v"
           required-kws))
  (unless ((or/c (listof keyword?) #t #f) allowed-kws)
    (error 'arity+keywords
           "expcetd (or/c (listof keyword?) #f) for third argument, given ~v"
           required-kws))
  (define new-arity (normalize-arity arity))
  (define new-required-kws
    (cond [(empty? new-arity) '()]
          [else (kws-sort required-kws)]))
  (define new-allowed-kws
    (cond [(empty? new-arity) '()]
          [(list? allowed-kws)
           (kws-combine/or new-required-kws allowed-kws)]
          [else #f]))
  (values new-arity new-required-kws new-allowed-kws))

;; arity+keywords : Procedure-Arity (Listof Keyword) (or/c (Listof Keyword) #f) -> Arity+Keywords
(struct arity+keywords (arity required-kws allowed-kws) #:transparent
  #:guard arity+keywords-guard)


;; procedure-arity+keywords : Procedure -> Arity+Keywords
(define (procedure-arity+keywords proc)
  (define arity (procedure-arity proc))
  (define-values (req-kws allowed-kws)
    (procedure-keywords proc))
  (arity+keywords arity req-kws allowed-kws))

(define (procedure-required-keywords proc)
  (arity+keywords-required-kws (procedure-arity+keywords proc)))

(define (procedure-allowed-keywords proc)
  (arity+keywords-allowed-kws (procedure-arity+keywords proc)))

;; proceudre-reduce-arity+keywords : Procedure Arity+Keywords -> Procedure
(define (procedure-reduce-arity+keywords proc a)
  (match-define (arity+keywords arity required-kws allowed-kws) a)
  (procedure-reduce-keyword-arity
   proc
   arity
   required-kws
   allowed-kws))

;; like procedure-reduce-keyword-arity, but without the constraint that the kws must be sorted
(define (procedure-reduce-keyword-arity/sort proc arity required-kws allowed-kws)
  (procedure-reduce-arity+keywords
   proc
   (arity+keywords arity required-kws allowed-kws)))

;; arity+keywords-matches? : Arity+Keywords Natural (Listof Keyword) -> Boolean
;; see also arity+keywords-includes?
(define (arity+keywords-matches? arity+kws n kws)
  (match-define (arity+keywords arity required-kws allowed-kws) arity+kws)
  (and (arity-includes? arity n)
       (or (false? allowed-kws)
           (for/and ([kw (in-list kws)])
             (member kw allowed-kws)))
       (for/and ([required-kw (in-list required-kws)])
         (member required-kw kws))
       #t))

;; procedure-arity+keywords-matches? : Procedure Natural (Listof Keyword) -> Boolean
(define (procedure-arity+keywords-matches? proc n kws)
  (arity+keywords-matches? (procedure-arity+keywords proc) n kws))

;; procedure-arity+keywords-matches?/c : Natural (Listof Keyword) -> (Procedure -> Boolean)
(define (procedure-arity+keywords-matches?/c n kws)
  (flat-named-contract
   `(procedure-arity+keywords-matches?/c ,n (quote ,kws))
   (lambda (proc)
     (procedure-arity+keywords-matches? proc n kws))))

;; arity+keywords-includes? : Arity+Keywords Arity+Keywords -> Boolean
;; see also arity+keywords-matches?
(define (arity+keywords-includes? a1 a2)
  (match-define (arity+keywords a1.arity a1.req-kws a1.allowed-kws) a1)
  (match-define (arity+keywords a2.arity a2.req-kws a2.allowed-kws) a2)
  (and (arity-includes? a1.arity a2.arity)
       (for/and ([a1-kw (in-list a1.req-kws)])
         (member a1-kw a2.req-kws))
       (cond [(false? a1.allowed-kws) #t]
             [(false? a2.allowed-kws) #f]
             [else (for/and ([a2-kw (in-list a2.allowed-kws)])
                     (member a2-kw a1.allowed-kws))])
       #t))

;; arity+keywords-combine/or : Arity+Keywords ... -> Arity+Keywords
(define arity+keywords-combine/or
  (case-lambda
    [() empty-arity+keywords]
    [(a) a]
    [(a1 a2) (match-define (arity+keywords a1.arity a1.required-kws a1.allowed-kws) a1)
             (match-define (arity+keywords a2.arity a2.required-kws a2.allowed-kws) a2)
             (cond
               [(andmap empty? (list a1.arity a2.arity)) empty-arity+keywords]
               [(empty? a1.arity) a2]
               [(empty? a2.arity) a1]
               [else
                (define arity
                  (arity-combine/or a1.arity a2.arity))
                (define required-kws
                  (kws-combine/and a1.required-kws a2.required-kws))
                (define allowed-kws
                  (kws-combine/or a1.allowed-kws a2.allowed-kws))
                (arity+keywords arity required-kws allowed-kws)])]
    [(a1 . rest-args) (arity+keywords-combine/or a1 (apply arity+keywords-combine/or rest-args))]
    ))

(define (arity+keywords-combine-warning)
  (with-handlers ([exn:fail? (λ (e) ((error-display-handler) (exn-message e) e))])
    (error 'arity+keywords-combine
           (string-append "please use arity+keywords-combine/or instead" "\n"
                          "  (to avoid confusion with arity+keywords-combine/and)"))))
(define-syntax arity+keywords-combine
  (lambda (stx)
    (with-handlers ([exn:fail:syntax? (λ (e) ((error-display-handler) (exn-message e) e))])
      (raise-syntax-error #f
                          (string-append "please use arity+keywords-combine/or instead" "\n"
                                         "  (to avoid confusion with arity+keywords-combine/and)")
                          stx))
    (syntax-case stx ()
      [(arity+keywords-combine . stuff)
       (quasisyntax/loc stx
         (begin
           #,(syntax/loc stx (arity+keywords-combine-warning))
           #,(syntax/loc stx (arity+keywords-combine/or . stuff))))]
      [arity+keywords-combine
       (quasisyntax/loc stx
         (begin
           #,(syntax/loc stx (arity+keywords-combine-warning))
           #,(quasisyntax/loc stx
               (λ args #,(syntax/loc stx (arity+keywords-combine-warning))
                 (apply arity+keywords-combine/or args)))))])))

;; arity+keywords-combine/and : Arity+Keywords ... -> Arity+Keywords
(define arity+keywords-combine/and
  (case-lambda
    [() any-arity+keywords]
    [(a) a]
    [(a1 a2) (match-define (arity+keywords a1.arity a1.required-kws a1.allowed-kws) a1)
             (match-define (arity+keywords a2.arity a2.required-kws a2.allowed-kws) a2)
             (define arity
               (arity-combine/and a1.arity a2.arity))
             (define required-kws
               (kws-combine/or a1.required-kws a2.required-kws))
             (define allowed-kws
               (kws-combine/and a1.allowed-kws a2.allowed-kws))
             (cond [(for/and ([req-kw (in-list required-kws)])
                      (member req-kw allowed-kws))
                    (arity+keywords arity required-kws allowed-kws)]
                   [else empty-arity+keywords])]
    [(a1 . rest-args) (arity+keywords-combine/and a1 (apply arity+keywords-combine/and rest-args))]
    ))

;; arity-combine/or : Procedure-Arity ... -> Procedure-Arity
(define (arity-combine/or . args)
  (normalize-arity (flatten args)))

;; arity-combine/and : Procedure-Arity Procedure-Arity -> Procedure-Arity
(define (arity-combine/and a1 a2)
  (let ([a1 (normalize-arity a1)]
        [a2 (normalize-arity a2)])
    (cond [(arity-includes? a1 a2) a2]
          [(arity-includes? a2 a1) a1]
          [(number? a1)
           (cond [(arity-includes? a2 a1) a1]
                 [else '()])]
          [(number? a2)
           (cond [(arity-includes? a1 a2) a2]
                 [else '()])]
          [(arity-at-least? a1)
           (cond [(arity-includes? a2 a1) a1]
                 [(number? a2) '()]
                 [(arity-at-least? a2)
                  (arity-at-least (max (arity-at-least-value a1)
                                       (arity-at-least-value a2)))]
                 [(list? a2)
                  (normalize-arity
                   (flatten
                    (for/list ([n (in-list a2)])
                      (arity-combine/and a1 n))))]
                 [else (error 'arity-combine/and "this should never happen")])]
          [(arity-at-least? a2)
           (cond [(arity-includes? a1 a2) a2]
                 [(number? a1) '()]
                 [(arity-at-least? a1)
                  (arity-at-least (max (arity-at-least-value a1)
                                       (arity-at-least-value a2)))]
                 [(list? a1)
                  (normalize-arity
                   (flatten
                    (for/list ([n (in-list a1)])
                      (arity-combine/and a2 n))))]
                 [else (error 'arity-combine/and "this should never happen")])]
          [(list? a1)
           (normalize-arity
            (flatten
             (for/list ([n (in-list a1)])
               (arity-combine/and a2 n))))]
          [(list? a2)
           (normalize-arity
            (flatten
             (for/list ([n (in-list a2)])
               (arity-combine/and a1 n))))]
          [else (error 'arity-combine/and "this should never happen")])))

;; kws-combine/or
;; note that this combines the allowed keywords in an or-like way.
;; for the required keywords, arity+keywords-combine/or actually uses kws-append/and
(define (kws-combine/or . args)
  (cond [(empty? args) '()]
        [(ormap false? args) #f]
        [else
         (kws-sort (apply append args))]))

(define kws-combine/and
  (case-lambda
    [() #f]
    [(a) (kws-sort a)]
    [(a b)
     (kws-sort
      (cond [(false? a) b]
            [(false? b) a]
            [else
             (for*/list ([a-kw (in-list a)]
                         [b-kw (in-list b)]
                         #:when (equal? a-kw b-kw))
               a-kw)]))]
    [(a b . rst)
     (apply kws-combine/and (kws-combine/and a b) rst)]))

(define (kws-sort kws)
  (cond [(false? kws) #f]
        [else (sort (remove-duplicates kws) keyword<?)]))
  


;; arity+keywords-add : Arity+Keywords Natural (Listof Keyword) (Listof Keyword) -> Arity+Keywords
(define (arity+keywords-add a n req-kws allwd-kws)
  (match-define (arity+keywords a.arity a.req-kws a.allowed-kws) a)
  (define arity (arity-add a.arity n))
  (define required-kws (kws-combine/or a.req-kws req-kws))
  (define allowed-kws (kws-combine/or a.allowed-kws req-kws allwd-kws))
  (arity+keywords arity req-kws allowed-kws))

;; arity+keywords-subtract : Arity+Keywords Natural (Listof Keyword) -> Arity+Keywords
(define (arity+keywords-subtract a n kws)
  (match-define (arity+keywords a.arity a.req-kws a.allowed-kws) a)
  (define arity (arity-subtract a.arity n))
  (cond [(empty? arity) empty-arity+keywords]
        [(not (list? a.allowed-kws))
         (define req-kws
           (remove* kws a.req-kws))
         (define allowed-kws #f)
         (arity+keywords arity req-kws allowed-kws)]
        [(not (for/and ([kw (in-list kws)])
                (member kw a.allowed-kws)))
         empty-arity+keywords]
        [else
         (define req-kws
           (remove* kws a.req-kws))
         (define allowed-kws
           (remove* kws a.allowed-kws))
         (arity+keywords arity req-kws allowed-kws)]))

;; arity-add : Procedure-Arity Integer -> Procedure-Arity
;; n could be negative
(define (arity-add arity n)
  (arity-map
   (λ (a)
     (arity-add/simple a n))
   arity))

;; arity-subtract : Procedure-Arity Natural -> Procedure-Arity
(define (arity-subtract arity n)
  (arity-add arity (- n)))

;; arity-add/simple : [(U Natural (arity-at-least Natural)) Integer -> Procedure-Arity]
;; n could be negative
(define (arity-add/simple a n)
  (cond [(number? a)
         (define new-a (+ a n))
         (cond [(negative? new-a) '()]
               [else new-a])]
        [(arity-at-least? a)
         (define a.n (arity-at-least-value a))
         (define new-a.n (+ a.n n))
         (cond [(negative? new-a.n) (arity-at-least 0)]
               [else (arity-at-least new-a.n)])]
        [else (error 'arity-add/simple "this should never happen")]))

;;(define-type Nat/Aal->Ar [(U Natural (arity-at-least Natural)) -> Procedure-Arity])

;; arity-map : [Nat/Aal->Ar Procedure-Arity -> Procedure-Arity]
(define (arity-map proc arity)
  (let ([arity (normalize-arity arity)])
    (cond [(number? arity) (normalize-arity (proc arity))]
          [(arity-at-least? arity) (normalize-arity (proc arity))]
          [(list? arity) (normalize-arity (flatten (map proc arity)))]
          [else (error 'arity-map "this should never happen, given ~v" arity)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define empty-arity+keywords (arity+keywords '() '() '()))
(define any-arity+keywords (arity+keywords (arity-at-least 0) '() #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (test-case "the arity+keywords constructor and guard"
    (check-equal? (arity+keywords (list 1 (arity-at-least 3) 2) '() #t)
                  (arity+keywords (arity-at-least 1) '() #f))
    (check-equal? (arity+keywords '(1) '(#:a #:b) '(#:c))
                  (arity+keywords 1 '(#:a #:b) '(#:a #:b #:c))))
  
  (test-case "procedure-arity+keywords and procedure-reduce-arity+keywords"
    (define proc (make-keyword-procedure void))
    (check-equal? (procedure-arity+keywords proc)
                  (arity+keywords (arity-at-least 0) '() #f))
    (check-equal? (procedure-arity+keywords (procedure-reduce-arity proc 5))
                  (arity+keywords 5 '() '()))
    (define proc-with-arity
      (procedure-reduce-arity+keywords
       proc
       (arity+keywords 3 '(#:kw #:other-kw) '(#:kw #:other-kw #:optional-kw))))
    (check-equal? (procedure-arity+keywords proc-with-arity)
                  (arity+keywords 3 '(#:kw #:other-kw) '(#:kw #:other-kw #:optional-kw)))
    (check-equal? (procedure-arity proc-with-arity) 3)
    (check-equal? (call-with-values (λ () (procedure-keywords proc-with-arity)) list)
                  (list '(#:kw #:other-kw) '(#:kw #:optional-kw #:other-kw))))
  
  (test-case "arity+keywords-matches?"
    (check-true  (arity+keywords-matches? (arity+keywords 0 '() '()) 0 '()))
    (check-false (arity+keywords-matches? (arity+keywords 0 '() '()) 1 '()))
    (check-false (arity+keywords-matches? (arity+keywords '() '() #f) 0 '()))
    (check-true  (arity+keywords-matches? (arity+keywords (list 2 (arity-at-least 5))
                                                          '() '())
                                          2 '()))
    (check-false (arity+keywords-matches? (arity+keywords (list 2 (arity-at-least 5))
                                                          '() '())
                                          3 '()))
    (check-true  (arity+keywords-matches? (arity+keywords (list 2 (arity-at-least 5))
                                                          '() '())
                                          5 '()))
    (check-true  (arity+keywords-matches? (arity+keywords 0 '(#:a #:b) '(#:a #:b #:c))
                                          0 '(#:a #:b)))
    (check-true  (arity+keywords-matches? (arity+keywords 0 '(#:a #:b) '(#:a #:b #:c))
                                          0 '(#:a #:b #:c)))
    (check-false (arity+keywords-matches? (arity+keywords 0 '(#:a #:b) '(#:a #:b #:c))
                                          0 '(#:a #:c)))
    (check-true  (arity+keywords-matches? (arity+keywords 0 '() #f)
                                          0 '(#:whatever))))
  
  (test-case "arity+keywords-includes?"
    (check-true  (arity+keywords-includes? (arity+keywords 1 '() '())
                                           (arity+keywords 1 '() '())))
    (check-true  (arity+keywords-includes? (arity+keywords '(1 2) '() '())
                                           (arity+keywords 1 '() '())))
    (check-false (arity+keywords-includes? (arity+keywords 1 '() '())
                                           (arity+keywords '(1 2) '() '())))
    (check-true  (arity+keywords-includes? (arity+keywords 0 '() #f)
                                           (arity+keywords 0 '() '(#:a))))
    (check-true  (arity+keywords-includes? (arity+keywords 0 '() #f)
                                           (arity+keywords 0 '(#:a) '(#:a))))
    (check-false (arity+keywords-includes? (arity+keywords 0 '(#:a) #f)
                                           (arity+keywords 0 '() '(#:a))))
    (check-true  (arity+keywords-includes? (arity+keywords 0 '() '(#:a #:b))
                                           (arity+keywords 0 '(#:a) '(#:a))))
    (check-false (arity+keywords-includes? (arity+keywords 0 '() '())
                                           (arity+keywords 0 '() #f))))
  
  (test-case "arity+keywords-combine/or"
    (check-equal? (arity+keywords-combine/or) (arity+keywords '() '() '()))
    (check-equal? (arity+keywords-combine/or (arity+keywords '(4 9 16) '(#:a #:b) '(#:a #:b #:c)))
                  (arity+keywords '(4 9 16) '(#:a #:b) '(#:a #:b #:c)))
    (check-equal? (arity+keywords-combine/or (arity+keywords 1 '(#:a)     '(#:a #:b #:c))
                                             (arity+keywords 2 '(#:a #:b) '(#:a #:b #:d)))
                  (arity+keywords '(1 2) '(#:a) '(#:a #:b #:c #:d))))
  
  (test-case "arity+keywords-combine/and"
    (check-equal? (arity+keywords-combine/and) (arity+keywords (arity-at-least 0) '() #f))
    (check-equal? (arity+keywords-combine/and (arity+keywords '(4 9 16) '(#:a #:b) '(#:a #:b #:c)))
                  (arity+keywords '(4 9 16) '(#:a #:b) '(#:a #:b #:c)))
    (check-equal? (arity+keywords-combine/and (arity+keywords '(1 2) '(#:a) '(#:a #:b #:c #:d))
                                              (arity+keywords '(2 3) '(#:b) '(#:a #:b #:c #:e)))
                  (arity+keywords 2 '(#:a #:b) '(#:a #:b #:c)))
    (check-match  (arity+keywords-combine/and (arity+keywords 0 '(#:a) #f)
                                              (arity+keywords 0 '() '()))
                  (arity+keywords '() _ _)))
  
  (test-case "arity+keywords-subtract"
    (check-equal? (arity+keywords-subtract (arity+keywords 0 '() '()) 0 '())
                  (arity+keywords 0 '() '()))
    (check-equal? (arity+keywords-subtract (arity+keywords 1 '() '()) 0 '())
                  (arity+keywords 1 '() '()))
    (check-equal? (arity+keywords-subtract (arity+keywords 1 '() '()) 1 '())
                  (arity+keywords 0 '() '()))
    (check-equal? (arity+keywords-subtract (arity+keywords '(0 1) '() '()) 0 '())
                  (arity+keywords '(0 1) '() '()))
    (check-equal? (arity+keywords-subtract (arity+keywords '(0 1) '() '()) 1 '())
                  (arity+keywords 0 '() '()))
    (check-equal? (arity+keywords-subtract (arity+keywords '(3 7 10) '() '()) 5 '())
                  (arity+keywords '(2 5) '() '()))
    (check-equal? (arity+keywords-subtract (arity+keywords (arity-at-least 0) '() '()) 5 '())
                  (arity+keywords (arity-at-least 0) '() '()))
    (check-equal? (arity+keywords-subtract (arity+keywords (arity-at-least 49) '() '()) 57 '())
                  (arity+keywords (arity-at-least 0) '() '()))
    (check-equal? (arity+keywords-subtract (arity+keywords (arity-at-least 57) '() '()) 49 '())
                  (arity+keywords (arity-at-least 8) '() '()))
    (check-equal? (arity+keywords-subtract (arity+keywords (list 3 7 10 (arity-at-least 47)) '() '())
                                           5 '())
                  (arity+keywords (list 2 5 (arity-at-least 42)) '() '()))
    
    (check-equal? (arity+keywords-subtract (arity+keywords 0 '(#:a #:b) #f) 0 '())
                  (arity+keywords 0 '(#:a #:b) #f))
    (check-equal? (arity+keywords-subtract (arity+keywords 0 '(#:a #:b) #f) 0 '(#:a))
                  (arity+keywords 0 '(#:b) #f))
    (check-equal? (arity+keywords-subtract (arity+keywords 0 '(#:a #:b) #f) 0 '(#:b))
                  (arity+keywords 0 '(#:a) #f))
    (check-equal? (arity+keywords-subtract (arity+keywords 0 '(#:a #:b) #f) 0 '(#:a #:b))
                  (arity+keywords 0 '() #f))
    (check-equal? (arity+keywords-subtract (arity+keywords 0 '(#:a #:b) '(#:a #:b #:c #:d)) 0 '())
                  (arity+keywords 0 '(#:a #:b) '(#:a #:b #:c #:d)))
    (check-equal? (arity+keywords-subtract (arity+keywords 0 '(#:a #:b) '(#:a #:b #:c #:d)) 0 '(#:a))
                  (arity+keywords 0 '(#:b) '(#:b #:c #:d)))
    (check-equal? (arity+keywords-subtract (arity+keywords 0 '(#:a #:b) '(#:a #:b #:c #:d)) 0 '(#:b))
                  (arity+keywords 0 '(#:a) '(#:a #:c #:d)))
    (check-equal? (arity+keywords-subtract (arity+keywords 0 '(#:a #:b) '(#:a #:b #:c #:d)) 0 '(#:c))
                  (arity+keywords 0 '(#:a #:b) '(#:a #:b #:d)))
    (check-equal? (arity+keywords-subtract (arity+keywords 0 '(#:a #:b) '(#:a #:b #:c #:d)) 0 '(#:d))
                  (arity+keywords 0 '(#:a #:b) '(#:a #:b #:c)))
    (check-equal? (arity+keywords-subtract (arity+keywords 0 '(#:a #:b) '(#:a #:b #:c #:d))
                                           0 '(#:a #:b))
                  (arity+keywords 0 '() '(#:c #:d)))
    (check-equal? (arity+keywords-subtract (arity+keywords 0 '(#:a #:b) '(#:a #:b #:c #:d))
                                           0 '(#:a #:c))
                  (arity+keywords 0 '(#:b) '(#:b #:d)))
    (check-equal? (arity+keywords-subtract (arity+keywords 0 '(#:a #:b) '(#:a #:b #:c #:d))
                                           0 '(#:a #:d))
                  (arity+keywords 0 '(#:b) '(#:b #:c)))
    (check-equal? (arity+keywords-subtract (arity+keywords 0 '(#:a #:b) '(#:a #:b #:c #:d))
                                           0 '(#:b #:c))
                  (arity+keywords 0 '(#:a) '(#:a #:d)))
    (check-equal? (arity+keywords-subtract (arity+keywords 0 '(#:a #:b) '(#:a #:b #:c #:d))
                                           0 '(#:b #:d))
                  (arity+keywords 0 '(#:a) '(#:a #:c)))
    (check-equal? (arity+keywords-subtract (arity+keywords 0 '(#:a #:b) '(#:a #:b #:c #:d))
                                           0 '(#:c #:d))
                  (arity+keywords 0 '(#:a #:b) '(#:a #:b)))
    )
  
  )


