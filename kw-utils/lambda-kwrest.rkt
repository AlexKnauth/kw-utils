#lang racket/base

(provide lambda/kwrest case-lambda/kwrest)

(require (only-in racket/list drop)
         (only-in racket/set subset?)
         (only-in racket/unsafe/undefined unsafe-undefined)
         (for-syntax racket/base
                     (only-in racket/function normalize-arity)
                     racket/list
                     racket/match
                     (only-in racket/set set-intersect set-union)
                     (only-in racket/syntax generate-temporary)
                     (only-in syntax/name syntax-local-infer-name)
                     syntax/parse
                     (only-in syntax/parse [attribute @])
                     (only-in syntax/parse/lib/function-header formals-no-rest)))
(module+ test
  (require racket/math
           rackunit))

;; ---------------------------------------------------------

(begin-for-syntax
  (define-syntax-class pos-param
    #:attributes [param id mand opt-id
                  param/tmp expr/tmp default]
    [pattern {~and mand:id id:id
                   param param/tmp}
      #:attr expr/tmp #f
      #:attr opt-id #f
      #:attr default #f]
    [pattern {~and [{~and opt-id:id id:id} default:expr]
                   param}
      #:with tmp (generate-temporary #'id)
      #:with param/tmp
      (syntax/loc #'param [tmp unsafe-undefined])
      #:with expr/tmp #'(if (eq? tmp unsafe-undefined) default tmp)
      #:attr mand #f])

  (define-splicing-syntax-class kw-param
    #:attributes [kw id mand-kw default]
    [pattern {~seq {~and kw:keyword mand-kw:keyword} id:id}
      #:attr default #f]
    [pattern {~seq kw:keyword [id:id default:expr]}
      #:attr mand-kw #f])

  (define-splicing-syntax-class single-param
    #:attributes [pos.param pos.id pos.mand pos.opt-id
                  pos.param/tmp pos.expr/tmp pos.default
                  kwp.kw kwp.id kwp.mand-kw kwp.default]
    [pattern {~seq pos:pos-param}
      #:attr kwp.kw #f
      #:attr kwp.id #f
      #:attr kwp.mand-kw #f
      #:attr kwp.default #f]
    [pattern kwp:kw-param
      #:attr pos.param #f
      #:attr pos.id/tmp #f
      #:attr pos.param/tmp #f
      #:attr pos.expr/tmp #f
      #:attr pos.id #f
      #:attr pos.mand #f
      #:attr pos.opt-id #f
      #:attr pos.default #f])

  (define-splicing-syntax-class single-params
    #:auto-nested-attributes
    [pattern {~and _:formals-no-rest {~seq :single-param ...} {~seq stx ...}}
      #:attr pos-mand-N (length (syntax->list #'({~? pos.mand} ...)))
      #:attr pos-opt-N (length (syntax->list #'({~? pos.opt-id} ...)))
      #:attr pos-all-N (+ (@ pos-mand-N) (@ pos-opt-N))
      #:with pos-mand-N-stx #`'#,(@ pos-mand-N)
      #:with pos-all-N-stx #`'#,(@ pos-all-N)
      #:attr [pos-index 1]
      (for/fold ([acc '()] [i 0] #:result (reverse acc))
                ([x (in-list (@ pos.id))])
        (if x (values (cons i acc) (add1 i)) (values (cons #f acc) i)))
      #:attr [pos-index-stx 1]
      (for/list ([i (in-list (@ pos-index))])
        (and i #`'#,i))
      #:attr mand-kws (sort (syntax->datum #'({~? kwp.mand-kw} ...)) keyword<?)])

  (define-splicing-syntax-class params/kwrest
    #:auto-nested-attributes
    [pattern {~seq (s:single-params)
                   {~alt {~optional {~seq #:rest r:id}}
                         {~optional {~seq #:kwrest kwr:id}}}
                   ...}
      #:attr pos-arity
      (cond
        [(@ r) (arity-at-least (@ s.pos-mand-N))]
        [(< (@ s.pos-mand-N) (@ s.pos-all-N))
         (inclusive-range (@ s.pos-mand-N) (@ s.pos-all-N))]
        [else (@ s.pos-mand-N)])
      #:attr allowed-kws
      (cond
        [(@ kwr) #f]
        [else (sort (syntax->datum #'({~? s.kwp.kw} ...)) keyword<?)])])

  (define-syntax-class (pos-arity-case-tmps arity)
    #:attributes [(s 2) (r 1)]
    [pattern _
      #:do [(define cs (flatten arity))]
      #:with [(s:id ... . {~or r:id ()}) ...]
      (for/list ([c (in-list cs)])
        (match c
          [(? exact-nonnegative-integer? n) (generate-temporaries (make-list n 's))]
          [(arity-at-least n) (append (generate-temporaries (make-list n 's))
                                      (generate-temporary 'r))]))])

  (define (arity->syntax a)
    (match (normalize-arity a)
      [(arity-at-least N) #`(arity-at-least '#,N)]
      [(list (? exact-nonnegative-integer? a) ... (arity-at-least N))
       (with-syntax ([(n ...) a])
         #`(list 'n ... (arity-at-least '#,N)))]
      [(list (? exact-nonnegative-integer? n) ...) #`'#,n]
      [(? exact-nonnegative-integer? n) #`'#,n])))

;; ---------------------------------------------------------

(define-syntax lambda/kwrest
  (lambda (stx)
    (syntax-parse stx
      [(_ (s:single-params) {~optional {~seq #:rest r:id}} b:expr ...+)
       #'(lambda (s.stx ... . {~? r ()}) b ...)]
      [(_ :params/kwrest
          b:expr
          ...+)
       #:with name:id (or (syntax-local-infer-name stx) #'proc)
       #:attr kw-mand-error
       (match (@ s.mand-kws)
         ['() #f]
         [(list kw) #`(raise-arguments-error 'name
                                             "required keyword argument not supplied"
                                             "required keyword" '#,kw)]
         [kws #`(raise-arguments-error 'name
                                       "required keyword arguments not supplied"
                                       "required keywords" '#,kws)])
       #:attr pos-arity-stx (arity->syntax (@ pos-arity))
       #:attr kw-mand-arity (and (pair? (@ s.mand-kws)) #`'#,(@ s.mand-kws))
       #:attr kw-allowed-arity (and (pair? (@ s.mand-kws)) #`'#,(@ allowed-kws))
       #:with {~var case-tmps (pos-arity-case-tmps (@ pos-arity))} #f
       #'(let*
             ([kwhash-proc
               (lambda (kwhash {~? s.pos.param/tmp} ... . {~? r ()})
                 (let*
                     ([kwr (hash-remove* kwhash '({~? s.kwp.kw} ...))]
                      {~? [s.pos.id s.pos.expr/tmp]
                          {~? [s.kwp.id
                               (hash-ref kwhash 's.kwp.kw
                                 (λ ()
                                   {~? s.kwp.default
                                       (raise-arguments-error
                                        'name
                                        "required keyword argument not supplied"
                                        "required keyword" 's.kwp.mand-kw)}))]}}
                      ...)
                   b
                   ...))]
              [name
               (case-lambda
                 [(case-tmps.s ... . {~? case-tmps.r ()})
                  {~? kw-mand-error
                      (apply kwhash-proc '#hasheq() case-tmps.s ...
                             {~? case-tmps.r '()})}]
                 ...)])
           {~? (procedure-reduce-keyword-arity
                (make-keyword-hash-procedure kwhash-proc name)
                pos-arity-stx
                kw-mand-arity
                kw-allowed-arity)
               (make-keyword-hash-procedure kwhash-proc name)})])))

(define-syntax case-lambda/kwrest
  (lambda (stx)
    (syntax-parse stx
      [(_ [c:params/kwrest
           b:expr
           ...+]
          ...)
       #:with name:id (or (syntax-local-infer-name stx) #'proc)
       #:do [(define pos-arity (normalize-arity (flatten (@ c.pos-arity))))
             (define mand-kws
               (cond
                 [(pair? (@ c.s.mand-kws))
                  (sort (apply set-intersect (@ c.s.mand-kws)) keyword<?)]
                 [else '()]))
             (define overall-kws
               (and (andmap values (@ c.allowed-kws))
                    (sort (apply set-union '() (@ c.allowed-kws)) keyword<?)))
             (define reduce-kws? (or (pair? mand-kws) overall-kws))]
       #:attr kw-mand-error
       (match mand-kws
         ['() #f]
         [(list kw) #`(raise-arguments-error 'name
                                             "required keyword argument not supplied"
                                             "required keyword" '#,kw)]
         [kws #`(raise-arguments-error 'name
                                       "required keyword arguments not supplied"
                                       "required keywords" '#,kws)])
       #:attr pos-arity-stx (arity->syntax pos-arity)
       #:attr kw-mand-arity (and reduce-kws? #`'#,mand-kws)
       #:attr kw-allowed-arity (and reduce-kws? #`'#,overall-kws)
       #:with {~var case-tmps (pos-arity-case-tmps pos-arity)} #f
       #'(let*
             ([kwhash-proc
               (lambda (kwhash . lst)
                 (define N (length lst))
                 (cond
                   [(and {~? (begin 'c.r (<= c.s.pos-mand-N-stx N))
                             (<= c.s.pos-mand-N-stx N c.s.pos-all-N-stx)}
                         (hash-has-keys? kwhash '({~? c.s.kwp.mand-kw} ...))
                         {~? 'c.kwr
                             (subset? (hash-keys kwhash) '({~? c.s.kwp.kw} ...))})
                    (let*
                        ({~? [c.r (drop lst (min N c.s.pos-all-N-stx))]}
                         {~? [c.kwr (hash-remove* kwhash '({~? c.s.kwp.kw} ...))]}
                         {~?
                          [c.s.pos.opt-id (if (< c.s.pos-index-stx N)
                                              (list-ref lst c.s.pos-index-stx)
                                              c.s.pos.default)]
                          {~?
                           [c.s.pos.mand (list-ref lst c.s.pos-index-stx)]
                           {~?
                            [c.s.kwp.id
                             (hash-ref kwhash 'c.s.kwp.kw
                               (λ ()
                                 {~? c.s.kwp.default
                                     (raise-arguments-error
                                      'name
                                      "required keyword argument not supplied"
                                      "required keyword" 'c.s.kwp.mand-kw)}))]}}}
                         ...)
                      b
                      ...)]
                   ...
                   [else (raise-arguments-error 'name "no case matches")]))]
              [name
               (case-lambda
                 [(case-tmps.s ... . {~? case-tmps.r ()})
                  {~? kw-mand-error
                      (apply kwhash-proc '#hasheq() case-tmps.s ...
                             {~? case-tmps.r '()})}]
                 ...)])
           {~? (procedure-reduce-keyword-arity
                (make-keyword-hash-procedure kwhash-proc name)
                pos-arity-stx
                kw-mand-arity
                kw-allowed-arity)
               (make-keyword-hash-procedure kwhash-proc name)})])))

;; ---------------------------------------------------------

(define (make-keyword-hash-procedure
         kwhash-proc
         [proc (lambda ps (apply kwhash-proc '#hasheq() ps))])
  (make-keyword-procedure
   (lambda (ks vs . ps)
     (apply kwhash-proc (keyword-lists->hash ks vs) ps))
   proc))

;; keyword-lists->hash : (Listof Keyword) (Listof V) -> (Hashof Keyword V)
(define (keyword-lists->hash ks vs)
  (make-immutable-hasheq (map cons ks vs)))

;; hash-remove* : (Hashof K V) (Listof K) -> (Hashof K V)
(define (hash-remove* h ks)
  (for/fold ([acc h]) ([k (in-list ks)])
    (hash-remove acc k)))

;; hash-has-keys? : (Hashof K V) (Listof K) -> Boolean
(define (hash-has-keys? h ks)
  (for/and ([k (in-list ks)]) (hash-has-key? h k)))

;; ---------------------------------------------------------

(module+ test
  (test-case "lambda/kwrest metadata"
    (check-equal? (procedure-arity (lambda/kwrest (a b) a))
                  2)
    (check-equal? (procedure-arity (lambda/kwrest (a [b #f] [c #f]) a))
                  '(1 2 3))
    (check-equal? (procedure-arity (lambda/kwrest (a [b #f] [c #f]) #:rest r a))
                  (arity-at-least 1))
    (check-equal? (procedure-arity (lambda/kwrest (a b) #:kwrest kwr a))
                  2)
    (check-equal? (procedure-arity (lambda/kwrest (a [b #f] [c #f]) #:kwrest kwr a))
                  '(1 2 3))
    (check-equal? (procedure-arity (lambda/kwrest (a [b #f] [c #f])
                                                  #:rest r #:kwrest kwr
                                                  a))
                  (arity-at-least 1))

    (check-equal? (call-with-values
                   (λ () (procedure-keywords
                          (lambda/kwrest (#:k v) v)))
                   list)
                  '((#:k) (#:k)))
    (check-equal? (call-with-values
                   (λ () (procedure-keywords
                          (lambda/kwrest (#:k [v #f]) v)))
                   list)
                  '(() (#:k)))
    (check-equal? (call-with-values
                   (λ () (procedure-keywords
                          (lambda/kwrest (#:k [v #f]) #:kwrest kwr v)))
                   list)
                  '(() #f))
    (check-equal? (call-with-values
                   (λ () (procedure-keywords
                          (lambda/kwrest (#:k v) #:kwrest kwr v)))
                   list)
                  '((#:k) #f))

    (check-equal? (object-name
                   (let ([f (lambda/kwrest (#:k v) #:kwrest kwr v)])
                     f))
                  'f))

  (test-case "default-expr scope"
    (check-equal? ((lambda (#:a a [b (list a)] #:c [c (list b)] [d (list c)])
                     d)
                   #:a 5)
                  '(((5))))

    (check-equal? ((lambda/kwrest (#:a a [b (list a)] #:c [c (list b)] [d (list c)])
                                  d)
                   #:a 5)
                  '(((5))))

    (check-equal? ((lambda/kwrest (#:a a [b (list a)] #:c [c (list b)] [d (list c)])
                                  #:kwrest kwr
                                  d)
                   #:a 5)
                  '(((5))))

    (check-equal? ((case-lambda/kwrest
                    [(#:a a [b (list a)] #:c [c (list b)] [d (list c)])
                     d])
                   #:a 5)
                  '(((5))))

    (check-equal? ((case-lambda/kwrest
                    [(#:a a [b (list a)] #:c [c (list b)] [d (list c)])
                     #:kwrest kwr
                     d])
                   #:a 5)
                  '(((5)))))

  (test-case "case-lambda/kwrest optional-case-no-backsies"
    (define optional-case-no-backsies
      (case-lambda/kwrest
       [(a) (list 'a a)]
       [([b #f]) (list 'b b)]))

    (check-equal? (optional-case-no-backsies 1) '(a 1))
    (check-equal? (optional-case-no-backsies) '(b #f)))

  (test-case "case-lambda/kwrest metadata"
    (check-equal? (procedure-arity (case-lambda/kwrest
                                    [(a b) a]))
                  2)
    (check-equal? (procedure-arity (case-lambda/kwrest
                                    [(a [b #f] [c #f]) a]))
                  '(1 2 3))
    (check-equal? (procedure-arity (case-lambda/kwrest
                                    [(a) a]
                                    [(a b c [d #f]) a]))
                  '(1 3 4))
    (check-equal? (procedure-arity (case-lambda/kwrest
                                    [(a [b #f] [c #f]) #:rest r a]))
                  (arity-at-least 1))
    (check-equal? (procedure-arity (case-lambda/kwrest
                                    [(a) a]
                                    [(a b c [d #f]) #:rest r a]))
                  (list 1 (arity-at-least 3)))
    (check-equal? (procedure-arity (case-lambda/kwrest
                                    [(a b) #:kwrest kwr a]))
                  2)
    (check-equal? (procedure-arity (case-lambda/kwrest
                                    [(a [b #f] [c #f]) #:kwrest kwr a]))
                  '(1 2 3))
    (check-equal? (procedure-arity (case-lambda/kwrest
                                    [(a) #:kwrest kwr a]
                                    [(a b c [d #f]) a]))
                  '(1 3 4))
    (check-equal? (procedure-arity (case-lambda/kwrest
                                    [(a [b #f] [c #f])
                                     #:rest r #:kwrest kwr
                                     a]))
                  (arity-at-least 1))
    (check-equal? (procedure-arity (case-lambda/kwrest
                                    [(a) #:kwrest kwr a]
                                    [(a b c [d #f]) #:rest r a]))
                  (list 1 (arity-at-least 3)))

    (check-equal? (call-with-values
                   (λ () (procedure-keywords
                          (case-lambda/kwrest [(#:k v) v])))
                   list)
                  '((#:k) (#:k)))
    (check-equal? (call-with-values
                   (λ () (procedure-keywords
                          (case-lambda/kwrest [(#:k v) v]
                                              [(#:m m) m])))
                   list)
                  '(() (#:k #:m)))
    (check-equal? (call-with-values
                   (λ () (procedure-keywords
                          (case-lambda/kwrest [(#:k v) v]
                                              [(#:k k #:m m) m])))
                   list)
                  '((#:k) (#:k #:m)))
    (check-equal? (call-with-values
                   (λ () (procedure-keywords
                          (case-lambda/kwrest [(#:k [v #f]) v])))
                   list)
                  '(() (#:k)))
    (check-equal? (call-with-values
                   (λ () (procedure-keywords
                          (case-lambda/kwrest [(#:k [v #f]) v]
                                              [(#:m [m #f]) m])))
                   list)
                  '(() (#:k #:m)))
    (check-equal? (call-with-values
                   (λ () (procedure-keywords
                          (case-lambda/kwrest [(#:k [v #f]) #:kwrest kwr v])))
                   list)
                  '(() #f))
    (check-equal? (call-with-values
                   (λ () (procedure-keywords
                          (case-lambda/kwrest [(#:k v) #:kwrest kwr v])))
                   list)
                  '((#:k) #f))
    (check-equal? (call-with-values
                   (λ () (procedure-keywords
                          (case-lambda/kwrest [(#:k v) v]
                                              [(#:k v) #:kwrest kwr v])))
                   list)
                  '((#:k) #f))
    (check-equal? (call-with-values
                   (λ () (procedure-keywords
                          (case-lambda/kwrest [() #f]
                                              [(#:k v) #:kwrest kwr v])))
                   list)
                  '(() #f))

    (check-equal? (object-name
                   (let ([f (case-lambda/kwrest [(#:k v) #:kwrest kwr v])])
                     f))
                  'f)))

;; ---------------------------------------------------------

(module+ test
  (let ([f (case-lambda/kwrest [([a 5]) a])])
    (check-equal? (f) 5)
    (check-equal? (f 4) 4))
  (let ([f (case-lambda/kwrest [(a [b 5]) (list a b)])])
    (check-equal? (f 1) (list 1 5))
    (check-equal? (f 1 2) (list 1 2)))
  (let ([f (case-lambda/kwrest [(a) (list 'first-case a)]
                               [(a [b 5]) (list 'second-case a b)])])
    (check-equal? (f 1) (list 'first-case 1))
    (check-equal? (f 1 2) (list 'second-case 1 2)))

  (let ([optional-case-no-backsies
         (case-lambda/kwrest
          [(a) (list 'a a)]
          [([b #f]) (list 'b b)])])
    (check-equal? (optional-case-no-backsies 1) '(a 1))
    (check-equal? (optional-case-no-backsies) '(b #f)))

  (let ([f (case-lambda/kwrest [(a b) (list 'first-case a b)]
                               [(a [b 5]) (list 'second-case a b)])])
    (check-equal? (f 1) (list 'second-case 1 5))
    (check-equal? (f 1 2) (list 'first-case 1 2)))
  (let ([f (case-lambda/kwrest [([a 5] [b 6]) #:rest rst (list a b rst)])])
    (check-equal? (f) (list 5 6 (list)))
    (check-equal? (f 1) (list 1 6 (list)))
    (check-equal? (f 1 2) (list 1 2 (list)))
    (check-equal? (f 1 2 3) (list 1 2 (list 3)))
    (check-equal? (f 1 2 3 4) (list 1 2 (list 3 4))))
  (let ([f (case-lambda/kwrest [(a b) (list 'first-case a b)]
                               [(a [b 5]) #:rest rst (list 'second-case a b rst)])])
    (check-equal? (f 1) (list 'second-case 1 5 (list)))
    (check-equal? (f 1 2) (list 'first-case 1 2))
    (check-equal? (f 1 2 3) (list 'second-case 1 2 (list 3)))
    (check-equal? (f 1 2 3 4) (list 'second-case 1 2 (list 3 4))))
  )

;; ---------------------------------------------------------

(module+ test
  (test-case "empty case-lambda/kwrest"
    (define f
      (case-lambda/kwrest))
    (check-equal? (procedure-arity f) '())
    )
  (test-case "positional case-lambda/kwrest"
    (define f
      (case-lambda/kwrest
       [() 0]
       [(x) 1]
       [(x y) 2]
       [() #:rest args (length args)]))
    (check-equal? (f) 0)
    (check-equal? (f 1) 1)
    (check-equal? (f 1 2) 2)
    (check-equal? (f 1 2 3 4 5 6) 6)
    )
  (test-case "keywords for case-lambda/kwrest"
    (define pythag
      (case-lambda/kwrest
       [(#:a a #:b b) (sqrt (+ (sqr a) (sqr b)))]
       [(#:c c #:a a) (sqrt (- (sqr c) (sqr a)))]
       [(#:c c #:b b) (sqrt (- (sqr c) (sqr b)))]
       [(#:a a #:b b #:c c) (= (+ (sqr a) (sqr b)) (sqr c))]))
    (check-equal? (pythag #:a 3 #:b 4) 5)
    (check-equal? (pythag #:c 5 #:a 3) 4)
    (check-equal? (pythag #:c 5 #:b 4) 3)
    (check-true (pythag #:a 3 #:b 4 #:c 5))
    (check-false (pythag #:a 3 #:b 4 #:c 6))
    (check-equal? (procedure-arity pythag) 0)
    (check-equal? (call-with-values
                   (λ () (procedure-keywords pythag)) list)
                  (list '() '(#:a #:b #:c)))
    (check-equal? (call-with-values
                   (λ () (procedure-keywords (case-lambda/kwrest
                                              [(#:a a #:b b) #t]
                                              [(#:a a #:not-b not-b) #t])))
                   list)
                  (list '(#:a) '(#:a #:b #:not-b)))
    )
  (test-case "rests for case-lambda/kwrest"
    (define f
      (case-lambda/kwrest
       [() #:rest rst #:kwrest kwrst (list rst kwrst)]))
    (check-match (f 0 1 2 #:kw "kw-arg")
                 (list (list 0 1 2) (hash-table ['#:kw "kw-arg"])))
    )
  (let ()
    (define f
      (case-lambda/kwrest
       [(#:m m #:v v) `(m: ,m v: ,v)]
       [(#:mass [m 0] #:velocity [v 0]) `(mass: ,m velocity: ,v)]
       [(m v) `(m: ,m v: ,v)]))
    (check-equal? (f #:m 2 #:v 1) '(m: 2 v: 1))
    (check-equal? (f #:mass 2) '(mass: 2 velocity: 0))
    (check-equal? (f #:mass 2 #:velocity 1) '(mass: 2 velocity: 1))
    (check-equal? (f) '(mass: 0 velocity: 0))
    (check-equal? (f 2 1) '(m: 2 v: 1))
    )
  (let ()
    (define f
      (case-lambda/kwrest
       [(a b #:d d [c #f] #:e [e #f]) #:rest rest
        (list a b c d e rest)]
       [(a [b #f] #:d [d #f] [c #f] #:e [e #f]) #:rest rest
        (list a b c d e rest)]
       [() #:rest rest #:kwrest kwrest
        (list rest kwrest)]))
    (check-equal? (f "a" "b" "c" #:d "d" #:e "e" "other-arg 1" "other-arg 2")
                  (list "a" "b" "c" "d" "e" (list "other-arg 1" "other-arg 2")))
    (check-equal? (f "a")
                  (list "a" #f #f #f #f '()))
    (check-equal? (f 0)
                  (list 0 #f #f #f #f '()))
    )
  )
