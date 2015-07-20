#lang racket/base

(provide partial
         app
         )

(require racket/list
         "arity+keywords.rkt"
         "keyword-app.rkt"
         )
(module+ test
  (require rackunit))

;; based on partial from rackjure:
;; https://github.com/greghendershott/rackjure/blob/master/rackjure/utils.rkt
;; http://pkg-build.racket-lang.org/doc/rackjure/index.html#%28def._%28%28lib._rackjure%2Futils..rkt%29._partial%29%29

(define (kw-proc name arity+kws proc)
  (procedure-reduce-arity+keywords
   (procedure-rename (make-keyword-procedure proc) name)
   arity+kws))

(define-syntax-rule (define-kw-proc name arity+kws clause ...)
  (define name (kw-proc 'name arity+kws (case-lambda clause ...))))

(define (double-keyword-apply f kws-1 kw-args-1 kws-2 kw-args-2 rest-args)
  (keyword-app keyword-apply kws-1 kw-args-1
               f kws-2 kw-args-2
               rest-args))

(define-kw-proc partial (arity+keywords (arity-at-least 0) '() #f)
  [(kws-1 kw-args-1 f . args-1)
   (define f.arity+kws
     (procedure-arity+keywords f))
   (define arity+kws
     (arity+keywords-subtract f.arity+kws (length args-1) kws-1))
   (cond [(and (empty? kws-1) (empty? kw-args-1) (empty? args-1)) f]
         [(empty? (arity+keywords-arity arity+kws))
          (raise-too-many-partial-arguments-error f kws-1 kw-args-1 args-1)]
         [else
          (define-kw-proc partial-f arity+kws
            [(kws-2 kw-args-2 . args-2)
             (double-keyword-apply f kws-1 kw-args-1 kws-2 kw-args-2
                                   (append args-1 args-2))])
          partial-f])]
  [(kws-1 kw-args-1)
   (keyword-app partial kws-1 kw-args-1 app)])

(define (raise-too-many-partial-arguments-error f kws-1 kw-args-1 args-1)
  (error 'partial
         (string-append "too many arguments" "\n"
                        "  function: ~v" "\n"
                        "  partial arguments: ~a")
         f
         (kw-args->string kws-1 kw-args-1 args-1)))

(define (kw-args->string kws kw-args rest-args)
  (define (string-append* . args)
    (apply string-append (flatten args)))
  (string-append*
   (for/list ([arg (in-list rest-args)])
     (format "~v " arg))
   (for/list ([kw (in-list kws)]
              [kw-arg (in-list kw-args)])
     (format "~a ~v " kw kw-arg))))


(define-kw-proc app (arity+keywords (arity-at-least 1) '() #f)
  [(kws kw-args f . args)
   (keyword-apply f kws kw-args args)])



(module+ test
  ;; If we tested against the variable-arity `+` there would
  ;; be no difference between `partial` and `curry`.
  (define (+* x y) (+ x y))

  (check-equal? ((partial +*) 1 2) 3)
  (check-equal? ((partial +* 1) 2) 3)
  (check-equal? ((partial +* 1 2)) 3)
  (check-equal? ((partial) +* 1 2) 3)
  (check-exn (regexp (regexp-quote "too many arguments"))
             (λ () (partial +* 1 2 3)))
  
  ;; arity
  (check-equal? (procedure-arity+keywords (partial +*)) (arity+keywords 2 '() '()))
  (check-equal? (procedure-arity+keywords (partial +* 1)) (arity+keywords 1 '() '()))
  (check-equal? (procedure-arity+keywords (partial +* 1 2)) (arity+keywords 0 '() '()))
  
  ;; keywords
  (test-case "partial with keywords"
    (define (KE #:m m #:v v)
      (* 1/2 m v v))
    (check-equal? ((partial KE) #:m 2 #:v 1) 1)
    (check-equal? ((partial KE #:m 2) #:v 1) 1)
    (check-equal? ((partial KE #:m 2 #:v 1)) 1)
    (check-equal? ((partial) KE #:m 2 #:v 1) 1)
    (check-equal? ((partial #:m 2) KE #:v 1) 1)
    (check-exn (regexp (regexp-quote "too many arguments"))
               (λ () (partial KE #:whatever "idontkare")))
    ;; arity
    (check-equal? (procedure-arity+keywords (partial KE)) (arity+keywords 0 '(#:m #:v) '(#:m #:v)))
    (check-equal? (procedure-arity+keywords (partial KE #:m 2)) (arity+keywords 0 '(#:v) '(#:v)))
    (check-equal? (procedure-arity+keywords (partial KE #:v 1)) (arity+keywords 0 '(#:m) '(#:m)))
    (check-equal? (procedure-arity+keywords (partial KE #:m 2 #:v 1)) (arity+keywords 0 '() '()))
    (check-equal? (procedure-arity+keywords (partial)) (arity+keywords (arity-at-least 1) '() #f))
    (check-equal? (procedure-arity+keywords (partial #:m 2))(arity+keywords(arity-at-least 1)'()#f))))
