#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         racket/contract/base
         racket/match
         racket/list)

;; Library

(provide define-type)

(struct TYPE (n v))

(define-syntax-rule (type-ref t)
  ;; XXX replace 'pos and 'neg with better srcloc
  (contract TYPE? t 'pos 'neg))

(define (type-format s)
  (match s
    [(TYPE n _)
     n]
    [(? string?)
     s]
    [(cons a d)
     (cons (type-format a)
           (type-format d))]))

(define-syntax (define-type stx)
  (syntax-parse stx
    [(_ n:id t:str)
     (syntax/loc stx
       (define n (TYPE 'n t)))]
    [(_ n:id ((~literal cons) t1:id t2:id))
     (syntax/loc stx
       (define n (TYPE 'n (cons (type-ref t1) (type-ref t2)))))]))

(provide make-header)
(define (make-header TYPES)
  (printf "Header =\n")
  (for ([n*t (in-list TYPES)])
    (match-define (TYPE n t) n*t)
    (printf "\t~a -> ~a\n" n (type-format t)))
  (printf "\n"))

(struct problem (type value))
(define (instance-verify t v)
  (match t
    ["long"
     (if (flonum? v)
         empty
         (list (problem t v)))]
    [(TYPE _ t-ref)
     (define inner (instance-verify t-ref v))
     (if (empty? inner)
         empty
         (cons (problem t v)
               inner))]
    [(cons t1 t2)
     (match v
       [(cons v1 v2)
        (append (instance-verify t1 v1)
                (instance-verify t2 v2))]
       [_
        (list (problem t v))])]))

(define (display-problems n ps)
  (printf "~a disobeys its type's specification:\n" n)
  (for ([p (in-list ps)])
    (match-define (problem t v) p)
    (printf "\t~e is not a ~a\n" v (type-format t)))
  (printf "\n"))

(provide define-export)
(struct EXPORT (n t v))
(define-syntax (define-export stx)
  (syntax-parse stx
    [(_ n:id t:id v:expr)
     (syntax/loc stx
       (define n
         (let* ([vi v]
                [problems (instance-verify (type-ref t) vi)])
           (cond
            [(empty? problems)
             (EXPORT 'n (type-ref t) vi)]
            [else
             (display-problems 'n problems)
             #f]))))]))

(provide make-blob)
(define (make-blob EXPORTS)
  (printf "Blob =\n")
  (begin0
      (for/hasheq ([n*v (in-list EXPORTS)]
                   [i (in-naturals)])
        (match-define (EXPORT n t v) n*v)
        (printf "\t~a. ~a(~a) -> ~v\n" i n (type-format t) v)
        (values n i))
    (printf "\n")))

(provide define-script)
(struct SCRIPT (n v))
(define-syntax (define-script stx)
  (syntax-parse stx
    [(_ n:id v:expr)
     (syntax/loc stx
       (define n (SCRIPT 'n v)))]))

(provide compile-script)
(define (compile-script EXPORT->ADDR s)
  (let compile-script ([s s])
    (match s
      [(cons (SCRIPT _ v) _)
       (compile-script (v s))]
      [(EXPORT n _ _)
       (cons '#%ptr (hash-ref EXPORT->ADDR n))]
      [(cons '#%datum d)
       d]
      [(list s ...)
       (map compile-script s)]
      [s
       s])))
