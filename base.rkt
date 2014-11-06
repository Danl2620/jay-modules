#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         racket/match
         racket/list)

;; Library

(define TYPES (make-hasheq))
(provide define-type)
(define-syntax (define-type stx)
  (syntax-parse stx
    [(_ n:id t:str)
     (syntax/loc stx
       (begin (hash-set! TYPES 'n t)))]
    [(_ n:id ((~literal cons) t1:id t2:id))
     (syntax/loc stx
       (begin (hash-set! TYPES 'n (cons 't1 't2))))]))

(provide make-header)
(define (make-header)
  (printf "Header =\n")
  (for ([(n t) (in-hash TYPES)])
    (printf "\t~a -> ~a\n" n t))
  (printf "\n"))


(struct problem (type value))
(define (instance-verify t v)
  (match t
    ["long"
     (if (flonum? v)
         empty
         (list (problem t v)))]
    [(? symbol? t)
     (define t-ref (hash-ref TYPES t))
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
    (printf "\t~e is not a ~a\n" v t))
  (printf "\n"))

(define EXPORTS (make-hasheq))
(define EXPORT->ADDR (make-hasheq))
(provide define-export)
(define-syntax (define-export stx)
  (syntax-parse stx
    [(_ n:id t:id v:expr)
     (syntax/loc stx
       (let* ([vi v]
              [problems (instance-verify 't vi)])
         (if (empty? problems)
             (hash-set! EXPORTS 'n vi)
             (display-problems 'n problems))))]))

(provide make-blob)
(define (make-blob)
  (printf "Blob =\n")
  (for ([(n v) (in-hash EXPORTS)]
        [i (in-naturals)])
    (hash-set! EXPORT->ADDR n i)
    (printf "\t~a. ~a -> ~v\n" i n v))
  (printf "\n"))

(define SCRIPT (make-hasheq))
(provide define-script)
(define-syntax (define-script stx)
  (syntax-parse stx
    [(_ n:id v:expr)
     (syntax/loc stx
       (hash-set! SCRIPT 'n v))]))

(provide compile-script)
(define (compile-script s)
  (match s
    [(cons (? (λ (x) (hash-has-key? SCRIPT x)) t) _)
     (compile-script ((hash-ref SCRIPT t) s))]
    [(? (λ (x) (hash-has-key? EXPORTS x)) x)
     (cons '#%ptr (hash-ref EXPORT->ADDR x))]
    [(cons '#%datum d)
     d]
    [(list s ...)
     (map compile-script s)]
    [s
     s]))

