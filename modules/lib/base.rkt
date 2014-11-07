#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     unstable/syntax)
         racket/contract/base
         racket/match
         racket/list)

(define-syntax-rule (define-cons-param current-types cons-type!)
  (begin
    (define current-types
      (make-parameter #f))
    (define (cons-type! n)
      (define ts-b (current-types))
      (set-box! ts-b (cons n (unbox ts-b))))))

(define-cons-param current-types cons-type!)
(define-cons-param current-types-requires cons-types-requires!)
(define-cons-param current-exports cons-export!)
(define-cons-param current-exports-requires cons-exports-requires!)

(define-syntax-rule (define-cons-param-submod current-types types)
  (module+ types
    (define types (box null))
    (provide types)
    (current-types types)))

(define-syntax (begin-submodules stx)
  (syntax-parse stx
    [(_)
     (quasisyntax/loc stx
       (begin
         (module+ me
           (define me
             (build-path #,(syntax-source-directory stx)
                         #,(syntax-source-file-name stx)))
           (provide me))

         (define-cons-param-submod current-types types)
         (define-cons-param-submod current-types-requires types-requires)
         (module+ make-header
           (require (submod ".." types)
                    (submod ".." types-requires)
                    (submod ".." me))
           (make-header (unbox types-requires)
                        (unbox types)
                        me))

         (define-cons-param-submod current-exports exports)
         (define-cons-param-submod current-exports-requires exports-requires)
         (module+ make-blob
           (require (submod ".." exports)
                    (submod ".." exports-requires)
                    (submod ".." me))
           (make-blob (unbox exports-requires)
                      (unbox exports)
                      me))))]))

(define-syntax-rule (require-for-dc m ...)
  (begin (require-for-dc* m) ...))
(define-syntax (require-for-dc* stx)
  (syntax-parse stx
    [(_ m:expr)
     (syntax/loc stx
       (begin (require m)
              (module+ types-requires
                (require (rename-in (submod m me) [me it]))
                (cons-types-requires! (mod->header it)))
              (module+ exports-requires
                (require (rename-in (submod m me) [me it])
                         (rename-in (submod m exports) [exports r-es-b]))
                (cons-exports-requires! (cons (mod->blob it) (unbox r-es-b))))))]))

(define (mod->header in)
  (path-replace-suffix in #".h"))

(define (make-header REQUIRES TYPES in)
  (define out (mod->header in))
  (with-output-to-file out
    #:exists 'replace
    (λ ()
      (printf "// Header for ~a\n" in)
      (for ([m (in-list REQUIRES)])
        (printf "#include ~v\n" (path->string m)))
      (printf "\n")
      (for ([n*t (in-list TYPES)])
        (match-define (TYPE n t) n*t)
        (printf "typedef ~v ~a;\n" (type-format t) n))
      (printf "\n"))))

(define (value-format REQUIRES EXPORTS v)
  (let value-format ([v v])
    (match v
      [(EXPORT n _ _)
       (vector '#%ptr
               ;; xxx this O(n) searches could be O(1)
               (or (for/or ([e (in-list EXPORTS)]
                            [i (in-naturals)])
                     (and (eq? e v) i))
                   (for/or ([r (in-list REQUIRES)])
                     (match-define (cons n r-es) r)
                     (for/or ([e (in-list r-es)]
                              [i (in-naturals)])
                       (and (eq? e v) (cons (path->string n) i))))
                   (error 'value-format "Unknown export: ~v" v)))]
      [(cons a d)
       (cons (value-format a) (value-format d))]
      [x
       x])))

(define (mod->blob in)
  (path-replace-suffix in #".blob"))

(define (make-blob REQUIRES EXPORTS in)
  (define out (mod->blob in))
  (with-output-to-file out
    #:exists 'replace
    (λ ()
      (printf ";; Blob for ~a\n" in)
      (for ([n*v (in-list EXPORTS)]
            [i (in-naturals)])
        (match-define (EXPORT n t v) n*v)
        (printf ";; ~a. ~a(~a)\n~v\n"
                i n (type-format t)
                (value-format REQUIRES EXPORTS v)))
      (printf "\n"))))

(struct TYPE (n v) #:transparent)

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
    [(_ n:id t:expr)
     (syntax/loc stx
       (begin (define n (TYPE 'n (type-rep t)))
              (provide n)
              (module+ types
                (cons-type! n))))]))

(define-syntax (type-rep stx)
  (syntax-parse stx
    [(_ t:str)
     (syntax/loc stx t)]
    [(_ t:id)
     (syntax/loc stx (type-ref t))]
    [(_ ((~literal cons) t1:expr t2:expr))
     (syntax/loc stx (cons (type-rep t1) (type-rep t2)))]))

(struct problem (type value))
(define (instance-verify t v)
  (match t
    ["long"
     (if (flonum? v)
         empty
         (list (problem t v)))]
    [(TYPE _ t-ref)
     (match v
       [(EXPORT _ vt _)
        (if (eq? vt t)
            empty
            (list (problem t v)))]
       [_
        (define inner (instance-verify t-ref v))
        (if (empty? inner)
            empty
            (cons (problem t v)
                  inner))])]
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

(struct EXPORT (n t v) #:transparent)
(define-syntax (define-export stx)
  (syntax-parse stx
    [(_ n:id t:id v:expr)
     (syntax/loc stx
       (begin
         (define n
           (let* ([vi v]
                  [problems (instance-verify (type-ref t) vi)])
             (cond
              [(empty? problems)
               (EXPORT 'n (type-ref t) vi)]
              [else
               (display-problems 'n problems)
               (error 'define-export)])))
         (provide n)
         (module+ exports
           (cons-export! n))))]))

(struct SCRIPT (n v)
        #:transparent
        #:property prop:procedure
        (λ (s . stx)
          (apply (SCRIPT-v s) stx)))
(define-syntax (define-script stx)
  (syntax-parse stx
    [(_ n:id v:expr)
     (syntax/loc stx
       (begin (define n (SCRIPT 'n v))
              (provide n)))]))

;; xxx tighten
(provide (all-defined-out))
