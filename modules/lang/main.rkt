#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "../lib/base.rkt")

(define-syntax (:module-begin stx)
  (syntax-parse stx
    [(_ body ...)
     (quasisyntax/loc stx
       (#%module-begin
        #,(syntax/loc stx (begin-submodules))
        body ...))]))

(provide
 require-for-dc
 define-type
 define-export
 define-script
 (rename-out [:module-begin #%module-begin])
 (except-out (all-from-out racket/base)
             #%module-begin))
