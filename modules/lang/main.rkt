#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "../lib/base.rkt")

;; The only purpose of this is to (a) make sure that every DC module
;; has the right submodules and (b) make sure that the DC code has the
;; right imports. Nothing exciting going on.

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
