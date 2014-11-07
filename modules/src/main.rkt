#lang s-exp "../lang/main.rkt"
(require-for-dc "exports.rkt"
                "types.rkt"
                "scripts.rkt")

(define-export first-part 2d-point
  start)

(define-export goal path
  (cons (cons first-part
              (3D-point 1.0 2.0 3.0))
        end))
