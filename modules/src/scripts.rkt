#lang s-exp "../lang/main.rkt"
(require racket/match)

(define-script 3D-point
  (λ (x y z)
    (cons (cons x y) z)))
