#lang s-exp "../lang/main.rkt"
(require-for-dc "types.rkt")

(define-export start 2d-point (cons 0.0 0.0))
(define-export end 3d-point (cons (cons 0.0 0.0) 1.0))
