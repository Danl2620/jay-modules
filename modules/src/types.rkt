#lang s-exp "../lang/main.rkt"

(define-type long "long")
(define-type 2d-point (cons long long))
(define-type 3d-point (cons 2d-point long))
(define-type path (cons (cons 2d-point 3d-point) 3d-point))
