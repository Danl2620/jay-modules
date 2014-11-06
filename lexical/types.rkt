#lang racket/base
(require "base.rkt")

(define-type long "long")
(define-type 2d-point (cons long long))
(define-type 3d-point (cons 2d-point long))

(provide (all-defined-out))
