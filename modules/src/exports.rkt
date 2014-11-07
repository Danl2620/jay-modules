#lang racket/base
(require "types.rkt"
         "base.rkt")

(define-export start 2d-point (cons 0.0 0.0))
(define-export middle 3d-point (cons 1.0 2.0))
(define-export end 3d-point (cons (cons 0.0 0.0) 1.0))

(provide (all-defined-out))
