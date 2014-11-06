#lang racket/base
(require racket/match
         "base.rkt")

(define-script 3D-point
  (match-lambda
   [(list _ x y z) (cons '#%datum (cons (cons x y) z))]))

(provide (all-defined-out))
