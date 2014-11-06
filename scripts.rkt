#lang racket/base
(require racket/match
         "base.rkt")

(define-script 3d-point
  (match-lambda
   [(list _ x y z) (cons '#%datum (cons (cons x y) z))]))
