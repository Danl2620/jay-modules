#lang racket/base

(require "base.rkt")
(require "types.rkt")
(require "exports.rkt")
(require "scripts.rkt")

(require racket/pretty)

(provide main)
(define (main . args)
  (pretty-print
   (compile-script
	`(begin
	   start
	   (3d-point 1.0 2.0 3.0)
	   end))))
