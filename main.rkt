#lang racket/base
(require "base.rkt"
         "types.rkt"
         "exports.rkt"
         "scripts.rkt"
         racket/pretty)

(module+ main
  ;; Mode 1
  (make-header)
  
  ;; Mode 2
  (make-blob)
  
  ;; Mode 3
  (pretty-print
   (compile-script
    `(begin
      start
      (3d-point 1.0 2.0 3.0)
      end))))
