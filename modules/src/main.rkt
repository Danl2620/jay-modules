#lang racket/base
(require "base.rkt"
         "types.rkt"
         "exports.rkt"
         "scripts.rkt"
         racket/pretty)

(module+ main
  ;; Mode 1
  (make-header (list long 2d-point 3d-point))
  
  ;; Mode 2
  (define MAP (make-blob (list start end)))
  
  ;; Mode 3
  (pretty-print
   (compile-script
    MAP
    `(begin
      ,start
      (,3D-point 1.0 2.0 3.0)
      ,end))))
