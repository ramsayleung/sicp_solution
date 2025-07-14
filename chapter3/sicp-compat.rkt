#lang racket
(require compatibility/mlist)
(provide (all-defined-out))

;;; racket doesn't support set-car! and set-cdr! for immutable pair,
;;; it provides a mutable counterpart, and m-* related functions
;;; so create aliases for the mutable list operations
;; SICP compatibility layer
(define cons mcons)
(define car mcar)
(define cdr mcdr)
(define set-car! set-mcar!)
(define set-cdr! set-mcdr!)
(define list mlist)
(define pair? mpair?)
(define append mappend)
(define length mlength)
(define reverse mreverse)
