#lang plait

;; =============================================================================
;; Type Inference: type-inference.rkt
;; =============================================================================

(require (typed-in racket/base [gensym : (-> Symbol)]))
(require "support.rkt")

(define (type-infer [str : S-Exp]): Term
  (type-of (parse str)))

;; DO NOT EDIT ABOVE THIS LINE =================================================

(define (desugar [expr : LExpr]): LExpr
  ; TODO: Implement me!
  ....)

(define (type-of [expr : LExpr]): Term
  ; TODO: Implement me!
  ....)

; Below are suggested helper functions that you may wish to implement
; when implementing `type-of`. You are free to change the signature of these
; functions as you wish, or you can remove them entirely.

(define (constraint-gen [expr : LExpr]): (Setof Constraint)
  ; TODO: Optional function.
  ....)

(define (unify [constraints : (Setof Constraint)]): Substitution
  ; TODO: Optional function.
  ....)
