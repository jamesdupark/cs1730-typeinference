#lang plait

;; =============================================================================
;; Type Inference: type-inference.rkt
;; =============================================================================

(require (typed-in racket/base [gensym : (-> Symbol)]))
(require "support.rkt")

(define (type-infer [str : S-Exp]): Term
  (type-of (desugar (parse str))))

;; DO NOT EDIT ABOVE THIS LINE =================================================

(define (desugar [expr : LExpr]): LExpr
  expr
  )

(define (type-of [expr : LExpr]): Term
  (local [(lenv : (Hashof Symbol Label))
           (define lenv (hash empty))]
    (type-of-env expr lenv)))

(define (type-of-env [expr : LExpr] [lenv : (Hashof Symbol Label)]): Term
  (type-case (Optionof Term) (hash-ref (unify (constraint-gen expr lenv)) (get-label expr))
    [(some t) t]
    [(none) (error 'InferError "label not found")])) ; add polymorphism here?

; Below are suggested helper functions that you may wish to implement
; when implementing `type-of`. You are free to change the signature of these
; functions as you wish, or you can remove them entirely.

(define (constraint-gen [expr : LExpr] [lenv : (Hashof Symbol Label)]): (Setof Constraint)
  (type-case LExpr expr
    [(e-num lb v) (set (eq (t-var lb) (t-num)))]
    [(e-bool lb v) (set (eq (t-var lb) (t-bool)))]
    [(e-op lb o l r) (set-union (list (gen-op-constraints lb o l r)
                                      (constraint-gen l lenv)
                                      (constraint-gen r lenv)))]
    [else (error 'InferError "not supported yet")]))

(define (unify [constraints : (Setof Constraint)]): Substitution
  ; TODO: Optional function.
  (type-case (Pick Constraint) (set-pick constraints)
    [(pick-none) (local [(subs : (Hashof Label Term))
                         (define subs (hash empty))]
                   subs)]
    [(pick-some c rst)
       (type-case Constraint c
         [(eq l r)
          (let ([replace-r-sub (lambda ([t : Term]) (if (equal? t r) l t))]
                [replace-r-cstr (replace-term-cstr r l)])
            (type-case Term l
              [(t-var l-lb)
               (type-case Term r
                 [(t-var r-lb) ; replace all r with l in subs and consts
                  (map-table-values replace-r-sub (unify (map-set replace-r-cstr rst)))]
                 [(t-con h args) ; check equality, recur on subterm equality
                  (hash-set (unify rst) l-lb r)])]
              [(t-con l-h l-rgs)
               (type-case Term r
                 [(t-var lb) ; rewrite as lhs being the t-var term
                  (unify (set-add rst (eq r l)))]
                 [(t-con r-h r-rgs) ; check equality, recur on subterm equality
                  (if (equal? l r)
                      (unify rst) ; TODO: recur on subterms here
                      (error 'InferError "type constraint violated: left/right side types do not match"))])]))])]))

(define (gen-op-constraints [lb : Label] [op : Operator] [l : LExpr] [r : LExpr]): (Setof Constraint)
  (let ([l-lb (get-label l)]
        [r-lb (get-label r)]
        [expr-c (void)]
        [subterm-c (void)])
    (begin 
      (type-case Operator op
        [(op-plus) (begin (set! expr-c (t-num))
                          (set! subterm-c (t-num)))]
        [else (error 'InferError "not supported yet")])
      (set (eq (t-var lb) expr-c)
           (eq (t-var l-lb) subterm-c)
           (eq (t-var r-lb) subterm-c)))))

(define (replace-term-cstr [find : Term] [replace : Term]): (Constraint -> Constraint)
  (lambda ([c : Constraint])
  (type-case Constraint c
    [(eq l r)
     (let ([new-l (if (equal? l find) replace l)]
           [new-r (if (equal? r find) replace r)])
       (eq new-l new-r))])))
