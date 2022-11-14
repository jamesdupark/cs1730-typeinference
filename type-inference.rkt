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
  (type-case LExpr expr
    [(e-op lb op l r) (e-op lb op (desugar l) (desugar r))]
    [(e-if lb cond then alt) (e-if lb (desugar cond) (desugar then) (desugar alt))]
    [(e-lam lb p-lb param body) (e-lam lb p-lb param (desugar body))]
    [(e-app lb fun arg) (e-app lb (desugar fun) (desugar arg))]
    [(sugar-and lb l r)
     (let ([l-desugared (desugar l)]
           [r-desugared (desugar r)])
     (e-if lb l-desugared
           (e-if (gensym) r-desugared (e-bool (gensym) #t) (e-bool (gensym) #f))
           (e-bool (gensym) #f)))]
    [(sugar-or lb l r)
     (let ([l-desugared (desugar l)]
           [r-desugared (desugar r)])
     (e-if lb l-desugared
           (e-bool (gensym) #t)
           (e-if (gensym) r-desugared (e-bool (gensym) #t) (e-bool (gensym) #f))))]
    [(sugar-let lb var value body)
     (desugar (e-app lb (e-lam (gensym) (gensym) var (desugar body)) (desugar value)))]
    [(e-un-op lb op arg) (e-un-op lb op (desugar arg))]
    [else expr]))

(define (type-of [expr : LExpr]): Term
  (local [(lenv : (Hashof Symbol Label))
          (define lenv (hash empty))]
    (type-of-env expr lenv)))

(define (type-of-env [expr : LExpr] [lenv : (Hashof Symbol Label)]): Term
  (let ([result-hash (unify (constraint-gen expr lenv) (hash empty))]
        [e-lb (get-label expr)])
    (type-case (Optionof Term) (hash-ref result-hash (get-label expr))
      [(some t) t]
      [(none) (t-var (gensym))])))

; Below are suggested helper functions that you may wish to implement
; when implementing `type-of`. You are free to change the signature of these
; functions as you wish, or you can remove them entirely.

(define (constraint-gen [expr : LExpr] [lenv : (Hashof Symbol Label)]): (Setof Constraint)
  (type-case LExpr expr
    [(e-num lb v) (set (eq (t-var lb) (t-num)))]
    [(e-bool lb v) (set (eq (t-var lb) (t-bool)))]
    [(e-str lb v) (set (eq (t-var lb) (t-str)))]
    [(e-empty lb) (set (eq (t-var lb) (t-list (t-var (gensym)))))]
    [(e-op lb o l r) (set-union (list (gen-op-constraints lb o l r)
                                      (constraint-gen l lenv)
                                      (constraint-gen r lenv)))]
    [(e-un-op lb o rg) (set-union (list (gen-unop-constraints lb o rg)
                                           (constraint-gen rg lenv)))]
    [(e-if lb c t e)
     (let ([c-lb (get-label c)]
           [t-lb (get-label t)]
           [e-lb (get-label e)])
       (set-union (list (set (eq (t-var c-lb) (t-bool))
                             (eq (t-var t-lb) (t-var e-lb))
                             (eq (t-var lb) (t-var e-lb)))
                        (constraint-gen c lenv)
                        (constraint-gen t lenv)
                        (constraint-gen e lenv))))]
    [(e-lam lb p-lb p b)
     (let ([b-lb (get-label b)])
       (set-add (constraint-gen b (hash-set lenv p p-lb))
                (eq (t-var lb) (t-fun (t-var p-lb) (t-var b-lb)))))]
    [(e-app lb f rg)
     (let ([f-lb (get-label f)]
           [rg-lb (get-label rg)])
       (set-union (list (set (eq (t-var f-lb) (t-fun (t-var rg-lb) (t-var lb))))
                        (constraint-gen f lenv)
                        (constraint-gen rg lenv))))]
    [(e-id lb s) (set (eq (t-var lb) (t-var (lookup lenv s))))]
    [else (error 'InferError "unreachable branch - should not have sugar exprs")]))

(define (unify [constraints : (Setof Constraint)] [subs : Substitution]): Substitution
  (type-case (Pick Constraint) (set-pick constraints)
    [(pick-none) subs]
    [(pick-some c rst)
     (type-case Constraint c
       [(eq l r)
        (let ([replace-l-sub (lambda ([t : Term]) (replace-term-r l r t))]
              [replace-l-cstr (provide-replace-function l r)])
          (type-case Term l
            [(t-var l-lb)
             (type-case Term r
               [(t-var r-lb) ; replace all l with r in subs and consts
                (unify (map-set replace-l-cstr rst)
                       (map-table-values replace-l-sub (hash-set subs l-lb r)))]
               [(t-con h args) ; add information to sub, replace all l with r
                (unify (map-set replace-l-cstr rst)
                       (map-table-values replace-l-sub (hash-set subs l-lb r)))])]
            [(t-con l-h l-rgs)
             (type-case Term r
               [(t-var lb) ; rewrite as lhs being the t-var term
                (unify (set-add rst (eq r l)) subs)]
               [(t-con r-h r-rgs) ; check equality
                (if (equal? l-h r-h)
                    (unify (add-subterms l-rgs r-rgs rst) subs) ; recur on subterms
                    (error 'InferError (string-append "type constraint violated: left/right side "
                                                      "types do not match")))])]))])]))

(define (gen-op-constraints [lb : Label] [op : Operator] [l : LExpr] [r : LExpr]): (Setof Constraint)
  (let ([l-lb (get-label l)]
        [r-lb (get-label r)])
    (type-case Operator op
      [(op-link) (set (eq (t-var lb) (t-list (t-var l-lb)))
                      (eq (t-var r-lb) (t-list (t-var l-lb))))]
      [else (constrain-sym-ops lb op l-lb r-lb)])))

(define (constrain-sym-ops ; shorthand for operators with both sides same typed
         [lb : Label]
         [op : Operator]
         [l-lb : Label]
         [r-lb : Label]): (Setof Constraint)
  (let ([expr-c (void)]
        [subterm-c (void)])
    (begin 
      (type-case Operator op
        [(op-plus) (begin (set! expr-c (t-num))
                          (set! subterm-c (t-num)))]
        [(op-num-eq) (begin (set! expr-c (t-bool))
                            (set! subterm-c (t-num)))]
        [(op-append) (begin (set! expr-c (t-str))
                            (set! subterm-c (t-str)))]
        [(op-str-eq) (begin (set! expr-c (t-bool))
                            (set! subterm-c (t-str)))]
        [else (error 'InferError "not supported yet")])
      (set (eq (t-var lb) expr-c)
           (eq (t-var l-lb) subterm-c)
           (eq (t-var r-lb) subterm-c)))))

(define (gen-unop-constraints [lb : Label] [o : UnaryOperator] [rg : LExpr]): (Setof Constraint)
  (let ([rg-lb (get-label rg)])
    (type-case UnaryOperator o
      [(op-first)
       (let ([new-list-type (gensym)])
         (set (eq (t-var rg-lb) (t-list (t-var lb)))
              (eq (t-var new-list-type) (t-var lb))
              ))]
      [(op-rest) (set (eq (t-var lb) (t-var rg-lb))
                      (eq (t-var rg-lb) (t-list (t-var (gensym))))
                      )]
      [(op-is-empty) (set (eq (t-var lb) (t-bool))
                          (eq (t-var rg-lb) (t-list (t-var (gensym)))))])))

(define (lookup [lenv : (Hashof Symbol Label)] [s : Symbol]): Label
  (type-case (Optionof Label) (hash-ref lenv s)
    [(some lb) lb]
    [(none) (error 'InferError "unbound identfier error")]))

(define (provide-replace-function [find : Term] [replace : Term]): (Constraint -> Constraint)
  (lambda ([c : Constraint])
    (type-case Constraint c
      [(eq l r)
       (let ([new-l (replace-term-r find replace l)]
             [new-r (replace-term-r find replace r)])
         (eq new-l new-r))])))

(define (occurs-check [find : Term] [replace : Term]): Term
  (type-case Term replace
    [(t-con r-h r-rgs) (if (member find r-rgs)
                           (error 'InferError "circular constraint found")
                           (t-con (t-con-head replace)
                                  (map (lambda (replace) (occurs-check find replace))
                                       (t-con-args replace))))]
    [else replace]))

(define (replace-term-r [find : Term] [replace : Term] [in : Term]): Term
    (type-case Term in
    [(t-var in-lb) (if (equal? find in)
                       (if (t-con? replace)
                           (occurs-check find replace)
                           replace)
                       in)]
    [(t-con in-h in-args)
     (if (equal? find in)
         (occurs-check find replace)
         ; recur over args
         (t-con in-h (map (lambda (in) (replace-term-r find replace in)) in-args)))]))

(define (add-subterms [l-args : (Listof Term)]
                      [r-args : (Listof Term)]
                      [cs : (Setof Constraint)]): (Setof Constraint)
  (set-union (list (list->set (map2 eq l-args r-args)) cs)))
