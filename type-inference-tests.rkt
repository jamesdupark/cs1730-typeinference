#lang racket

;; =============================================================================
;; Type Inference: type-inference-tests.rkt
;; =============================================================================

(require (only-in "type-inference.rkt" type-infer)
         "support.rkt"
         "test-support.rkt")

;; DO NOT EDIT ABOVE THIS LINE =================================================

(define/provide-test-suite student-tests ;; DO NOT EDIT THIS LINE =======
  ; TODO: Add your own tests below!
  (test-equal? "Works with Num primitive"
               (type-infer `2) (t-num))
  (test-raises-error? "Unbound identifier"
                      (type-infer `x))

  ; primitive types
  (test-equal? "num primitive" (type-infer `3/4) (t-num))
  (test-equal? "str primitive" (type-infer `"hi") (t-str))
  (test-equal? "bool primitive" (type-infer `true) (t-bool))
  (test-equal? "empty primitive"
               (normalize (type-infer `empty))
               (normalize (t-list (t-var (gensym)))))

  ;; Testing binops
  ; testing +
  (test-equal? "basic add" (type-infer `(+ 1.3 2/4)) (t-num))
  (test-equal? "nested addition" (type-infer `(+ 1 (+ 3.0 4/5))) (t-num))
  (test-raises-error? "l not number" (type-infer `(+ true 3)))
  (test-raises-error? "r not number" (type-infer `(+ 4 (link 3 (link 4 empty)))))
  (test-raises-error? "both non-number" (type-infer `(+ (lam x x) "hello")))
  (test-raises-error? "nested error" (type-infer `(+ (+ 4 "hi") 5)))
  (test-equal? "binops are desugared" (type-infer `(+ (let (x 3) x) (let (x 4) (+ x 3)))) (t-num))

  ; testing num=
  (test-equal? "basic num= check" (type-infer `(num= 3 3)) (t-bool))
  (test-equal? "basic num= check, but false" (type-infer `(num= 3 2.0)) (t-bool))
  (test-equal? "check for decimals" (type-infer `(num= 0.2 0.2)) (t-bool))
  (test-equal? "different representations" (type-infer `(num= 0.4 2/5)) (t-bool))
  (test-equal? "check with add operator" (type-infer `(num= (+ 1 2) 3)) (t-bool))
  (test-raises-error? "not enough args" (type-infer `(num= 2)))
  (test-raises-error? "no args" (type-infer `(num= )))
  (test-raises-error? "too many args" (type-infer `(num= 2 2 2)))
  (test-raises-error? "l not num" (type-infer `(num= "hi" 1)))
  (test-raises-error? "r not num" (type-infer `(num= 1 true)))
  (test-raises-error? "both sides not num" (type-infer `(num= true "hi")))
  (test-raises-error? "nested error" (type-infer `(num= 3 (+ "hey" 3))))

  ; testing str-append
  (test-equal? "basic string appending" (type-infer `(++ "h" "i")) (t-str))
  (test-equal? "nested string appending" (type-infer `(++ "h" (++ "el" "lo"))) (t-str))
  (test-raises-error? "l not str" (type-infer `(++ 2 "two")))
  (test-raises-error? "r not str" (type-infer `(++ "yes" false)))
  (test-raises-error? "both not str" (type-infer `(++ 2 true)))
  (test-raises-error? "nested error" (type-infer `(++ "hi" (++ "hey" 3))))

  ; testing str=
  (test-equal? "basic str= check, false return" (type-infer `(str= "hi" "hey")) (t-bool))
  (test-equal? "basic str= check, true return" (type-infer `(str= "hi" "hi")) (t-bool))
  (test-equal? "check with concacted str" (type-infer `(str= "hey" (++ "h" "ey"))) (t-bool))
  (test-raises-error? "not enough args" (type-infer `(str= "hey")))
  (test-raises-error? "not args" (type-infer `(str= )))
  (test-raises-error? "too many args" (type-infer `(str= "h" "h" "h")))
  (test-raises-error? "l not str" (type-infer `(str= 2 "hi")))
  (test-raises-error? "r not str" (type-infer `(str= "hey" false)))
  (test-raises-error? "both sides not str" (type-infer `(str= true 3.0)))
  (test-raises-error? "nested error" (type-infer `(str= "hi" (++ "hey" 3))))

  ; testing link
  (test-equal? "basic link" (type-infer `(link "h" (link "e" empty))) (t-list (t-str)))
  (test-equal? "list of lists"
               (type-infer `(link (link 1 (link 2 empty))
                                  (link (link 5 (link 6 (link 7 empty)))
                                        (link (link 3 empty)
                                              empty))))
               (t-list (t-list (t-num))))
  (test-raises-error? "nonhomogeneous list of lists"
                      (type-infer `(link (link 1 empty)
                                         (link (link "hi" empty)
                                               empty))))
  (test-equal? "empty correctly has filled type" (type-infer `(rest (link 2 empty))) (t-list (t-num)))
  (test-raises-error? "nonhomogeneous list" (type-infer `(link 2 (link "hi" empty))))
  (test-equal? "fills type on long list"
               (type-infer `(rest (rest (rest
                                         (link true
                                               (link (and true false)
                                                     (link (or true false) empty)))))))
               (t-list (t-bool)))
  (test-raises-error? "second arg is not a list" (type-infer `(link 3 4)))
  (test-equal? "+ operation at head of list"
               (type-infer `(link (+ 1 2) (link 3 empty))) (t-list (t-num)))
  (test-equal? "weird list defs"
               (normalize (type-infer `(link empty empty))) (t-list (t-list (t-var 'a))))
  (test-equal? "deeper weird list defs"
               (normalize (type-infer `(link (link empty empty)
                                             (link (link empty empty)
                                                   (link (link (link 3 empty) empty)
                                                         empty)))))
               (t-list (t-list (t-list (t-num)))))
  (test-raises-error? "link must type-check"
                      (type-infer `(link true (link false (link (and true "false") empty)))))

  ;; Testing unary operators
  ; testing first
  (test-equal? "basic test for first" (type-infer `(first (link 3 empty))) (t-num))
  (test-equal? "basic test for first with str"
               (type-infer `(first (link "str" empty))) (t-str))
  (test-equal? "test num= on output of first call"
               (type-infer `(or (first (link (and true false) empty)) true)) (t-bool))
  (test-equal? "call first on list of lists"
               (type-infer `(first (link (link 1 (link 2 empty))
                                         (link (link 5 (link 6 (link 7 empty)))
                                               (link (link 3 empty)
                                                     empty))))) (t-list (t-num)))
  (test-equal? "call first on list of lists"
               (type-infer `(first (link (link 1 (link 2 empty))
                                         (link (link 5 (link 6 (link 7 empty)))
                                               (link (link 3 empty)
                                                     empty))))) (t-list (t-num)))
  (test-equal? "first on empty list" (normalize (type-infer `(first empty))) (t-var 'a))
  (test-equal? "call first many times on empty list"
               (normalize (type-infer `(first (first (first empty))))) (t-var 'a))
  (test-raises-error? "call first on non list" (type-infer (first (++ "HI" "hey"))))
  (test-raises-error? "call first on fun" (type-infer (first (lam x x))))

  ; testing rest
  (test-equal? "basic test for rest with nums"
               (type-infer `(rest (link 3 (link 4 empty)))) (t-list (t-num)))
  (test-equal? "rest on list with one element"
               (type-infer `(rest (link "hi" empty))) (t-list (t-str)))
  (test-equal? "desugar with rest"
               (type-infer `(rest (link true (link (or true false) empty)))) (t-list (t-bool)))
  (test-equal? "rest on empty list" (normalize (type-infer `(rest empty))) (t-list (t-var 'a)))
  (test-equal? "call rest many times on empty list"
               (normalize (type-infer `(rest (rest (rest (rest (rest empty))))))) (t-list (t-var 'a)))
  (test-raises-error? "call rest on non list" (type-infer `(rest "hello")))
  (test-raises-error? "call rest on fun" (type-infer (rest (lam x x))))
  (test-equal? "series of first and rest"
               (type-infer `(first (rest (rest (link 3 (link 2 (link 1 empty))))))) (t-num))

  ; testing is-empty
  (test-equal? "test is-empty on non empty list"
               (type-infer `(is-empty (link 3 empty))) (t-bool))
  (test-equal? "test is-empty on empty list" (type-infer `(is-empty empty)) (t-bool))
  (test-equal? "test is-empty on rest call of single element list"
               (type-infer `(is-empty (rest (link "hi" empty)))) (t-bool))
  (test-equal? "series of first, rest, and is-empty"
               (type-infer `(is-empty (rest (rest
                                             (link (and true false) (link false empty)))))) (t-bool))
  (test-equal? "test is-empty on weird list" (type-infer `(is-empty (link empty empty))) (t-bool))
  (test-raises-error? "test is-empty on non list" (type-infer `(is-empty 0)))
  (test-raises-error? "call is-empty on fun" (type-infer (is-empty (lam x x))))
  (test-raises-error? "body must type-check"
                      (type-infer `(is-empty (link (and (num= 1 2) (str= "hi" 4)) empty))))

  ; testing if
  (test-equal? "test simple if" (type-infer `(if true 3 4)) (t-num))
  (test-equal? "test nested exprs" (type-infer `(if (num= 3 4) (+ 1 2) (+ 2 2))) (t-num))
  (test-equal? "if is nested" (type-infer `(+ 3 (if true 4 5))) (t-num))
  (test-equal? "nested if statements"
               (type-infer `(if (if (and true false) false true)
                                (if false (or true true) false)
                                (if false false (let (x true) x)))) (t-bool))
  (test-raises-error? "cond not a boolean" (type-infer `(if "true" true false)))
  (test-raises-error? "conseq and altern have different types" (type-infer `(if true true "hi")))
  (test-raises-error? "no short-ciruit" (type-infer `(if true (num= 1 2) (num= 3 "hi"))))

  ; identifier tests
  (test-raises-error? "unbound identifier" (type-infer `z))
  (test-equal? "list using ids"
               (type-infer `(let (x 3) (let (y 4) (link x (link y empty))))) (t-list (t-num)))
  (test-raises-error? "ids are not a type"
                      (type-infer `(let (x 3) (let (y "g") (link x (link y empty))))))

  ;; Testing Lambdas
  ; lambda definition
  (test-equal? "test simple definition"
               (type-infer `(lam z (str= z "zed"))) (t-fun (t-str) (t-bool)))
  (test-equal? "nested functions"
               (type-infer `(lam x (lam z (num= (+ x z) 3))))
               (t-fun (t-num) (t-fun (t-num) (t-bool))))
  (test-equal? "identity function" (normalize (type-infer `(lam x x))) (t-fun (t-var 'a) (t-var 'a)))
  (test-raises-error? "body must type-infer" (type-infer `(lam z (+ "hi" 3))))
  (test-raises-error? "unbound variable reference" (type-infer `(lam z x)))
  (test-equal? "list of lambdas"
               (normalize (type-infer `(link (lam x true)
                                             (link (lam y false)
                                                   empty)))) (t-list (t-fun (t-var 'a) (t-bool))))
  (test-raises-error? "lambda type mismatch with list annotation"
                      (type-infer `(link (lam x true)
                                         (link (lam y 3)
                                               empty))))
  (test-equal? "linked lambda signatures are inferred even when not directly used"
                      (type-infer `(let (l (link (lam x x) (link (lam y y) empty)))
                                     (let (g (first (rest l)))
                                       (let (z ((first l) 3)) g))))
                      (t-fun (t-num) (t-num)))
  (test-raises-error? "strange function composition resulting in circular constraint"
                      (type-infer `(let (f (lam x x))
                                     (let (g (lam y (f y))) (f g)))))
  (test-equal? "list in/out"
               (type-infer `(lam l (let (z (first l)) (link z (link "hi" (rest l))))))
                                 (t-fun (t-list (t-str)) (t-list (t-str))))

  ; lambda application
  (test-equal? "simple application"
               (type-infer `((lam x (num= x 0)) 1)) (t-bool))
  (test-equal? "nested function applications"
               (type-infer `(((lam x
                                   (lam y (++ x y))) "hi") " bye")) (t-str))
  (test-equal? "returns another lam"
               (normalize (type-infer `((lam b1 (lam b2 (if b1 1 0))) false)))
               (t-fun (t-var 'a) (t-num)))
  (test-equal? "returns another lam 2"
               (normalize (type-infer `((lam b1 (lam b2 (if b2 1 0))) false)))
               (t-fun (t-bool) (t-num)))
  (test-equal? "shadowing"
               (type-infer `((lam b1 (lam b1 (if b1 true false))) 3))
               (t-fun (t-bool) (t-bool)))
  (test-equal? "lam takes in another function"
               (type-infer `((lam f (f 5))
                             (lam x (if (num= x 5) "hi" "bye")))) (t-str))
  (test-equal? "lam takes in a list"
               (type-infer `((lam f (first f))
                             (link 5 (link 3 empty)))) (t-num))
  (test-equal? "identity function"
               (normalize (type-infer `((lam x x) true))) (t-bool))
  (test-raises-error? "trying to apply something that is not a function"
                      (type-infer `("my-function" 3)))
  (test-equal? "input list to function"
               (type-infer `((lam f (first f)) (link "a" (link "b" empty)))) (t-str))
  (test-raises-error? "incorrect list input type"
               (type-infer `((lam z (link true (rest z))) (link 2 (link 3 empty)))))
  (test-raises-error? "param must type-check"
                      (type-infer `((lam f (f 5)) (lam x (if x "hi" "bye")))))
  (test-equal? "desugar in fun"
               (type-infer `((lam x (and true (num= x 0))) (let (x 3) x))) (t-bool))
  (test-raises-error? "id is not a symbol"
                      (type-infer `((lam "three" (num= x 0)) 1)))
  (test-raises-error? "id is not symbol"
                      (type-infer `((lam 1 (num= 3 3)))))
  (test-equal? "generic intype"
               (normalize (type-infer `(lam x "hi"))) (t-fun (t-var 'a) (t-str)))
  (test-equal? "generic outtype simple"
               (normalize (type-infer `(lam l (first l)))) (t-fun (t-list (t-var 'a)) (t-var 'a)))
  (test-equal? "generic outtype"
               (normalize (type-infer
                           `(let (f (lam x (lam y (let (z (+ y 1)) (first empty))))) (f 3))))
               (t-fun (t-num) (t-var 'a)))
  (test-equal? "multiple nested generics?"
               (normalize (type-infer
                           `(let (f (lam x (lam y (let (z (+ y 1)) (first empty))))) f)))
               (t-fun (t-var 'a) (t-fun (t-num) (t-var 'b))))
  (test-equal? "any type resolves to function"
               (normalize (type-infer `(let (a (first empty)) (let (b (a 3)) a))))
               (t-fun (t-num) (t-var 'a)))
  (test-raises-error? "function that returns list must be consistent"
                      (normalize (type-infer `(let (f (lam x (link x empty)))
                                                (let (l (f 3))
                                                  (f "hi"))))))
  (test-equal? "empty list is typed after use"
               (normalize (type-infer `(let (e empty) (let (l (link 1 e)) e))))
               (t-list (t-num)))
  (test-raises-error? "empty list is not generic after use"
               (normalize (type-infer `(let (e empty)
                                         (let (a (first e))
                                           (let (l (link 1 e)) (link "hi" e)))))))
  (test-equal? "generic anys are equivalent when forced to be"
               (normalize (type-infer `(let (a (first empty))
                                               (let (b (first empty))
                                                 (if true a b)))))
               (t-var 'a))
  (test-equal? "generic anys are not equivalent until unified"
               (normalize (type-infer `(let (a (first empty))
                                         (let (b (first empty))
                                           (let (c (lam x b))
                                             (let (d (c a)) c))))))
               (t-fun (t-var 'a) (t-var 'b)))
  (test-raises-error? "textually equivalent exprs can have different types"
               (normalize (type-infer `(let (a (first empty))
                                               (let (b (first empty))
                                                 (let (c (link 1 a))
                                                   (let (d (link "hi" b))
                                                     (if true a b))))))))

  ;; testing sugar
  ; testing and/or
  (test-equal? "basic desugar" (type-infer `(and true false)) (t-bool))
  (test-equal? "nested desugar" (type-infer `(and (or true false) (and false false))) (t-bool))
  (test-equal? "nested ops" (type-infer `(or (num= 1 2) (str= "hi" "hi"))) (t-bool))
  (test-raises-error? "lhs not bool" (type-infer `(and 3 false)))
  (test-raises-error? "rhs not bool" (type-infer `(and false "hi")))
  (test-raises-error? "no short-circuit 2" (type-infer `(or true (lam x true))))

  ; testing let
  (test-equal? "simple let call" (type-infer `(let (x 3) (+ x -2.4))) (t-num))
  (test-equal? "let binding to operator" (type-infer `(let (x (+ -1/3 2)) (+ x 1))) (t-num))
  (test-equal? "let in binding position" (type-infer `(let (x (let (y 3) (num= y 4))) x)) (t-bool))
  (test-equal? "passing on bindings"
               (normalize (type-infer `(let (x (and true false)) (let (y x) (lam z (or x y))))))
               (t-fun (t-var 'a) (t-bool)))
  (test-raises-error? "wrong operation on binding" (type-infer `(let (x "hi") (++ x 2))))
  (test-raises-error? "unbound variable reference" (type-infer `(let (x 1) (+ y x))))
  (test-raises-error? "no symbol" (type-infer `(let ("one" 1) (+ 1 1))))
  (test-raises-error? "binding is to a symbol" (type-infer `(let (x y) (+ 1 x))))
  (test-raises-error? "disallow recursive definitions" (type-infer `(let (x (+ x 1)) (+ x x))))
  (test-raises-error? "recursive functions" (type-infer `(let (f (lam x
                                                                      (if (num= x 0)
                                                                          0
                                                                          (f (+ x -1))))) (f 3))))
  (test-equal? "function compositions"
               (normalize (type-infer `(let (f (lam x empty)) ((lam y f) f))))
               (t-fun (t-var 'a) (t-list (t-var 'b))))
  (test-equal? "multiple generics"
               (normalize (type-infer `(let (f (lam x empty)) (lam y f))))
               (t-fun (t-var 'a) (t-fun (t-var 'b) (t-list (t-var 'c)))))
  (test-equal? "generics resolve to concrete upon use"
               (normalize (type-infer `(let (a (first empty)) (let (b (and a true)) a))))
               (t-bool))

  ; scoping stuff
  (test-equal? "scoping test" (type-infer `(let (x 1)
                                             (let (y 2) (+ y x)))) (t-num))
  (test-equal? "variable shadowing" (type-infer `(let (x 2)
                                                   (let (x "hi") x))) (t-str))

  ; mix of lam and let
  (test-equal? "simple lam then let"
               (type-infer `((lam x (let (y 2) (num= x y))) 1)) (t-bool))
  (test-equal? "simple let then lam"
               (type-infer `(let (z "ab") ((lam y (str= y z)) "c"))) (t-bool))
  (test-equal? "same symbol for let and lam"
               (type-infer `((lam x (let (x 2) (+ x x))) 2)) (t-num))
  (test-equal? "var bound to lam" (type-infer `(let (f (lam x x)) (f 5))) (t-num))

  ; like everything
  (test-equal? "like everything"
               (type-infer `(let (x (if true 2 1))
                              (link 3 (link x empty)))) (t-list (t-num)))
  
  ;; example tests
  (test-equal? "Empty list n/a" (normalize (type-infer `empty)) (t-list (t-var 'a)))
  (test-equal? "Empty list in function" (normalize (type-infer `(lam x (lam y empty))))
               (t-fun (t-var 'a) (t-fun (t-var 'b) (t-list (t-var 'c)))))

  ; function that violates subterm constraint
  (test-raises-error? "subterms violated" (type-infer `(lam x (x x))))
  (test-raises-error? "subterms violated w/depth"
                      (type-infer `(link (lam x (x x)) empty)))
  (test-raises-error? "two-level circular constraint"
                      (type-infer `(let (e empty)
                                     (let (b (lam x e))
                                       (let (c (link b e))
                                         e)))))
  
  ; let-polymorphism
  (test-raises-error? "let-polymorphism"
                      (type-infer `(let (f (lam x x))
                                     (if (f true) (f 1) (f 2)))))
  (test-raises-error? "let-polymorphism with list elts"
                      (type-infer `(let (a (first empty))
                                     (if a (+ 1 a) (+ 2 a)))))

  )

;; DO NOT EDIT BELOW THIS LINE =================================================

(module+ main (run-tests student-tests))
