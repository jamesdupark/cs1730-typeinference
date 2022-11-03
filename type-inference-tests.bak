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
                      (type-infer `x)))

;; DO NOT EDIT BELOW THIS LINE =================================================

(module+ main (run-tests student-tests))
