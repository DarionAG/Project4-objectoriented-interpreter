#lang racket

(require racket/runtime-path)
(require "../interp.rkt")

(define-runtime-path prog "step17a-alias-mutation.txt")

(define (check label got expected)
  (if (equal? got expected)
      (printf "PASS: ~a => ~a\n" label got)
      (error 'test (format "FAIL: ~a, got ~a, expected ~a" label got expected))))

(check "alias mutation should affect shared object"
       (interpret (path->string prog) 'Holder)
       7)

(displayln "Step 17a tests all passed.")