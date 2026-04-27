#lang racket

(require racket/runtime-path)
(require "../interp.rkt")

(define-runtime-path prog "step9-static-main-objects.txt")

(define (check label got expected)
  (if (equal? got expected)
      (printf "PASS: ~a => ~a\n" label got)
      (error 'test (format "FAIL: ~a, got ~a, expected ~a" label got expected))))

(check "run A main with new/dot"
       (interpret (path->string prog) 'A)
       5)

(check "run C main with runtime-class field view"
       (interpret (path->string prog) 'C)
       222)

(displayln "Step 9 tests all passed.")