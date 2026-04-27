#lang racket

(require racket/runtime-path)
(require "../interp.rkt")

(define-runtime-path prog "step14-super-calls.txt")

(define (check label got expected)
  (if (equal? got expected)
      (printf "PASS: ~a => ~a\n" label got)
      (error 'test (format "FAIL: ~a, got ~a, expected ~a" label got expected))))

(check "C main: chained super calls with dynamic dispatch"
       (interpret (path->string prog) 'C)
       26)

(check "Q main: super mutating method writes back through this"
       (interpret (path->string prog) 'Q)
       9)

(displayln "Step 14 tests all passed.")