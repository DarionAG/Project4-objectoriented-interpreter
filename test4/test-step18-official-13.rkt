#lang racket

(require racket/runtime-path)
(require "../interp.rkt")

(define-runtime-path prog "step18-official-13.txt")

(define (check label got expected)
  (if (equal? got expected)
      (printf "PASS: ~a => ~a\n" label got)
      (error 'test (format "FAIL: ~a, got ~a, expected ~a" label got expected))))

(check "Official Test 13: run C main"
       (interpret (path->string prog) 'C)
       -716)

(displayln "Step 18 tests all passed.")