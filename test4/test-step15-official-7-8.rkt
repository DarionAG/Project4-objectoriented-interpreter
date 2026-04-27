#lang racket

(require racket/runtime-path)
(require "../interp.rkt")

(define-runtime-path prog "step15-official-7-8.txt")

(define (check label got expected)
  (if (equal? got expected)
      (printf "PASS: ~a => ~a\n" label got)
      (error 'test (format "FAIL: ~a, got ~a, expected ~a" label got expected))))

(check "Official Test 7: run C main"
       (interpret (path->string prog) 'C)
       26)

(check "Official Test 8: run Square main"
       (interpret (path->string prog) 'Square)
       117)

(displayln "Step 15 tests all passed.")