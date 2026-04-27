#lang racket

(require racket/runtime-path)
(require "../interp.rkt")

(define-runtime-path prog "step17-official-11-12.txt")

(define (check label got expected)
  (if (equal? got expected)
      (printf "PASS: ~a => ~a\n" label got)
      (error 'test (format "FAIL: ~a, got ~a, expected ~a" label got expected))))

(check "Official Test 11: run List main"
       (interpret (path->string prog) 'List)
       123456)

(check "Official Test 12: run List2 main"
       (interpret (path->string prog) 'List2)
       5285)

(displayln "Step 17 tests all passed.")