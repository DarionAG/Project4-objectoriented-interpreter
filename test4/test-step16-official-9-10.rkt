#lang racket

(require racket/runtime-path)
(require "../interp.rkt")

(define-runtime-path prog "step16-official-9-10.txt")

(define (check label got expected)
  (if (equal? got expected)
      (printf "PASS: ~a => ~a\n" label got)
      (error 'test (format "FAIL: ~a, got ~a, expected ~a" label got expected))))

(check "Official Test 9: run Square main"
       (interpret (path->string prog) 'Square)
       32)

(check "Official Test 10: run List main"
       (interpret (path->string prog) 'List)
       15)

(displayln "Step 16 tests all passed.")