#lang racket

(require racket/runtime-path)
(require "../interp.rkt")

(displayln (procedure-arity interpret))
(define-runtime-path prog "step7-basic-main.txt")

(define (check label got expected)
  (if (equal? got expected)
      (printf "PASS: ~a => ~a\n" label got)
      (error 'test (format "FAIL: ~a, got ~a, expected ~a" label got expected))))

(check "run A main"
       (interpret (path->string prog) 'A)
       5)

(check "run B main"
       (interpret (path->string prog) 'B)
       9)

(displayln "Step 7 tests all passed.")