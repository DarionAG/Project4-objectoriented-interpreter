#lang racket

(require racket/runtime-path)
(require "../interp.rkt")

(define-runtime-path prog "step12-bare-field-read.txt")

(define (check label got expected)
  (if (equal? got expected)
      (printf "PASS: ~a => ~a\n" label got)
      (error 'test (format "FAIL: ~a, got ~a, expected ~a" label got expected))))

(check "C main: bare field read with shadowing"
       (interpret (path->string prog) 'C)
       26)

(check "D main: bare field read in same class"
       (interpret (path->string prog) 'D)
       28)

(displayln "Step 12 tests all passed.")