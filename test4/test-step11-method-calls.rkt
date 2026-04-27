#lang racket

(require racket/runtime-path)
(require "../interp.rkt")

(define-runtime-path prog "step11-method-calls.txt")

(define (check label got expected)
  (if (equal? got expected)
      (printf "PASS: ~a => ~a\n" label got)
      (error 'test (format "FAIL: ~a, got ~a, expected ~a" label got expected))))

(check "T2 main: method with parameters"
       (interpret (path->string prog) 'T2)
       12)

(check "T3 main: this.x + parameter x"
       (interpret (path->string prog) 'T3)
       125)

(check "T4 main: this.x assignment writes back to receiver"
       (interpret (path->string prog) 'T4)
       36)

(check "T5 main: nested method calls"
       (interpret (path->string prog) 'T5)
       54)

(displayln "Step 11 tests all passed.")