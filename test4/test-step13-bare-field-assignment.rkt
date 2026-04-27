#lang racket

(require racket/runtime-path)
(require "../interp.rkt")

(define-runtime-path prog "step13-bare-field-assignment.txt")

(define (check label got expected)
  (if (equal? got expected)
      (printf "PASS: ~a => ~a\n" label got)
      (error 'test (format "FAIL: ~a, got ~a, expected ~a" label got expected))))

(check "Rect main: bare field assignments in same class"
       (interpret (path->string prog) 'Rect)
       32)

(check "C main: inherited owner-class method updates B.y, not C.y"
       (interpret (path->string prog) 'C)
       222)

(check "D main: method defined in D updates D.y"
       (interpret (path->string prog) 'D)
       888)

(displayln "Step 13 tests all passed.")