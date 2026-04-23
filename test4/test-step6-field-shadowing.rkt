#lang racket

(require "../oo-runtime.rkt")

(define (check label got expected)
  (if (equal? got expected)
      (printf "PASS: ~a => ~a\n" label got)
      (error 'test (format "FAIL: ~a, got ~a, expected ~a" label got expected))))

(define ast
  '((class A ()
      ((var x 1)
       (var y 2)))
    (class B (extends A)
      ((var y 22)
       (var z 3)))
    (class C (extends B)
      ((var y 222)
       (var w 4)))))

(define table (build-class-table ast))
(define c1 (make-instance table 'C))

;; lookup by class view
(check "A view of y on C instance"
       (lookup-field table c1 'y 'A)
       2)

(check "B view of y on C instance"
       (lookup-field table c1 'y 'B)
       22)

(check "C view of y on C instance"
       (lookup-field table c1 'y 'C)
       222)

(check "B view of x on C instance"
       (lookup-field table c1 'x 'B)
       1)

(check "ignored view defaults to runtime class C"
       (lookup-field table c1 'y 'ignored)
       222)

;; update only B.y
(define c2 (update-field table c1 'y 999 'B))

(check "after update, B view of y"
       (lookup-field table c2 'y 'B)
       999)

(check "after update, A view of y unchanged"
       (lookup-field table c2 'y 'A)
       2)

(check "after update, C view of y unchanged"
       (lookup-field table c2 'y 'C)
       222)

(displayln "Step 6 field shadowing tests all passed.")