#lang racket

(require "../oo-runtime.rkt")

(define (check label got expected)
  (if (equal? got expected)
      (printf "PASS: ~a => ~a\n" label got)
      (error 'test (format "FAIL: ~a, got ~a, expected ~a" label got expected))))

(define sample-ast
  '((class A ()
      ((var x 5)
       (var y)
       (function add (g h)
         ((return (+ g h))))
       (static-function main ()
         ((return 0)))))
    (class B (extends A)
      ((var z 3)
       (function m ()
         ((return z)))))))

(define table (build-class-table sample-ast))

(define a1 (make-instance table 'A))
(define b1 (make-instance table 'B))

(check "a1 is instance" (instance? a1) #t)
(check "b1 is instance" (instance? b1) #t)

(check "a1 runtime class"
       (instance-closure-runtime-class a1)
       'A)

(check "b1 runtime class"
       (instance-closure-runtime-class b1)
       'B)

(check "a1 x"
       (lookup-field table a1 'x 'ignored)
       5)

(check "a1 y default"
       (lookup-field table a1 'y 'ignored)
       0)

(check "b1 inherited x"
       (lookup-field table b1 'x 'ignored)
       5)

(check "b1 inherited y"
       (lookup-field table b1 'y 'ignored)
       0)

(check "b1 own z"
       (lookup-field table b1 'z 'ignored)
       3)

(define a2 (update-field table a1 'x 99 'ignored))

(check "updated a2 x"
       (lookup-field table a2 'x 'ignored)
       99)

(check "original a1 x unchanged"
       (lookup-field table a1 'x 'ignored)
       5)

(check "updated object keeps class"
       (instance-closure-runtime-class a2)
       'A)

(displayln "Step 2 tests all passed.")