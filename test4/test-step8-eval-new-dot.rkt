#lang racket

(require "../eval.rkt")
(require "../state.rkt")
(require "../oo-runtime.rkt")

(define (check label got expected)
  (if (equal? got expected)
      (printf "PASS: ~a => ~a\n" label got)
      (error 'test (format "FAIL: ~a, got ~a, expected ~a" label got expected))))

(define ast
  '((class A ()
      ((var x 5)
       (var y 2)
       (static-function main ()
         ((return 0)))))
    (class B (extends A)
      ((var y 22)
       (var z 3)))
    (class C (extends B)
      ((var y 222)
       (static-function main ()
         ((return 1)))))))

(define table (build-class-table ast))
(register-class-table! table)
(set-current-class-context! 'ignored)

(define st0 (empty-state))

;; new A()
(define newA-vs (M_expression-vs '(new A) st0))
(define a-obj (vs-value newA-vs))

(check "new A returns instance"
       (instance? a-obj)
       #t)

(check "new A runtime class"
       (instance-closure-runtime-class a-obj)
       'A)

(check "new A has x = 5"
       (lookup-field table a-obj 'x 'ignored)
       5)

;; a.x
(define st1 (state-declare/init 'a a-obj st0))

(check "a.x"
       (vs-value (M_expression-vs '(dot a x) st1))
       5)

;; (new A).x
(check "(new A).x"
       (vs-value (M_expression-vs '(dot (new A) x) st0))
       5)

;; current-class context matters
(define c-obj (vs-value (M_expression-vs '(new C) st0)))
(define st2 (state-declare/init 'c c-obj st0))

(set-current-class-context! 'ignored)
(check "ignored context uses runtime class"
       (vs-value (M_expression-vs '(dot c y) st2))
       222)

(set-current-class-context! 'B)
(check "B context sees B.y"
       (vs-value (M_expression-vs '(dot c y) st2))
       22)

(set-current-class-context! 'A)
(check "A context sees A.y"
       (vs-value (M_expression-vs '(dot c y) st2))
       2)

(set-current-class-context! 'ignored)

(displayln "Step 8 tests all passed.")