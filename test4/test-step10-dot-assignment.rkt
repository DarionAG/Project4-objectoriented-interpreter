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

;; ----------------------------
;; basic a.x = 99
;; ----------------------------
(set-current-class-context! 'ignored)

(define st0 (empty-state))
(define a-obj (make-instance table 'A))
(define st1 (state-declare/init 'a a-obj st0))

(define assign-a-vs
  (M_expression-vs '(= (dot a x) 99) st1))

(check "assignment returns rhs value"
       (vs-value assign-a-vs)
       99)

(define st2 (vs-state assign-a-vs))
(define a2 (state-lookup 'a st2))

(check "a.x updated to 99"
       (lookup-field table a2 'x 'ignored)
       99)

(check "a.y unchanged"
       (lookup-field table a2 'y 'ignored)
       2)

;; ----------------------------
;; shadowed field under runtime-class view
;; c.y should update C.y
;; ----------------------------
(define c-obj (make-instance table 'C))
(define st3 (state-declare/init 'c c-obj st0))

(set-current-class-context! 'ignored)
(define assign-c-runtime-vs
  (M_expression-vs '(= (dot c y) 777) st3))

(define st4 (vs-state assign-c-runtime-vs))
(define c2 (state-lookup 'c st4))

(check "runtime-class view updates C.y"
       (lookup-field table c2 'y 'C)
       777)

(check "runtime-class view keeps B.y unchanged"
       (lookup-field table c2 'y 'B)
       22)

(check "runtime-class view keeps A.y unchanged"
       (lookup-field table c2 'y 'A)
       2)

;; ----------------------------
;; class-context view updates B.y
;; ----------------------------
(set-current-class-context! 'B)
(define assign-c-B-vs
  (M_expression-vs '(= (dot c y) 888) st3))

(define st5 (vs-state assign-c-B-vs))
(define c3 (state-lookup 'c st5))

(check "B context updates B.y"
       (lookup-field table c3 'y 'B)
       888)

(check "B context keeps C.y unchanged"
       (lookup-field table c3 'y 'C)
       222)

(check "B context keeps A.y unchanged"
       (lookup-field table c3 'y 'A)
       2)

(set-current-class-context! 'ignored)

(displayln "Step 10 tests all passed.")