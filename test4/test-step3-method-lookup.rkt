#lang racket

(require "../oo-runtime.rkt")

(define (check label got expected)
  (if (equal? got expected)
      (printf "PASS: ~a => ~a\n" label got)
      (error 'test (format "FAIL: ~a, got ~a, expected ~a" label got expected))))

(define sample-ast
  '((class A ()
      ((function ping ()
         ((return 1)))
       (function onlyA ()
         ((return 10)))
       (static-function main ()
         ((return 0)))))
    (class B (extends A)
      ((function ping ()
         ((return 2)))
       (function onlyB ()
         ((return 20)))))
    (class C (extends B)
      ((function ping ()
         ((return 3)))))))

(define table (build-class-table sample-ast))

(check "superclass of A"
       (superclass-of table 'A)
       '())

(check "superclass of B"
       (superclass-of table 'B)
       'A)

(check "superclass of C"
       (superclass-of table 'C)
       'B)

(define pingA (lookup-method table 'A 'ping '()))
(define pingB (lookup-method table 'B 'ping '()))
(define pingC (lookup-method table 'C 'ping '()))

(check "A ping owner"
       (method-closure-owner-class pingA)
       'A)

(check "B ping owner"
       (method-closure-owner-class pingB)
       'B)

(check "C ping owner"
       (method-closure-owner-class pingC)
       'C)

(define onlyA-from-C
  (lookup-method table 'C 'onlyA '()))

(check "C inherits onlyA from A"
       (method-closure-owner-class onlyA-from-C)
       'A)

(define onlyB-from-C
  (lookup-method table 'C 'onlyB '()))

(check "C inherits onlyB from B"
       (method-closure-owner-class onlyB-from-C)
       'B)

(define super-ping-from-C
  (lookup-method table 'C 'ping (superclass-of table 'C)))

(check "super ping from C starts at B"
       (method-closure-owner-class super-ping-from-C)
       'B)

(define super-ping-from-B
  (lookup-method table 'B 'ping (superclass-of table 'B)))

(check "super ping from B starts at A"
       (method-closure-owner-class super-ping-from-B)
       'A)

(check "ping is not static"
       (method-closure-static? pingC)
       #f)

(displayln "Step 3 tests all passed.")