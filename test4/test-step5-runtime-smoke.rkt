#lang racket

(require "../oo-runtime.rkt")

(define (check label got expected)
  (if (equal? got expected)
      (printf "PASS: ~a => ~a\n" label got)
      (error 'test (format "FAIL: ~a, got ~a, expected ~a" label got expected))))

(define (check-error label thunk)
  (define got-error? #f)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (set! got-error? #t)
                     (printf "PASS: ~a => caught error: ~a\n"
                             label
                             (exn-message e)))])
    (thunk))
  (unless got-error?
    (error 'test (format "FAIL: ~a, expected an error but got success" label))))

(define ast
  '((class A ()
      ((var x 5)
       (var y)
       (function ping ()
         ((return x)))
       (function onlyA ()
         ((return 10)))
       (static-function main ()
         ((return 0)))))
    (class B (extends A)
      ((var z 3)
       (function ping ()
         ((return z)))
       (function onlyB ()
         ((return 20)))))
    (class C (extends B)
      ((function ping ()
         ((return 99)))
       (static-function main ()
         ((return 1)))))))

(define table (build-class-table ast))

;; class / superclass
(check "A exists" (class-exists? table 'A) #t)
(check "B superclass" (superclass-of table 'B) 'A)
(check "C superclass" (superclass-of table 'C) 'B)

;; instance / field
(define c1 (make-instance table 'C))
(check "c1 is instance" (instance? c1) #t)
(check "c1 inherited x" (lookup-field table c1 'x 'ignored) 5)
(check "c1 inherited y default" (lookup-field table c1 'y 'ignored) 0)
(check "c1 own z" (lookup-field table c1 'z 'ignored) 3)

(define c2 (update-field table c1 'x 42 'ignored))
(check "updated c2 x" (lookup-field table c2 'x 'ignored) 42)
(check "original c1 x unchanged" (lookup-field table c1 'x 'ignored) 5)

;; method lookup / inheritance / override / super-style start
(define pingC (lookup-method table 'C 'ping '()))
(check "C ping owner" (method-closure-owner-class pingC) 'C)

(define onlyA-from-C (lookup-method table 'C 'onlyA '()))
(check "C inherits onlyA from A" (method-closure-owner-class onlyA-from-C) 'A)

(define onlyB-from-C (lookup-method table 'C 'onlyB '()))
(check "C inherits onlyB from B" (method-closure-owner-class onlyB-from-C) 'B)

(define super-ping-from-C
  (lookup-method table 'C 'ping (superclass-of table 'C)))
(check "super ping from C starts at B"
       (method-closure-owner-class super-ping-from-C)
       'B)

;; main lookup
(define mainC (lookup-main-method table 'C))
(check "mainC owner" (method-closure-owner-class mainC) 'C)
(check "mainC is static" (method-closure-static? mainC) #t)

;; validation still works
(check-error "unknown class in lookup-class"
             (lambda ()
               (lookup-class table 'Missing)))

(check-error "unknown field should fail"
             (lambda ()
               (lookup-field table c1 'nope 'ignored)))

(displayln "Step 5 runtime smoke tests all passed.")