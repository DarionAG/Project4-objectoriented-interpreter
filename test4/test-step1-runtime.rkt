#lang racket

(require "../oo-runtime.rkt")

(define (check label got expected)
  (if (equal? got expected)
      (printf "PASS: ~a => ~a\n" label got)
      (error 'test (format "FAIL: ~a, got ~a, expected ~a" label got expected))))

(define sample-ast
  '((class A ()
      ((var x 5)
       (var y 10)
       (function add (g h)
         ((return (+ g h))))
       (static-function main ()
         ((return 0)))))
    (class B (extends A)
      ((var z 3)
       (function m ()
         ((return z)))))))

(define table (build-class-table sample-ast))

(define classA (lookup-class table 'A))
(define classB (lookup-class table 'B))

(check "class A exists" (class-exists? table 'A) #t)
(check "class B exists" (class-exists? table 'B) #t)
(check "class C does not exist" (class-exists? table 'C) #f)

(check "A superclass" (class-closure-super classA) '())
(check "B superclass" (class-closure-super classB) 'A)

(check "A fields"
       (class-closure-field-defs classA)
       '((x 5) (y 10)))

(check "B fields"
       (class-closure-field-defs classB)
       '((z 3)))

(define main-entry (assoc 'main (class-closure-method-table classA)))
(define add-entry  (assoc 'add  (class-closure-method-table classA)))

(check "main exists" (not (false? main-entry)) #t)
(check "add exists"  (not (false? add-entry)) #t)

(check "main is static"
       (method-closure-static? (cdr main-entry))
       #t)

(check "add is not static"
       (method-closure-static? (cdr add-entry))
       #f)

(check "add owner class"
       (method-closure-owner-class (cdr add-entry))
       'A)

(displayln "Step 1 tests all passed.")