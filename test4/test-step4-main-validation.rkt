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

;; ----------------------------
;; valid program
;; ----------------------------
(define good-ast
  '((class A ()
      ((static-function main ()
         ((return 0)))
       (function ping ()
         ((return 1)))))
    (class B (extends A)
      ((function pong ()
         ((return 2)))))))

(define good-table (build-class-table good-ast))
(define mainA (lookup-main-method good-table 'A))

(check "mainA owner"
       (method-closure-owner-class mainA)
       'A)

(check "mainA is static"
       (method-closure-static? mainA)
       #t)

;; B does not define its own main
(check-error "B has no own main"
             (lambda ()
               (lookup-main-method good-table 'B)))

;; ----------------------------
;; bad superclass
;; ----------------------------
(define bad-super-ast
  '((class X (extends Missing)
      ((static-function main ()
         ((return 0)))))))

(check-error "unknown superclass should fail"
             (lambda ()
               (build-class-table bad-super-ast)))

;; ----------------------------
;; self inheritance
;; ----------------------------
(define self-extend-ast
  '((class Self (extends Self)
      ((static-function main ()
         ((return 0)))))))

(check-error "self inheritance should fail"
             (lambda ()
               (build-class-table self-extend-ast)))

;; ----------------------------
;; non-static main
;; ----------------------------
(define bad-main-ast
  '((class Bad ()
      ((function main ()
         ((return 0)))))))

(define bad-main-table (build-class-table bad-main-ast))

(check-error "non-static main should fail"
             (lambda ()
               (lookup-main-method bad-main-table 'Bad)))

(displayln "Step 4 tests all passed.")