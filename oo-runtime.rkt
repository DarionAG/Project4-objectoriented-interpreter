#lang racket

;;;; ***************************************************
;;;; Jianhao Deng
;;;; Darion Achilles Gomez
;;;; Chloe de Lamare
;;;; CSDS 345 Spring 2026
;;;; Project 4
;;;; ***************************************************

(provide
 (struct-out class-closure)
 (struct-out method-closure)
 (struct-out instance-closure)
 build-class-table
 lookup-class
 class-exists?
 instance?
 make-instance
 lookup-field
 update-field
 superclass-of
 lookup-method
 validate-class-table
 lookup-main-method)

;; ----------------------------
;; Runtime data structures
;; ----------------------------

(struct class-closure (name super field-defs method-table) #:transparent)
(struct method-closure (name params body owner-class static?) #:transparent)
(struct instance-closure (runtime-class field-bindings) #:transparent #:mutable)


(define class-name cadr)
(define class-extends caddr)
(define class-body cadddr)

(define member-kind car)

(define member-name
  (lambda (m)
    (cadr m)))

(define member-params
  (lambda (m)
    (caddr m)))

(define member-body
  (lambda (m)
    (cadddr m)))

(define field-member?
  (lambda (m)
    (and (pair? m)
         (eq? (member-kind m) 'var))))

(define method-member?
  (lambda (m)
    (and (pair? m)
         (or (eq? (member-kind m) 'function)
             (eq? (member-kind m) 'static-function)))))

(define static-method?
  (lambda (m)
    (eq? (member-kind m) 'static-function)))

(define superclass-name
  (lambda (extends-part)
    (if (null? extends-part)
        '()
        (cadr extends-part))))

;; field defs:
;;   (var x)
;;   (var x 5)
;;
;; store as:
;;   (x UNINITIALIZED)
;;   (x 5)
(define field->entry
  (lambda (m)
    (cond
      ((= (length m) 2)
       (list (member-name m) 'UNINITIALIZED))
      ((= (length m) 3)
       (list (member-name m) (caddr m)))
      (else
       (error 'field->entry "Bad field declaration: ~s" m)))))

;; method defs:
;;   (function add (a b) (...))
;;   (static-function main () (...))
(define method->entry
  (lambda (m owner-class)
    (cons (member-name m)
          (method-closure
           (member-name m)
           (member-params m)
           (member-body m)
           owner-class
           (static-method? m)))))

(define build-class-closure
  (lambda (class-ast)
    (let* ([name (class-name class-ast)]
           [super (superclass-name (class-extends class-ast))]
           [body (class-body class-ast)]
           [fields (map field->entry (filter field-member? body))]
           [methods (map (lambda (m) (method->entry m name))
                         (filter method-member? body))])
      (class-closure name super fields methods))))

;; class table format:
;;   ((A . <class-closure>) (B . <class-closure>) ...)
(define build-class-table
  (lambda (program-ast)
    (let loop ([classes program-ast] [table '()])
      (cond
        [(null? classes)
         (validate-class-table (reverse table))]
        [else
         (let* ([class-ast (car classes)]
                [c (build-class-closure class-ast)]
                [name (class-closure-name c)])
           (if (assoc name table)
               (error 'build-class-table "Duplicate class name: ~s" name)
               (loop (cdr classes)
                     (cons (cons name c) table))))]))))

(define lookup-class
  (lambda (class-table class-name)
    (let ([entry (assoc class-name class-table)])
      (if entry
          (cdr entry)
          (error 'lookup-class "Unknown class: ~s" class-name)))))

(define class-exists?
  (lambda (class-table class-name)
    (if (assoc class-name class-table) #t #f)))


(define instance? instance-closure?)

;; ----------------------------
;; Instance / field helpers
;; ----------------------------

(define init-expr->value
  (lambda (init-expr)
    (cond
      [(eq? init-expr 'UNINITIALIZED) 0]
      [(number? init-expr) init-expr]
      [(boolean? init-expr) init-expr]
      [(eq? init-expr 'true) #t]
      [(eq? init-expr 'false) #f]
      [else
       (error 'make-instance
              "Only constant/default field initializers are supported for now: ~s"
              init-expr)])))

(define collect-owned-field-defs
  (lambda (class-table class-name)
    (let* ([c (lookup-class class-table class-name)]
           [super (class-closure-super c)]
           [own-fields
            (map (lambda (fd)
                   (list class-name (car fd) (cadr fd)))
                 (class-closure-field-defs c))])
      (if (null? super)
          own-fields
          (append (collect-owned-field-defs class-table super)
                  own-fields)))))

(define owned-field-def->binding
  (lambda (owned-field-def)
    (list (list (car owned-field-def) (cadr owned-field-def))
          (init-expr->value (caddr owned-field-def)))))

(define make-instance
  (lambda (class-table class-name)
    (instance-closure
     class-name
     (map owned-field-def->binding
          (collect-owned-field-defs class-table class-name)))))

(define field-start-class
  (lambda (instance current-class)
    (if (or (null? current-class)
            (eq? current-class 'ignored))
        (instance-closure-runtime-class instance)
        current-class)))

(define class-defines-field?
  (lambda (class-table class-name field-name)
    (let* ([c (lookup-class class-table class-name)]
           [entry (assoc field-name (class-closure-field-defs c))])
      (if entry #t #f))))

(define find-field-owner
  (lambda (class-table start-class field-name)
    (cond
      [(null? start-class) #f]
      [(class-defines-field? class-table start-class field-name) start-class]
      [else
       (find-field-owner class-table
                         (superclass-of class-table start-class)
                         field-name)])))

(define binding-entry
  (lambda (bindings owner field-name)
    (assoc (list owner field-name) bindings)))

(define lookup-field
  (lambda (class-table instance field-name current-class)
    (let* ([start-class (field-start-class instance current-class)]
           [owner (find-field-owner class-table start-class field-name)])
      (if owner
          (let ([entry (binding-entry
                        (instance-closure-field-bindings instance)
                        owner
                        field-name)])
            (if entry
                (cadr entry)
                (error 'lookup-field
                       "Missing binding for field ~s declared in ~s"
                       field-name
                       owner)))
          (error 'lookup-field
                 "Unknown field ~s in class view ~s for instance of ~s"
                 field-name
                 start-class
                 (instance-closure-runtime-class instance))))))

(define update-field-bindings
  (lambda (bindings owner field-name new-value)
    (cond
      [(null? bindings)
       (error 'update-field
              "Missing binding for field ~s declared in ~s"
              field-name
              owner)]
      [(equal? (caar bindings) (list owner field-name))
       (cons (list (list owner field-name) new-value)
             (cdr bindings))]
      [else
       (cons (car bindings)
             (update-field-bindings (cdr bindings)
                                    owner
                                    field-name
                                    new-value))])))

(define update-field
  (lambda (class-table instance field-name new-value current-class)
    (let* ([start-class (field-start-class instance current-class)]
           [owner (find-field-owner class-table start-class field-name)])
      (if owner
          (begin
            (set-instance-closure-field-bindings!
             instance
             (update-field-bindings
              (instance-closure-field-bindings instance)
              owner
              field-name
              new-value))
            instance)
          (error 'update-field
                 "Unknown field ~s in class view ~s for instance of ~s"
                 field-name
                 start-class
                 (instance-closure-runtime-class instance))))))


;; ----------------------------
;; Inheritance / method lookup
;; ----------------------------

(define superclass-of
  (lambda (class-table class-name)
    (class-closure-super
     (lookup-class class-table class-name))))

(define lookup-method-in-one-class
  (lambda (class-table class-name method-name)
    (let* ([c (lookup-class class-table class-name)]
           [entry (assoc method-name
                         (class-closure-method-table c))])
      (if entry
          (cdr entry)
          #f))))

(define lookup-method-from-class
  (lambda (class-table start-class method-name)
    (let ([m (lookup-method-in-one-class class-table start-class method-name)])
      (cond
        [m m]
        [(null? (superclass-of class-table start-class)) #f]
        [else
         (lookup-method-from-class
          class-table
          (superclass-of class-table start-class)
          method-name)]))))

;; runtime-class:
;;   the true/runtime class of the receiver object
;;
;; start-class:
;;   where lookup should begin
;;   - pass '() for normal dynamic lookup (start from runtime-class)
;;   - pass (superclass-of table current-class) for super lookup
(define lookup-method
  (lambda (class-table runtime-class method-name start-class)
    (let* ([actual-start
            (if (null? start-class)
                runtime-class
                start-class)]
           [m (lookup-method-from-class
               class-table
               actual-start
               method-name)])
      (if m
          m
          (error 'lookup-method
                 "Unknown method ~s starting from class ~s"
                 method-name
                 actual-start)))))


;; ----------------------------
;; Class-table validation / main lookup
;; ----------------------------

(define validate-class-table
  (lambda (class-table)
    (for-each
     (lambda (entry)
       (let* ([name (car entry)]
              [c (cdr entry)]
              [super (class-closure-super c)])
         (cond
           [(null? super) (void)]
           [(eq? name super)
            (error 'validate-class-table
                   "Class ~s cannot extend itself"
                   name)]
           [(class-exists? class-table super) (void)]
           [else
            (error 'validate-class-table
                   "Unknown superclass ~s for class ~s"
                   super
                   name)])))
     class-table)
    class-table))

(define lookup-main-method
  (lambda (class-table class-name)
    (let ([m (lookup-method-in-one-class class-table class-name 'main)])
      (cond
        [(not m)
         (error 'lookup-main-method
                "Class ~s has no main method"
                class-name)]
        [(not (method-closure-static? m))
         (error 'lookup-main-method
                "main in class ~s must be static"
                class-name)]
        [else m]))))

