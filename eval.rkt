#lang racket

;;;; ***************************************************
;;;; Jianhao Deng
;;;; Darion Gomez
;;;; Chloe de Lamare
;;;; CSDS 345 Spring 2026
;;;; Project 4
;;;; ***************************************************


(require "state.rkt")
(require "oo-runtime.rkt")

(provide
 M_expression
 M_boolean
 M_value
 M_expression-vs
 M_boolean-vs
 M_value-vs
 make-vs
 vs-value
 vs-state
 register-funcall-handler!
 get-current-throw-handler
 set-current-throw-handler!
 register-class-table!
 get-current-class-table
 set-current-class-context!
 get-current-class-context)

;; Helper Functions -----------------------------------------------------------------------

;; abstraction functions
(define operator car)
(define operand1 cadr)
(define operand2 caddr)

;; checks if input is a boolean expression
(define condition?
  (lambda (e)
    (cond
      ((eq? e 'true)  #t)
      ((eq? e 'false) #t)
      (else (and (list? e)
                 (or (eq? (operator e) '&&)
                     (eq? (operator e) '||)
                     (eq? (operator e) '!)
                     (eq? (operator e) '<)
                     (eq? (operator e) '<=)
                     (eq? (operator e) '>)
                     (eq? (operator e) '>=)
                     (eq? (operator e) '==)
                     (eq? (operator e) '!=)))))))

;; checks if operator is binary
(define binary-opp?
  (lambda (e)
    (and (list? e) (= (length e) 3))))

;; checks if operator is unary
(define unary-opp?
  (lambda (e)
    (and (list? e) (= (length e) 2))))

(define assignment-target?
  (lambda (lhs)
    (or (symbol? lhs)
        (and (dot-expression? lhs)
             (symbol? (cadr lhs))))))

(define assignment-expression?
  (lambda (e)
    (and (list? e)
         (= (length e) 3)
         (eq? (operator e) '=)
         (assignment-target? (operand1 e)))))

(define funcall-expression?
  (lambda (e)
    (and (list? e)
         (pair? e)
         (eq? (car e) 'funcall))))

;; returns an error if a variable is unassigned
(define lookup-variable
  (lambda (name st)
    (with-handlers ([exn:fail?
                     (lambda (e1)
                       (if (eq? (get-current-class-context) 'ignored)
                           (raise e1)
                           (with-handlers ([exn:fail?
                                            (lambda (e2)
                                              (raise e1))])
                             (let ([this-obj (state-lookup 'this st)])
                               (if (instance? this-obj)
                                   (lookup-field
                                    (get-current-class-table)
                                    this-obj
                                    name
                                    (get-current-class-context))
                                   (raise e1))))))])
      (state-lookup name st))))

(define new-expression?
  (lambda (e)
    (and (list? e)
         (pair? e)
         (eq? (car e) 'new)
         (>= (length e) 2)
         (symbol? (cadr e)))))

(define dot-expression?
  (lambda (e)
    (and (list? e)
         (= (length e) 3)
         (eq? (car e) 'dot)
         (symbol? (caddr e)))))

(define current-class-table 'UNSET)

(define register-class-table!
  (lambda (class-table)
    (set! current-class-table class-table)))

(define get-current-class-table
  (lambda ()
    (if (eq? current-class-table 'UNSET)
        (error 'eval "Class table has not been registered yet")
        current-class-table)))

(define current-class-context 'ignored)

(define set-current-class-context!
  (lambda (class-name)
    (set! current-class-context class-name)))

(define get-current-class-context
  (lambda ()
    current-class-context))


(define get-current-throw-handler
  (lambda ()
    current-throw-handler))

(define lang-bool->racket
  (lambda (v)
    (cond
      ((eq? v 'true) #t)
      ((eq? v 'false) #f)
      ((boolean? v) v)
      (else (error 'M_boolean "Expected boolean value, got: ~s" v)))))

(define make-vs
  (lambda (v s)
    (cons v s)))

(define vs-value car)
(define vs-state cdr)



(define M_expression
  (lambda (expression st)
    (vs-value (M_expression-vs expression st))))

(define M_boolean
  (lambda (expression st)
    (vs-value (M_boolean-vs expression st))))

(define M_value
  (lambda (expression st)
    (vs-value (M_value-vs expression st))))

;; ----------------------------------------------------------------------------------------

(define M_expression-vs
  (lambda (expression st)
    (cond
      ((assignment-expression? expression)
       (M_assignmentexpr-vs expression st))
      ((condition? expression)
       (M_boolean-vs expression st))
      (else
       (M_value-vs expression st)))))

;; assignment expression: (= x expr)
;; returns assigned value + updated state
(define M_assignmentexpr-vs
  (lambda (expression st)
    (let* ([lhs (operand1 expression)]
           [rhs-vs (M_expression-vs (operand2 expression) st)]
           [rhs-val (vs-value rhs-vs)]
           [rhs-state (vs-state rhs-vs)])
      (cond
        [(symbol? lhs)
         (assign-symbol-or-field-vs lhs rhs-val rhs-state)]

        [(dot-expression? lhs)
         (M_dot-assignment-vs lhs rhs-val rhs-state)]

        [else
         (error 'M_assignmentexpr-vs
                "Unsupported assignment target: ~s"
                lhs)]))))

(define assign-symbol-or-field-vs
  (lambda (lhs rhs-val rhs-state)
    (with-handlers ([exn:fail?
                     (lambda (e1)
                       (if (eq? (get-current-class-context) 'ignored)
                           (raise e1)
                           (with-handlers ([exn:fail?
                                            (lambda (e2)
                                              (raise e1))])
                             (let* ([this-obj (state-lookup 'this rhs-state)])
                               (if (instance? this-obj)
                                   (let* ([updated-this
                                           (update-field
                                            (get-current-class-table)
                                            this-obj
                                            lhs
                                            rhs-val
                                            (get-current-class-context))]
                                          [new-state
                                           (state-update 'this updated-this rhs-state)])
                                     (make-vs rhs-val new-state))
                                   (raise e1))))))])
      (make-vs rhs-val
               (state-update lhs rhs-val rhs-state)))))


(define M_boolean-vs
  (lambda (expression st)
    (cond
      ((eq? expression 'true)
       (make-vs #t st))

      ((eq? expression 'false)
       (make-vs #f st))

      ((symbol? expression)
       (make-vs (lang-bool->racket (lookup-variable expression st)) st))

      ;; short-circuit &&
      ((and (binary-opp? expression) (eq? '&& (operator expression)))
       (let* ([left-vs (M_boolean-vs (operand1 expression) st)]
              [left-val (vs-value left-vs)]
              [st1 (vs-state left-vs)])
         (if left-val
             (M_boolean-vs (operand2 expression) st1)
             (make-vs #f st1))))

      ;; short-circuit ||
      ((and (binary-opp? expression) (eq? '|| (operator expression)))
       (let* ([left-vs (M_boolean-vs (operand1 expression) st)]
              [left-val (vs-value left-vs)]
              [st1 (vs-state left-vs)])
         (if left-val
             (make-vs #t st1)
             (M_boolean-vs (operand2 expression) st1))))

      ((and (unary-opp? expression) (eq? '! (operator expression)))
       (let* ([arg-vs (M_boolean-vs (operand1 expression) st)]
              [arg-val (vs-value arg-vs)]
              [st1 (vs-state arg-vs)])
         (make-vs (not arg-val) st1)))

      ((and (binary-opp? expression) (eq? '< (operator expression)))
       (let* ([left-vs (M_expression-vs (operand1 expression) st)]
              [left-val (vs-value left-vs)]
              [st1 (vs-state left-vs)]
              [right-vs (M_expression-vs (operand2 expression) st1)]
              [right-val (vs-value right-vs)]
              [st2 (vs-state right-vs)])
         (make-vs (< left-val right-val) st2)))

      ((and (binary-opp? expression) (eq? '<= (operator expression)))
       (let* ([left-vs (M_expression-vs (operand1 expression) st)]
              [left-val (vs-value left-vs)]
              [st1 (vs-state left-vs)]
              [right-vs (M_expression-vs (operand2 expression) st1)]
              [right-val (vs-value right-vs)]
              [st2 (vs-state right-vs)])
         (make-vs (<= left-val right-val) st2)))

      ((and (binary-opp? expression) (eq? '> (operator expression)))
       (let* ([left-vs (M_expression-vs (operand1 expression) st)]
              [left-val (vs-value left-vs)]
              [st1 (vs-state left-vs)]
              [right-vs (M_expression-vs (operand2 expression) st1)]
              [right-val (vs-value right-vs)]
              [st2 (vs-state right-vs)])
         (make-vs (> left-val right-val) st2)))

      ((and (binary-opp? expression) (eq? '>= (operator expression)))
       (let* ([left-vs (M_expression-vs (operand1 expression) st)]
              [left-val (vs-value left-vs)]
              [st1 (vs-state left-vs)]
              [right-vs (M_expression-vs (operand2 expression) st1)]
              [right-val (vs-value right-vs)]
              [st2 (vs-state right-vs)])
         (make-vs (>= left-val right-val) st2)))

      ((and (binary-opp? expression) (eq? '== (operator expression)))
       (let* ([left-vs (M_expression-vs (operand1 expression) st)]
              [left-val (vs-value left-vs)]
              [st1 (vs-state left-vs)]
              [right-vs (M_expression-vs (operand2 expression) st1)]
              [right-val (vs-value right-vs)]
              [st2 (vs-state right-vs)])
         (make-vs (equal? left-val right-val) st2)))

      ((and (binary-opp? expression) (eq? '!= (operator expression)))
       (let* ([left-vs (M_expression-vs (operand1 expression) st)]
              [left-val (vs-value left-vs)]
              [st1 (vs-state left-vs)]
              [right-vs (M_expression-vs (operand2 expression) st1)]
              [right-val (vs-value right-vs)]
              [st2 (vs-state right-vs)])
         (make-vs (not (equal? left-val right-val)) st2)))

      (else
 (let* ([expr-vs (M_expression-vs expression st)]
        [val (vs-value expr-vs)]
        [st1 (vs-state expr-vs)])
   (cond
     [(boolean? val)
      (make-vs val st1)]
     [(or (eq? val 'true) (eq? val 'false))
      (make-vs (lang-bool->racket val) st1)]
     [else
      (error 'M_boolean
             "Invalid boolean expression ~s"
             expression)]))))))
(define M_value-vs
  (lambda (expression st)
    (cond
      ((number? expression)
       (make-vs expression st))

      ((new-expression? expression)
       (M_new-vs expression st))

      ((dot-expression? expression)
       (M_dot-vs expression st))

      ((symbol? expression)
       (make-vs (lookup-variable expression st) st))

      ((funcall-expression? expression)
       (M_funcall-expression expression st))

      ((and (eq? '+ (operator expression)) (binary-opp? expression))
       (let* ([left-vs (M_expression-vs (operand1 expression) st)]
              [left-val (vs-value left-vs)]
              [st1 (vs-state left-vs)]
              [right-vs (M_expression-vs (operand2 expression) st1)]
              [right-val (vs-value right-vs)]
              [st2 (vs-state right-vs)])
         (make-vs (+ left-val right-val) st2)))

      ((and (eq? '- (operator expression)) (binary-opp? expression))
       (let* ([left-vs (M_expression-vs (operand1 expression) st)]
              [left-val (vs-value left-vs)]
              [st1 (vs-state left-vs)]
              [right-vs (M_expression-vs (operand2 expression) st1)]
              [right-val (vs-value right-vs)]
              [st2 (vs-state right-vs)])
         (make-vs (- left-val right-val) st2)))

      ((and (eq? '* (operator expression)) (binary-opp? expression))
       (let* ([left-vs (M_expression-vs (operand1 expression) st)]
              [left-val (vs-value left-vs)]
              [st1 (vs-state left-vs)]
              [right-vs (M_expression-vs (operand2 expression) st1)]
              [right-val (vs-value right-vs)]
              [st2 (vs-state right-vs)])
         (make-vs (* left-val right-val) st2)))

      ((and (eq? '/ (operator expression)) (binary-opp? expression))
       (let* ([left-vs (M_expression-vs (operand1 expression) st)]
              [left-val (vs-value left-vs)]
              [st1 (vs-state left-vs)]
              [right-vs (M_expression-vs (operand2 expression) st1)]
              [right-val (vs-value right-vs)]
              [st2 (vs-state right-vs)])
         (make-vs (quotient left-val right-val) st2)))

      ((and (eq? '% (operator expression)) (binary-opp? expression))
       (let* ([left-vs (M_expression-vs (operand1 expression) st)]
              [left-val (vs-value left-vs)]
              [st1 (vs-state left-vs)]
              [right-vs (M_expression-vs (operand2 expression) st1)]
              [right-val (vs-value right-vs)]
              [st2 (vs-state right-vs)])
         (make-vs (remainder left-val right-val) st2)))

      ((and (eq? '- (operator expression)) (unary-opp? expression))
       (let* ([arg-vs (M_expression-vs (operand1 expression) st)]
              [arg-val (vs-value arg-vs)]
              [st1 (vs-state arg-vs)])
         (make-vs (- arg-val) st1)))

      (else
       (error 'badop "Invalid int expression ~s" expression)))))

(define M_new-vs
  (lambda (expression st)
    (let ([class-name (cadr expression)]
          [args (cddr expression)])
      (if (not (null? args))
          (error 'M_new-vs "Constructors are not supported in the current phase")
          (make-vs
           (make-instance (get-current-class-table) class-name)
           st)))))

(define dot-field-context
  (lambda (receiver-exp)
    (if (and (symbol? receiver-exp)
             (eq? receiver-exp 'this))
        (get-current-class-context)
        'ignored)))

(define M_dot-vs
  (lambda (expression st)
    (let* ([receiver-exp (cadr expression)]
           [obj-vs (M_expression-vs receiver-exp st)]
           [obj (vs-value obj-vs)]
           [st1 (vs-state obj-vs)]
           [field-name (caddr expression)]
           [field-context (dot-field-context receiver-exp)])
      (if (instance? obj)
          (make-vs
           (lookup-field
            (get-current-class-table)
            obj
            field-name
            field-context)
           st1)
          (error 'M_dot-vs
                 "Left side of dot is not an object: ~s"
                 obj)))))

(define M_dot-assignment-vs
  (lambda (lhs rhs-val st)
    (let* ([receiver-exp (cadr lhs)]
           [field-name (caddr lhs)])
      (if (not (symbol? receiver-exp))
          (error 'M_dot-assignment-vs
                 "Dot assignment currently requires a variable receiver: ~s"
                 receiver-exp)
          (let* ([obj (lookup-variable receiver-exp st)]
                 [field-context (dot-field-context receiver-exp)])
            (if (instance? obj)
                (let* ([updated-obj
                        (update-field
                         (get-current-class-table)
                         obj
                         field-name
                         rhs-val
                         field-context)]
                       [new-state
                        (state-update receiver-exp updated-obj st)])
                  (make-vs rhs-val new-state))
                (error 'M_dot-assignment-vs
                       "Left side receiver is not an object: ~s"
                       obj)))))))

;; Placeholder only.
;; Member 2 / later integration will replace this with real function-call execution.

(define current-throw-handler
  (lambda (val st)
    (error 'throw "No throw handler registered for expression context: ~a" val)))

(define set-current-throw-handler!
  (lambda (handler)
    (set! current-throw-handler handler)))

(define current-funcall-handler
  (lambda (expression st)
    (error 'M_funcall-expression
           "funcall handler not registered yet: ~s"
           expression)))

(define register-funcall-handler!
  (lambda (handler)
    (set! current-funcall-handler handler)))

(define M_funcall-expression
  (lambda (expression st)
    (current-funcall-handler expression st)))
