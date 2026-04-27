#lang racket

;;;; ***************************************************
;;;; Jianhao Deng
;;;; Darion Achilles Gomez
;;;; Chloe de Lamare
;;;; CSDS 345 Spring 2026
;;;; Project 4
;;;; ***************************************************


(require "state.rkt")
(require "eval.rkt")
(require "classParser.rkt")
(require "oo-runtime.rkt")

(provide interpret M_statementlist-cps M_statement)

;interpret
(define interpret
  (lambda (filename classname)
    (let* ([ast (parser filename)]
           [class-table (build-class-table ast)]
           [main-method (lookup-main-method class-table classname)])
      (register-class-table! class-table)
      (set-current-class-context! 'ignored)
      (run-static-method-body main-method))))



(define run-static-method-body
  (lambda (method)
    (if (not (null? (method-closure-params method)))
        (error 'interpret "main should not take parameters in the current phase")
        (call/cc
         (lambda (return-escape)
           (M_statementlist-cps
            (method-closure-body method)
            (empty-state)
            (lambda (finished-state)
              (return-escape 'undefined))
            (lambda (ret-val ret-state)
              (return-escape ret-val))
            (lambda (st)
              (error "break used outside loop"))
            (lambda (st)
              (error "continue used outside loop"))
            (lambda (val st)
              (error (format "Uncaught exception: ~a" val)))))))))

;abstractions for M_blockofcode
(define beginningof car)
(define bodyof cdr)

; M_blockofcode
(define M_blockofcode
  (lambda (block state next return break continue throw)
    (let ((inner-state (push-layer state)))
      (M_statementlist-cps
       (bodyof block)
       inner-state
       (lambda (finished-state)
         (next (pop-layer finished-state)))
       (lambda (val return-state)
         (return val (pop-layer return-state)))
       (lambda (break-state)
         (break (pop-layer break-state)))
       (lambda (continue-state)
         (continue (pop-layer continue-state)))
       (lambda (val throw-state)
         (throw val (pop-layer throw-state)))))))

; abstractions for M_statementlist
(define empty? null?)
(define statementof car)
(define statementlistof cdr)
(define statement-statementlist? pair?)

(define with-throw-handler
  (lambda (throw thunk)
    (set-current-throw-handler! throw)
    (thunk)))

; M_statementlist-cps
; statementlist state -> state
(define M_statementlist-cps
  (lambda (statementlist state next return break continue throw)
    (cond
      ((empty? statementlist) (next state)) ; If the statementlist is empty, then no changes to state occur
      (else (M_statement (statementof statementlist) state
                         (lambda (new-state)
                           (M_statementlist-cps
                            (statementlistof statementlist) new-state next return break continue throw))
                         return break continue throw)))))


(define M_global_statement
  (lambda (statement state next return break continue throw)
    (cond
      ((eq? (keyword statement) 'var)
       (if (= (length statement) 3)
           (M_declare2 (name-of statement)
                       (expression-of-statement statement)
                       state next return break continue throw)
           (M_declare1 (name-of statement)
                       state next return break continue throw)))
      ((eq? (keyword statement) 'function)
       (M_function-definition statement state next return break continue throw))
      (else
       (error 'global
              "Only global var declarations and function definitions are allowed at top level: ~s"
              statement)))))

(define M_global_statementlist
  (lambda (statementlist state fun-names next return break continue throw)
    (cond
      ((null? statementlist)
       (next (refresh-global-function-closures state fun-names)))

      (else
       (let ([stmt (car statementlist)])
         (M_global_statement
          stmt
          state
          (lambda (new-state)
            (M_global_statementlist
             (cdr statementlist)
             new-state
             (if (eq? (keyword stmt) 'function)
                 (cons (function-name stmt) fun-names)
                 fun-names)
             next return break continue throw))
          return break continue throw))))))

(define refresh-global-function-closures
  (lambda (state names)
    (cond
      ((null? names) state)
      (else
       (let* ([fname (car names)]
              [old-closure (state-lookup fname state)]
              [new-closure
               (make-closure
                (closure-params old-closure)
                (closure-body old-closure)
                state)])
         (refresh-global-function-closures
          (state-update fname new-closure state)
          (cdr names)))))))


; abstractions for M_statement
(define keyword car)
(define name-of cadr)
(define name-of-assignment cadr)
(define expression-of-statement caddr)
(define expression cadr)

;; function / closure helpers
(define function-name cadr)
(define function-params caddr)
(define function-body cadddr)

(define funcall-name cadr)
(define funcall-args cddr)

(define make-closure
  (lambda (params body env)
    (list 'closure params body env)))

(define closure? 
  (lambda (v)
    (and (list? v)
         (>= (length v) 4)
         (eq? (car v) 'closure))))

(define closure-params cadr)
(define closure-body caddr)
(define closure-env cadddr)

; M_statement-cps
; statement state -> state
(define M_statement
  (lambda (statement state next return break continue throw)
    (cond
      ((eq? (keyword statement) 'begin) (M_blockofcode statement state next return break continue throw))
      ((eq? (keyword statement) 'var) (if (= (length statement) 3) ; if 'var is present we know its a declare statement, additionally if '= is present we must consider expression
                                          (M_declare2 (name-of statement) (expression-of-statement statement)
                                                      state next return break continue throw) ; use diff declare to handle expression
                                          (M_declare1 (name-of statement)
                                                      state next return break continue throw)))
      ((eq? (keyword statement) 'return) (M_return (expression statement) state next return break continue throw)) ;return - change to return continuation?
      ((eq? (keyword statement) '=) (M_assign statement state next return break continue throw))
      ((eq? (keyword statement) 'if) (M_if statement state next return break continue throw))
      ((eq? (keyword statement) 'while) (M_while statement state next return break continue throw))
      ((eq? (keyword statement) 'break) (M_break state next return break continue throw))
      ((eq? (keyword statement) 'continue) (M_continue state next return break continue throw))
      ((eq? (keyword statement) 'throw) (M_throw statement state next return break continue throw))
      ((eq? (keyword statement) 'try)   (M_try   statement state next return break continue throw))
      ((eq? (keyword statement) 'funcall) (M_funcall-statement statement state next return break continue throw))
      ((eq? (keyword statement) 'function) (M_function-definition statement state next return break continue throw))
      (else (error 'badop "Invalid statement form: ~s" statement)))))


;M_declare1 - declares a variable
; returns a new state for the new variable declared
(define M_declare1
  (lambda (name state next return break continue throw)
    (next (state-declare name state))))

;M_declare2 - declares a variable while with a value
; returns a new state for the new variable declared
(define M_declare2
  (lambda (name expression state next return break continue throw)
    (with-throw-handler
        throw
      (lambda ()
        (let* ([rhs-vs (M_expression-vs expression state)]
               [rhs-val (vs-value rhs-vs)]
               [rhs-state (vs-state rhs-vs)])
          (next (state-declare/init name rhs-val rhs-state)))))))

(define M_assign
  (lambda (statement state next return break continue throw)
    (with-throw-handler
        throw
      (lambda ()
        (let* ([assign-vs (M_expression-vs statement state)]
               [new-state (vs-state assign-vs)])
          (next new-state))))))

(define M_return 
  (lambda (expression state next return break continue throw)
    (with-throw-handler
        throw
      (lambda ()
        (let* ([ans-vs (M_expression-vs expression state)]
               [ans-val (vs-value ans-vs)]
               [ans-state (vs-state ans-vs)])
          (return (convert-bool ans-val) ans-state))))))

(define convert-bool
  (lambda (v)
    (if (boolean? v)
        (if v 'true 'false)
        v)))

(define M_if
  (lambda (statement state next return break continue throw)
    (with-throw-handler
        throw
      (lambda ()
        (let* ([cond-expr (cadr statement)]
               [then-stmt (caddr statement)]
               [has-else? (>= (length statement) 4)]
               [cond-vs (M_boolean-vs cond-expr state)]
               [cond-val (vs-value cond-vs)]
               [cond-state (vs-state cond-vs)])
          (if cond-val
              (M_statement then-stmt cond-state next return break continue throw)
              (if has-else?
                  (M_statement (cadddr statement) cond-state next return break continue throw)
                  (next cond-state))))))))

(define M_while
  (lambda (statement state next return break continue throw)
    (let ([cond-expr (cadr statement)]
          [body-stmt (caddr statement)])
      (let loop ([st state])
        (with-throw-handler
         throw
         (lambda ()
           (let* ([cond-vs (M_boolean-vs cond-expr st)]
                  [cond-val (vs-value cond-vs)]
                  [cond-state (vs-state cond-vs)])
             (if cond-val
                 (M_statement body-stmt
                              cond-state
                              (lambda (new-state)
                                (loop new-state))
                              return
                              (lambda (break-state)
                                (next break-state))
                              (lambda (continue-state)
                                (loop continue-state))
                              throw)
                 (next cond-state)))))))))

(define M_break
  (lambda (state next return break continue throw)
    (break state)))

(define M_continue
  (lambda (state next return break continue throw)
    (continue state)))

(define M_throw
  (lambda (statement state next return break continue throw)
    (with-throw-handler
     throw
     (lambda ()
       (let* ([ans-vs (M_expression-vs (cadr statement) state)]
              [ans-val (vs-value ans-vs)]
              [ans-state (vs-state ans-vs)])
         (throw ans-val ans-state))))))

(define M_funcall-statement
  (lambda (statement state next return break continue throw)
    (let ([ans-vs (M_expression-vs statement state)])
      (next (vs-state ans-vs)))))

(define M_function-definition
  (lambda (statement state next return break continue throw)
    (let* ([closure0
            (make-closure
             (function-params statement)
             (function-body statement)
             state)]
           [state1
            (state-declare/init
             (function-name statement)
             closure0
             state)]
           [closure1
            (make-closure
             (function-params statement)
             (function-body statement)
             state1)]
           [state2
            (state-update
             (function-name statement)
             closure1
             state1)])
      (next state2))))

(define eval-actuals-vs
  (lambda (args state)
    (cond
      ((null? args) (make-vs '() state))
      (else
       (let* ([first-vs (M_expression-vs (car args) state)]
              [first-val (vs-value first-vs)]
              [st1 (vs-state first-vs)]
              [rest-vs (eval-actuals-vs (cdr args) st1)])
         (make-vs (cons first-val (vs-value rest-vs))
                  (vs-state rest-vs)))))))

(define bind-params
  (lambda (params values state)
    (cond
      ((and (null? params) (null? values)) state)
      ((null? params) (error "Too many arguments provided"))
      ((null? values) (error "Too few arguments provided"))
      (else
       (bind-params (cdr params)
                    (cdr values)
                    (state-declare/init (car params) (car values) state))))))

(define replace-state-suffix
  (lambda (state suffix)
    (if (= (length state) (length suffix))
        suffix
        (cons (car state)
              (replace-state-suffix (cdr state) suffix)))))

(define merge-layer-back
  (lambda (caller-layer final-layer)
    (map
     (lambda (binding)
       (let ([match (layer-lookup (car binding) final-layer)])
         (if match
             (list (car binding) (cadr match))
             binding)))
     caller-layer)))

(define merge-state-into-caller
  (lambda (callee-state caller-state)
    (let* ([n (length callee-state)]
           [caller-suffix (state-suffix caller-state n)]
           [merged-suffix (map merge-layer-back caller-suffix callee-state)])
      (replace-state-suffix caller-state merged-suffix))))


(define layer-lookup
  (lambda (name layer)
    (cond
      ((null? layer) #f)
      ((eq? name (caar layer)) (car layer))
      (else (layer-lookup name (cdr layer))))))

(define refresh-layer-from-layer
  (lambda (closure-layer caller-layer)
    (map
     (lambda (binding)
       (let ([match (layer-lookup (car binding) caller-layer)])
         (if match
             (list (car binding) (cadr match))
             binding)))
     closure-layer)))

(define state-suffix
  (lambda (st n)
    (if (= (length st) n)
        st
        (state-suffix (cdr st) n))))

(define refresh-env-from-caller
  (lambda (closure-env caller-state)
    (let ([caller-suffix (state-suffix caller-state (length closure-env))])
      (map refresh-layer-from-layer closure-env caller-suffix))))


(define dot-call-target?
  (lambda (x)
    (and (list? x)
         (= (length x) 3)
         (eq? (car x) 'dot)
         (symbol? (caddr x)))))

(define M_assignment
  (lambda (statement st next return break continue throw)
    (let* ([assign-vs (M_expression-vs statement st)]
           [new-state (vs-state assign-vs)])
      (next new-state))))

(define dot-receiver cadr)
(define dot-member caddr)

(define merge-receiver-back
  (lambda (receiver-exp final-this caller-state)
    (cond
      [(and (symbol? receiver-exp) (eq? receiver-exp 'super))
       (state-update 'this final-this caller-state)]

      [(symbol? receiver-exp)
       (with-handlers ([exn:fail?
                        (lambda (e1)
                          (if (eq? (get-current-class-context) 'ignored)
                              (raise e1)
                              (with-handlers ([exn:fail?
                                               (lambda (e2)
                                                 (raise e1))])
                                (let* ([this-obj (state-lookup 'this caller-state)]
                                       [updated-this
                                        (update-field
                                         (get-current-class-table)
                                         this-obj
                                         receiver-exp
                                         final-this
                                         (get-current-class-context))])
                                  (state-update 'this updated-this caller-state)))))])
         (state-update receiver-exp final-this caller-state))]

      [else
       caller-state])))

(define interp-plain-funcall-expression
  (lambda (statement caller-state)
    (let* ([fname (funcall-name statement)]
           [local-binding
            (with-handlers ([exn:fail? (lambda (e) 'NO-LOCAL-BINDING)])
              (state-lookup fname caller-state))])
      (cond
        [(not (eq? local-binding 'NO-LOCAL-BINDING))
         (let ([closure local-binding])
           (if (not (closure? closure))
               (error 'funcall "Attempted to call a non-function: ~s" fname)
               (let* ([params (closure-params closure)]
                      [body (closure-body closure)]
                      [actuals-vs (eval-actuals-vs (funcall-args statement) caller-state)]
                      [actual-values (vs-value actuals-vs)]
                      [caller-state-after-args (vs-state actuals-vs)]
                      [outer-throw-handler (get-current-throw-handler)]
                      [env (refresh-env-from-caller (closure-env closure) caller-state-after-args)]
                      [call-state0 (push-layer env)]
                      [call-state1 (bind-params params actual-values call-state0)])
                 (call/cc
                  (lambda (return-escape)
                    (M_statementlist-cps
                     body
                     call-state1
                     (lambda (finished-state)
                       (let ([callee-final (pop-layer finished-state)])
                         (return-escape
                          (make-vs 'undefined
                                   (merge-state-into-caller callee-final caller-state-after-args)))))
                     (lambda (ret-val ret-state)
                       (let ([callee-final (pop-layer ret-state)])
                         (return-escape
                          (make-vs ret-val
                                   (merge-state-into-caller callee-final caller-state-after-args)))))
                     (lambda (st) (error "break used outside loop in function"))
                     (lambda (st) (error "continue used outside loop in function"))
                     (lambda (ex st)
                       (let* ([callee-final (pop-layer st)]
                              [merged-state (merge-state-into-caller callee-final caller-state-after-args)])
                         (outer-throw-handler ex merged-state)))))))))]

        [else
         (if (eq? (get-current-class-context) 'ignored)
             (state-lookup fname caller-state) ; re-raise original undeclared-variable error
             (let* ([receiver (state-lookup 'this caller-state)]
                    [actuals-vs (eval-actuals-vs (funcall-args statement) caller-state)]
                    [actual-values (vs-value actuals-vs)]
                    [caller-state-after-args (vs-state actuals-vs)]
                    [method (lookup-method
                             (get-current-class-table)
                             (instance-closure-runtime-class receiver)
                             fname
                             '())])
               (if (method-closure-static? method)
                   (error 'funcall
                          "Static methods are not supported through bare method calls in the current phase: ~s"
                          fname)
                   (run-instance-method-call
                    method
                    receiver
                    'this
                    actual-values
                    caller-state-after-args))))]))))

(define run-instance-method-call
  (lambda (method receiver receiver-exp actual-values caller-state-after-args)
    (let* ([params (method-closure-params method)]
           [body (method-closure-body method)]
           [owner-class (method-closure-owner-class method)]
           [saved-class-context (get-current-class-context)]
           [outer-throw-handler (get-current-throw-handler)]
           [call-state0 (empty-state)]
           [call-state1 (state-declare/init 'this receiver call-state0)]
           [call-state2 (bind-params params actual-values call-state1)])
      (call/cc
       (lambda (return-escape)
         (set-current-class-context! owner-class)
         (M_statementlist-cps
          body
          call-state2
          (lambda (finished-state)
            (let* ([final-this (state-lookup 'this finished-state)]
                   [merged-caller
                    (merge-receiver-back receiver-exp final-this caller-state-after-args)])
              (set-current-class-context! saved-class-context)
              (return-escape (make-vs 'undefined merged-caller))))
          (lambda (ret-val ret-state)
            (let* ([final-this (state-lookup 'this ret-state)]
                   [merged-caller
                    (merge-receiver-back receiver-exp final-this caller-state-after-args)])
              (set-current-class-context! saved-class-context)
              (return-escape (make-vs ret-val merged-caller))))
          (lambda (st)
            (set-current-class-context! saved-class-context)
            (error "break used outside loop in method"))
          (lambda (st)
            (set-current-class-context! saved-class-context)
            (error "continue used outside loop in method"))
          (lambda (ex st)
            (let* ([final-this (state-lookup 'this st)]
                   [merged-caller
                    (merge-receiver-back receiver-exp final-this caller-state-after-args)])
              (set-current-class-context! saved-class-context)
              (outer-throw-handler ex merged-caller)))))))))

(define interp-method-funcall-expression
  (lambda (statement caller-state)
    (let* ([callee (funcall-name statement)]
           [receiver-exp (dot-receiver callee)]
           [method-name (dot-member callee)])
      (cond
        [(and (symbol? receiver-exp) (eq? receiver-exp 'super))
         (let* ([current-class (get-current-class-context)])
           (if (eq? current-class 'ignored)
               (error 'funcall "super used outside an instance method")
               (let* ([start-class
                       (superclass-of (get-current-class-table) current-class)])
                 (if (null? start-class)
                     (error 'funcall
                            "Class ~s has no superclass for super call"
                            current-class)
                     (let* ([receiver (state-lookup 'this caller-state)]
                            [actuals-vs
                             (eval-actuals-vs (funcall-args statement) caller-state)]
                            [actual-values (vs-value actuals-vs)]
                            [caller-state-after-args (vs-state actuals-vs)]
                            [method
                             (lookup-method
                              (get-current-class-table)
                              (instance-closure-runtime-class receiver)
                              method-name
                              start-class)])
                       (if (method-closure-static? method)
                           (error 'funcall
                                  "Static methods are not supported through super calls in the current phase: ~s"
                                  method-name)
                           (run-instance-method-call
                            method
                            receiver
                            'super
                            actual-values
                            caller-state-after-args)))))))]

        [else
         (let* ([receiver-vs (M_expression-vs receiver-exp caller-state)]
                [receiver (vs-value receiver-vs)]
                [st1 (vs-state receiver-vs)])
           (if (not (instance? receiver))
               (error 'funcall "Attempted to call method ~s on non-object: ~s"
                      method-name
                      receiver)
               (let* ([actuals-vs (eval-actuals-vs (funcall-args statement) st1)]
                      [actual-values (vs-value actuals-vs)]
                      [caller-state-after-args (vs-state actuals-vs)]
                      [method (lookup-method
                               (get-current-class-table)
                               (instance-closure-runtime-class receiver)
                               method-name
                               '())])
                 (if (method-closure-static? method)
                     (error 'funcall
                            "Static methods are not supported through object calls in the current phase: ~s"
                            method-name)
                     (run-instance-method-call
                      method
                      receiver
                      receiver-exp
                      actual-values
                      caller-state-after-args)))))]))))

(define interp-funcall-expression
  (lambda (statement caller-state)
    (let ([callee (funcall-name statement)])
      (cond
        [(symbol? callee)
         (interp-plain-funcall-expression statement caller-state)]
        [(dot-call-target? callee)
         (interp-method-funcall-expression statement caller-state)]
        [else
         (error 'funcall "Unsupported function/method call target: ~s" callee)]))))

;; abstractions for try/catch/finally
(define catch-block second)
(define finally-block third)
(define catch-var (lambda (c) (car (cadr c))))
(define catch-body (lambda (c) (caddr c)))
(define finally-body (lambda (f) (cadr f)))
(define catch?   (lambda (x) (and (pair? x) (eq? (car x) 'catch))))
(define finally? (lambda (x) (and (pair? x) (eq? (car x) 'finally))))

;; runs the catch body
(define M_catch-body
  (lambda (catch-part val state next return break continue throw)
    (let ((catch-state (state-declare/init (catch-var catch-part) val (push-layer state))))
      ;; runs the catch body with a state that has the exception bound
      (M_statementlist-cps
       (catch-body catch-part)
       catch-state
       ;; for the continuations, remove the layer with the exception bound
       (lambda (s) (next (pop-layer s)))
       (lambda (v s) (return v (pop-layer s)))
       (lambda (s) (break (pop-layer s)))
       (lambda (s) (continue (pop-layer s)))
       (lambda (v s) (throw v (pop-layer s)))))))

;; runs the finally block if it exists then calls continuation k
(define M_finally
  (lambda (finally-part state k return break continue throw)
    (if (null? finally-part)
        (k state)
        (M_blockofcode (cons 'begin (finally-body finally-part))
                       state k return break continue throw))))

(define M_try
  (lambda (statement state next return break continue throw)
    (let* ((try-body (cadr statement))
           (catch-part (if (catch? (caddr statement))
                           (caddr statement)
                           '()))
           (finally-part (cond
                           ((= (length statement) 4) (cadddr statement))
                           ((and (= (length statement) 3) (finally? (caddr statement))) (caddr statement))
                           (else '())))
           (new-throw
            (lambda (val st)
              (if (null? catch-part)
                  (M_finally finally-part st (lambda (s) (throw val s)) return break continue throw)
                  (M_catch-body catch-part val st
                                (lambda (s) (M_finally finally-part s next return break continue throw))
                                (lambda (v s) (M_finally finally-part s (lambda (s2) (return v s2)) return break continue throw))
                                (lambda (s) (M_finally finally-part s break return break continue throw))
                                (lambda (s) (M_finally finally-part s continue return break continue throw))
                                (lambda (v s) (M_finally finally-part s (lambda (s2) (throw v s2)) return break continue throw)))))))
      (M_blockofcode (cons 'begin try-body)
                   state
                   (lambda (s) (M_finally finally-part s next return break continue throw))
                   (lambda (v s) (M_finally finally-part s (lambda (s2) (return v s2)) return break continue throw))
                   (lambda (s) (M_finally finally-part s break return break continue throw))
                   (lambda (s) (M_finally finally-part s continue return break continue throw))
                   new-throw))))


(register-funcall-handler! interp-funcall-expression)