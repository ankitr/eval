;;; SICP Ch 4.

;;; Compatability (bear with me)
(define apply-underlying apply)
(define true #t)
(define false #f)

;;; Core evaluation function
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ; Looks up special form in eval-rules.
        ((assq (car exp) eval-rules) => (lambda (pair) 
                                          ((cdr pair) exp env)))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

;;; Interpreter

;; Basic Expressions
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

;; Quotation
(define (text-of-quotation exp) (cadr exp))

;; Tagged Lists
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;; Conditionals
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

; NOTE: Used by cond->if to simplify things.
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; Welcome to the wonderful world of cond
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
; Allows for => operations.
(define (extended-cond? clause) (eq? (cadr clause) '=>))
(define (extended-cond-predicate clause) (car clause))
(define (extended-cond-consequent clause) (caddr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (cond ((cond-else-clause? first)
               (if (null? rest)
                   (sequence->exp (cond-actions first))
                   (error "ELSE clause isn't last -- COND->IF"
                          clauses)))
              ((extended-cond? first)
               (make-if (extended-cond-predicate first)
                        (list (extended-cond-consequent first)
                              (extended-cond-predicate first))
                        (expand-clauses rest)))
              (else (make-if (cond-predicate first)
                             (sequence->exp (cond-actions first))
                             (expand-clauses rest)))))))

;; Sequences
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

; Begin
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

; NOTE: For use by cond->if
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))


;; Assignment
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

; Definition
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

;; Lambdas (<3)
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

; NOTE: Used by define
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;; Boolean Logic
; and
(define (and-clauses exp) (cdr exp))
(define (expand-and-clauses clauses)
  (if (null? clauses)
      'true
      (make-if (car clauses)
               (expand-and-clauses (cdr clauses))
               'false)))
(define (and->if exp)
  (expand-and-clauses (and-clauses exp)))

; or
(define (or-clauses exp) (cdr exp))
(define (expand-or-clauses clauses)
  (if (null? clauses)
      'false
      (make-if (car clauses)
               'true
               (expand-or-clauses (cdr clauses)))))
(define (or->if exp)
  (expand-or-clauses (or-clauses exp)))

;; Let and its cousins
; let
(define (let-variables exp) (map car (cadr exp)))
(define (let-expressions exp) (map cadr (cadr exp)))
(define (let-body exp) (cddr exp))

(define (let->combination exp)
  (cons (make-lambda (let-variables exp)
                     (let-body exp))
        (let-expressions exp)))

(define (make-let assignment body)
  (cons 'let (cons assignment body)))

; let*
(define (let*-assignment exp) (cadr exp))
(define (let*-body exp) (caddr exp))

(define (let*->nested-lets exp)
  (transform-let* (let*-assignment exp) (let*-body exp)))
(define (transform-let* assignment body)
  (if (null? (cdr assignment))
      (make-let assignment body)
      ; TODO: Make this a make-let
      (list 'let (list (car assignment))
            (transform-let* (cdr assignment) body)))) 

;; Loops

; TODO: this
  
;; Procedure Applications
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

; NOTE: Syntax for special forms can be considered in the eval-rules function.

;;; Data Structures

;; Predicates
; Everything is true unless it is false.
(define (true? b)
  (not (eq? b false)))
(define (false? b)
  (eq? b false))

;; Procedures
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;; Environment Operations
; Frames
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

; Environment Extension
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

; Environment Loop Abstraction
(define (env-loop var null-action eq-action env)
  (define (scan vars vals)
    ; DEBUGGING
    (cond ((null? vars)
           (null-action env))
          ((eq? var (car vars))
           (eq-action vals))
          (else (scan (cdr vars) (cdr vals)))))
  (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))

; Variable Lookup
(define (lookup-variable-value var env)
  (define (null-action e)
    (env-loop var null-action eq-action (enclosing-environment e)))
  (define eq-action car)
  (env-loop var null-action eq-action env))

; Setting Variables
(define (set-variable-value! var val env)
  (define (null-action e)
    (env-loop var null-action eq-action (enclosing-environment e)))
  (define (eq-action vs)
    (set-car! vs val))
  (env-loop var null-action eq-action env))

; Variable Definition
(define (define-variable! var val env)
  (define (null-action e)
    (add-binding-to-frame! var val (first-frame e)))
  (define (eq-action vs)
    (set-car! vs val))
  (env-loop var null-action eq-action env))

;;; Process Setup

;; Primitive Procedures
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  ; TODO: Make these *core* primitives and have everything else on these abstractions.
  ;       Think PG.
  (list (cons 'car car)
        (cons 'cdr cdr)
        (cons 'cons cons)
        (cons 'null? null?)
        (cons 'eq? eq?)
        (cons 'pair? pair?)
        (cons '+ +)
        (cons '- -)
        (cons '* *)
        (cons '/ /)
        (cons '= =)
        ;<more primitives>
        ))
(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cdr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-underlying
   (primitive-implementation proc) args))

;; Environment Setup
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))

;;; Evaluation and Application Finalization
;; Evaluation Rule Association List
; This has a list of all special forms and their respective actions.
; Extensions require the exp and env inputs.
(define eval-rules
  (list (cons 'quote (lambda (exp env) (text-of-quotation exp)))
        (cons 'set! eval-assignment)
        (cons 'define eval-definition)
        (cons 'if eval-if)
        (cons 'lambda (lambda (exp env)
                        (make-procedure (lambda-parameters exp)
                                        (lambda-body exp)
                                        env)))
        (cons 'begin (lambda (exp env) (eval-sequence (begin-actions exp) env)))
        (cons 'cond (lambda (exp env) (eval (cond->if exp) env)))
        (cons 'and (lambda (exp env) (eval (and->if exp) env)))
        (cons 'or (lambda (exp env) (eval (or->if exp) env)))
        (cons 'let (lambda (exp env) (eval (let->combination exp) env)))
        (cons 'let* (lambda (exp env) (eval (let*->nested-lets exp) env)))))

;; Application Procedures
(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))



;; Application Prompt
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

; Special Printing
(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

;; Let there be code
(driver-loop)