;;; -*- Mode: Scheme -*-

;;;; Explicit Renaming Macros
;;;; Standard Syntax

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define (standard-macrology)
  (compose-macrologies
   (standard-core-macrology)
   (standard-definition-macrology)
   (standard-syntactic-binding-macrology)
   (standard-variable-binding-macrology)
   (standard-derived-syntax-macrology)))

(define (make-extended-macrology procedure)
  (make-macrology
   (lambda (*define-classifier *define-transformer)

     (define (define-classifier pattern procedure)
       (*define-classifier (car pattern)
         (let ((predicate (pattern-predicate `(KEYWORD ,@(cdr pattern)))))
           (lambda (form environment history)
             (if (predicate form
                            (make-alias-generator (make-alias-token)
                                                  environment
                                                  (car pattern))
                            (make-name-comparator environment))
                 (procedure form environment history)
                 (syntax-error "Invalid syntax:" history form))))))

     (define (define-expression-compiler pattern procedure)
       (define-classifier pattern
         (lambda (form environment history)
           (values (make-expression
                    (lambda ()
                      (procedure form environment history)))
                   history))))

     (define (define-transformer pattern procedure auxiliary-names)
       (*define-transformer (car pattern)
         (let ((predicate (pattern-predicate `(KEYWORD ,@(cdr pattern)))))
           (lambda (form rename compare)
             (if (predicate form rename compare)
                 (procedure form rename compare)
                 form)))
         auxiliary-names))

     (procedure define-classifier
                define-expression-compiler
                define-transformer))))

(define (make-standard-transformer-macrology procedure)
  (make-extended-macrology
   (lambda (define-classifier define-expression-compiler define-transformer)
     define-classifier define-expression-compiler ;ignore
     (procedure define-transformer))))

;++ This should be implemented properly.

(define (call-with-syntax-error-procedure receiver)
  (receiver (lambda (message selector . irritants)
              selector                  ;ignore
              (apply error message irritants))))

;;; This pattern predicate mechanism is not for use outside this file.
;;; Please use a real pattern matcher instead.  This is used only for
;;; the sake of bootstrapping.

;++ Improvement:  Record the position of the subexpression
;++ that went wrong, and report that in the syntax error.

(define (pattern-predicate pattern)
  (cond ((pair? pattern)
         (case (car pattern)
           ((@)
            (if (and (pair? (cdr pattern))
                     (string? (cadr pattern))
                     (pair? (cddr pattern))
                     (null? (cdddr pattern)))
                (pattern/annotated (cadr pattern)
                                   (pattern-predicate (caddr pattern)))
                (pattern-error pattern)))
           ((? * +)
            (if (pair? (cdr pattern))
                ((case (car pattern)
                   ((?) (repeating-pattern 0 1))
                   ((*) (repeating-pattern 0 #f))
                   ((+) (repeating-pattern 1 #f)))
                 (pattern-predicate (cadr pattern))
                 (pattern-predicate (cddr pattern)))
                (pattern-error pattern)))
           ((NOT)
            (if (and (pair? (cdr pattern))
                     (null? (cddr pattern)))
                (pattern/complement (cadr pattern))
                (pattern-error pattern)))
           ((AND)
            (pattern/conjunction (map pattern-predicate (cdr pattern))))
           ((OR)
            (pattern/disjunction (map pattern-predicate (cdr pattern))))
           ((QUOTE)
            (if (and (pair? (cdr pattern))
                     (null? (cddr pattern)))
                (pattern/literal (cadr pattern))
                (pattern-error pattern)))
           (else
            (pattern/pair (pattern-predicate (car pattern))
                          (pattern-predicate (cdr pattern))))))
        ((symbol? pattern)
         (case pattern
           ((NAME)
            (pattern/predicate name?))
           ((SYMBOL)
            (pattern/predicate symbol?))
           ((DATUM FORM KEYWORD BVL EXPRESSION LOCATION)    ;Plugh...
            (pattern/variable))
           (else
            (pattern/literal pattern))))
        ((procedure? pattern)
         pattern)
        (else
         (pattern/literal pattern))))

(define (pattern-error pattern)
  (error "Malformed pattern:" pattern))

(define (test-pattern pattern form)
  ((pattern-predicate pattern) form (lambda (x) x) eq?))

(define (pattern/variable)
  (lambda (form rename compare)
    form rename compare                 ;ignore
    #t))

(define (pattern/annotated annotation predicate)
  annotation                            ;ignore
  predicate)

(define (pattern/predicate procedure)
  (lambda (form rename compare)
    rename compare                      ;ignore
    (procedure form)))

(define (pattern/complement predicate)
  (lambda (form rename compare)
    (not (predicate form rename compare))))

(define (pattern/literal datum)
  (if (name? datum)
      (lambda (form rename compare)
        (compare form (rename datum)))
      (lambda (form rename compare)
        rename compare                  ;ignore
        (eqv? form datum))))

(define (pattern/pair car-predicate cdr-predicate)
  (lambda (form rename compare)
    (and (pair? form)
         (car-predicate (car form) rename compare)
         (cdr-predicate (cdr form) rename compare))))

(define (pattern/conjunction conjuncts)
  (lambda (form rename compare)
    (let loop ((conjuncts conjuncts))
      (or (not (pair? conjuncts))
          (and ((car conjuncts) form rename compare)
               (loop (cdr conjuncts)))))))

(define (pattern/disjunction disjuncts)
  (lambda (form rename compare)
    (let loop ((disjuncts disjuncts))
      (and (pair? disjuncts)
           (or ((car disjuncts) form rename compare)
               (loop (cdr disjuncts)))))))

(define (repeating-pattern lower upper)
  (lambda (element-predicate tail-predicate)
    (lambda (form rename compare)
      (define (element? form)
        (element-predicate form rename compare))
      (define (tail? form)
        (tail-predicate form rename compare))
      (let loop ((form form) (count 0))
        (cond ((and upper (= count upper))
               (tail? form))
              ((not (pair? form))
               (and (>= count lower)
                    (tail? form)))
              ((element? (car form))
               (loop (cdr form) (+ count 1)))
              ((>= count lower)
               (tail? form))
              (else #f))))))

(define (standard-core-macrology)
  (make-extended-macrology
   (lambda (define-classifier define-expression-compiler define-transformer)
     define-transformer                 ;ignore

     (define-classifier '(BEGIN * FORM)
       (lambda (form environment history)
         (classify-sequence cdr-selector (cdr form) environment history)))

     (define-expression-compiler '(IF EXPRESSION EXPRESSION ? EXPRESSION)
       (lambda (form environment history)
         (define (subexpression selector cxr)
           (receive (expression history)
               (classify-subexpression selector (cxr form) environment history)
             history                    ;ignore
             expression))
         (let* ((condition (subexpression cadr-selector cadr))
                (consequent (subexpression caddr-selector caddr))
                (alternative
                 (and (pair? (cdddr form))
                      (subexpression cadddr-selector cadddr))))
           ((conditional-compiler environment) condition
                                               consequent
                                               alternative
                                               history))))

     (define-expression-compiler '(QUOTE DATUM)
       (lambda (form environment history)
         ((quotation-compiler environment) (strip-syntax (cadr form))
                                           history)))

     (define-expression-compiler '(SET! LOCATION EXPRESSION)
       (lambda (form environment history)
         (define (subform classifier selector cxr)
           (receive (classification history)
               (classifier selector (cxr form) environment history)
             history                    ;ignore
             classification))
         ((location/assignment-compiler
           (subform classify-sublocation cadr-selector cadr))
          (subform classify-subexpression caddr-selector caddr)
          history)))

     (define-expression-compiler '(LAMBDA BVL + FORM)
       (lambda (form environment history)
         (let* ((environment* (syntactic-extend environment))
                (bvl*
                 (let ((bvl (cadr form)))
                   ((lambda-bvl-mapper environment)
                    bvl
                    (history/add-subform history cadr-selector bvl environment)
                    (lambda (name)
                      (bind-variable! name environment*))))))
           (syntactic-seal! environment*)
           ((lambda-compiler environment)
            bvl*
            (make-sequence cddr-selector
                           (cddr form)
                           (syntactic-extend environment*)
                           history)
            environment*
            history)))))))

(define (standard-definition-macrology)
  (make-extended-macrology
   (lambda (define-classifier define-expression-compiler define-transformer)
     define-expression-compiler define-transformer ;ignore
     (define-variable-definer define-classifier standard-definition-pattern
       make-variable-definition)
     (define-syntax-definer define-classifier))))

(define (curried-definition-macrology)
  (make-extended-macrology
   (lambda (define-classifier define-expression-compiler define-transformer)
     define-expression-compiler define-transformer ;ignore
     (define-variable-definer define-classifier curried-definition-pattern
       make-variable-definition)
     (define-syntax-definer define-classifier))))

(define (overloaded-definition-macrology)
  (make-extended-macrology
   (lambda (define-classifier define-expression-compiler define-transformer)
     define-expression-compiler define-transformer ;ignore
     (define-variable-definer define-classifier curried-definition-pattern
       make-overloaded-definition))))

(define standard-definition-pattern
  '(DEFINE . (OR (NAME EXPRESSION) ((NAME . BVL) + FORM))))

(define curried-definition-pattern
  (letrec ((curried?
            (pattern-predicate
             `((OR NAME
                   ,(lambda (form rename compare)
                      (curried? form rename compare)))
               . BVL))))
    `(DEFINE . (OR (NAME EXPRESSION) (,curried? + FORM)))))

(define (define-variable-definer define-classifier pattern make-definition)
  (define-classifier pattern
    (lambda (form environment history)
      (if (name? (cadr form))
          (values (make-definition cadr-selector (cadr form)
                                   caddr-selector (caddr form)
                                   environment history)
                  history)
          (classify-reduction
           (let ((define-keyword (car form))
                 (name (caadr form))
                 (bvl (cdadr form))
                 (body (cddr form)))
             (let ((lambda-keyword      ;++ Better absolute reference...
                    (generate-alias 'LAMBDA environment define-keyword)))
               `(,define-keyword ,name (,lambda-keyword ,bvl ,@body))))
           environment
           history)))))

(define (define-syntax-definer define-classifier)
  (define-classifier '(DEFINE-SYNTAX NAME FORM)
    (lambda (form environment history)
      (values (make-keyword-definition cadr-selector (cadr form)
                                       caddr-selector (caddr form)
                                       environment history)
              history))))

(define (make-syntactic-binding-macrology classifier)
  (make-extended-macrology
   (lambda (define-classifier define-expression-compiler define-transformer)
     define-expression-compiler define-transformer ;ignore

     (let ()

       (define (local-syntax form environment closing-environment history)
         (select-for-each cadr-selector (cadr form)
           (lambda (selector binding)
             (receive (keyword history)
                 (classify-subkeyword (select-car (select-cdr selector))
                                      (cadr binding)
                                      closing-environment
                                      history)
               history                  ;ignore
               (syntactic-bind! environment
                                (car binding)
                                (keyword/denotation keyword)))))
         (classifier cddr-selector (cddr form) environment history (car form)))

       (define-classifier '(LET-SYNTAX (* (@ "LET-SYNTAX binding"
                                             (NAME EXPRESSION)))
                             + FORM)
         (lambda (form environment history)
           (local-syntax form
                         (syntactic-extend environment)
                         environment
                         history)))

       (define-classifier '(LETREC-SYNTAX (* (@ "LETREC-SYNTAX binding"
                                                (NAME EXPRESSION)))
                             + FORM)
         (lambda (form environment history)
           (let ((environment (syntactic-extend environment)))
             (local-syntax form environment environment history))))))))

;;; Per R5RS, LET-SYNTAX and LETREC-SYNTAX always introduce a new
;;; scope.  This means, though, that one can't do something along
;;; these lines:
;;;
;;;   (let-syntax ((define-foo ...))
;;;     (define-foo mumble)
;;;     (define-foo grumble)),
;;;
;;; which is often a useful idiom, and which I believe Chez supports.
;;; So we provide two options.

(define (standard-syntactic-binding-macrology)
  (make-syntactic-binding-macrology
   (lambda (selector forms environment history keyword)
     selector                           ;ignore
     (classify-reduction
      ;++ This should probably not generate a reference to LET.
      `(,(generate-alias 'LET environment keyword) () ,@forms)
      environment
      history))))

(define (non-standard-syntactic-binding-macrology)
  (make-syntactic-binding-macrology
   (lambda (selector forms environment history keyword)
     keyword                            ;ignore
     (classify-sequence selector forms environment history))))

(define (standard-variable-binding-macrology)
  (make-standard-transformer-macrology
   (lambda (define-transformer)

     (define-transformer '(LET . DATUM)   ;Dummy pattern.
       (let ((unnamed-let?
              (pattern-predicate
               '(KEYWORD (* (NAME EXPRESSION)) + FORM)))
             (named-let?
              (pattern-predicate
               '(KEYWORD NAME (* (NAME EXPRESSION)) + FORM))))
         (lambda (form rename compare)
           (cond ((unnamed-let? form rename compare)
                  (let ((bindings (cadr form))
                        (body (cddr form)))
                    (let ((variables (map car bindings))
                          (initializers (map cadr bindings)))
                      `((,(rename 'LAMBDA) ,variables ,@body)
                        ,@initializers))))
                 ((named-let? form rename compare)
                  (let ((name (cadr form))
                        (bindings (caddr form))
                        (body (cdddr form)))
                    (let ((variables (map car bindings))
                          (initializers (map cadr bindings)))
                      `((,(rename 'LET) ()
                          (,(rename 'DEFINE) (,name ,@variables)
                            ,@body)
                          ,name)
                        ,@initializers))))
                 (else form))))
       '(DEFINE LAMBDA LET))

     (define-transformer '(LET* (* (@ "LET* binding" (NAME EXPRESSION)))
                            + FORM)
       (lambda (form rename compare)
         compare                        ;ignore
         (let ((clauses (cadr form))
               (body (cddr form)))
           (if (not (pair? clauses))
               `(,(rename 'LET) () ,@body)
               (let recur ((clauses clauses))
                 (let ((clause (car clauses))
                       (clauses (cdr clauses)))
                   `(,(rename 'LET) (,clause)
                      ,@(if (pair? clauses)
                            `(,(recur clauses))
                            body)))))))
       '(LET))

     (define-transformer '(LETREC (* (@ "LETREC binding" (NAME EXPRESSION)))
                            + FORM)
       (lambda (form rename compare)
         compare
         (let ((bindings (cadr form))
               (body (cddr form)))
           `(,(rename 'LET) ()
              ,@(map (lambda (binding)
                       (let ((variable (car binding))
                             (initializer (cadr binding)))
                         `(,(rename 'DEFINE) ,variable ,initializer)))
                     bindings)
              (,(rename 'LET) ()
                ,@body))))
       '(DEFINE LET)))))

(define (standard-derived-syntax-macrology)
  (make-standard-transformer-macrology
   (lambda (define-transformer)

     (define (boolean-reduction null-case binary-case)
       (lambda (form rename compare)
         compare                        ;ignore
         (let ((operands (cdr form)))
           (cond ((not (pair? operands))
                  (null-case rename))
                 ((not (pair? (cdr operands)))
                  (car operands))
                 (else
                  (binary-case rename
                               (car operands)
                               `(,(car form) ,@(cdr operands))))))))

     (define-transformer '(OR * EXPRESSION)
       (boolean-reduction (lambda (rename)
                            rename      ;ignore
                            '#F)
                          (lambda (rename a b)
                            `(,(rename 'LET) ((,(rename 'T) ,a))
                               (,(rename 'IF) ,(rename 'T)
                                 ,(rename 'T)
                                 ,b))))
       '(LET IF))

     (define-transformer '(AND * EXPRESSION)
       (boolean-reduction (lambda (rename)
                            rename      ;ignore
                            '#T)
                          (lambda (rename a b)
                            `(,(rename 'IF) ,a ,b #F)))
       '(IF))

     (define-transformer
         '(DO (* (@ "DO variable clause" (NAME EXPRESSION ? EXPRESSION)))
              (@ "DO return clause" (EXPRESSION ? EXPRESSION))
            * FORM)
       (lambda (form rename compare)
         compare                        ;ignore
         (let ((bindings (cadr form))
               (condition (car (caddr form)))
               (result (cdr (caddr form)))
               (body (cdddr form)))
           (let ((loop-variables
                  (map (lambda (binding)
                         `(,(car binding) ,(cadr binding)))
                       bindings))
                 (loop-updates
                  (map (lambda (binding)
                         (if (null? (cddr binding))
                             (car binding)
                             (caddr binding)))
                       bindings)))
             `(,(rename 'LET) ,(rename 'DO-LOOP) (,@loop-variables)
                (,(rename 'IF) ,condition
                  (,(rename 'IF) #F #F ,@result)
                  (,(rename 'BEGIN)
                    ,@body
                    (,(rename 'DO-LOOP) ,@loop-updates)))))))
       '(BEGIN IF LET))

     ;;; This supports the SRFI 61 extension to `=>' clauses.

     (define-transformer '(COND + (@ "COND clause" (+ EXPRESSION)))
       (let ((else-clause? (pattern-predicate '('ELSE + EXPRESSION)))
             (or-clause? (pattern-predicate '(EXPRESSION)))
             (=>-clause? (pattern-predicate '(EXPRESSION '=> EXPRESSION)))
             (=>*-clause?
              (pattern-predicate '(EXPRESSION EXPRESSION '=> EXPRESSION))))
         (lambda (form rename compare)
           (call-with-syntax-error-procedure
             (lambda (syntax-error)
               (let recur ((selector cdr-selector) (clauses (cdr form)))
                 (let ((clause (car clauses))
                       (clauses* (cdr clauses))
                       (selector* (select-cdr selector)))
                   (define (more?) (pair? clauses*))
                   (define (more) (recur selector* clauses*))
                   (define (maybe-more) (if (more?) `(,(more)) '()))
                   (cond ((else-clause? clause rename compare)
                          (if (more?)
                              (syntax-error "COND clauses after ELSE:"
                                            selector*
                                            clauses*)
                              (let ((body (cdr clause)))
                                ;; Tricky pedantic case to prevent
                                ;; internal definitions at all costs.
                                `(,(rename 'IF) #T
                                   (,(rename 'BEGIN) ,@body)))))
                         ((or-clause? clause rename compare)
                          (let ((expression (car clause)))
                            (if (more?)
                                ;; More pedantic tricks.
                                `(,(rename 'IF) #T
                                   ,expression)
                                `(,(rename 'OR) ,expression ,(more)))))
                         ((=>-clause? clause rename compare)
                          (let ((producer (car clause))
                                (consumer (caddr clause)))
                            `(,(rename 'LET) ((,(rename 'T) ,producer))
                               (,(rename 'IF) ,(rename 'T)
                                 (,consumer ,(rename 'T))
                                 ,@(maybe-more)))))
                         ((=>*-clause? clause rename compare)
                          (let ((producer (car clause))
                                (tester (caddr clause))
                                (consumer (cadddr clause)))
                            `(,(rename 'LET) ((,(rename 'T) ,producer))
                               (,(rename 'IF)
                                   (,(rename APPLY) ,tester ,(rename 'T))
                                 (,(rename 'APPLY) ,consumer ,(rename 'T))
                                 ,@(maybe-more)))))
                         (else
                          (let ((condition (car clause))
                                (body (cdr clause)))
                            `(,(rename 'IF) ,condition
                               (,(rename 'BEGIN) ,@body)
                               ,@(maybe-more)))))))))))
       '(APPLY BEGIN IF LET OR))

     (define-transformer '(CASE EXPRESSION + (@ "CASE clause" (+ EXPRESSION)))
       (let ((else-clause? (pattern-predicate '('ELSE + EXPRESSION)))
             (case-clause? (pattern-predicate '((* DATUM) + EXPRESSION))))
         (lambda (form rename compare)
           (call-with-syntax-error-procedure
             (lambda (syntax-error)
               `(,(rename 'LET) ((,(rename 'KEY) ,(cadr form)))
                  ,(let recur ((selector cddr-selector)
                               (clauses (cddr form)))
                     (let ((clause (car clauses))
                           (clauses* (cdr clauses))
                           (selector* (select-cdr selector)))
                       (define (more?) (pair? clauses*))
                       (define (more) (recur selector* clauses*))
                       (define (maybe-more) (if (more?) `(,(more)) '()))
                       (cond ((else-clause? clause rename compare)
                              (if (more?)
                                  (syntax-error "CASE clauses after ELSE:"
                                                selector*
                                                clauses*)
                                  `(,(rename 'IF) #T
                                     (,(rename 'BEGIN) ,@(cdr clause)))))
                             ((case-clause? clause rename compare)
                              (let ((data (car clause))
                                    (forms (cdr clause)))
                                `(,(rename 'IF)
                                     ,(let ((compare
                                             (lambda (datum)
                                               `(,(rename 'EQV?)
                                                 ,(rename 'KEY)
                                                 (,(rename 'QUOTE) ,datum)))))
                                        (cond ((not (pair? data))
                                               '#F)
                                              ((not (pair? (cdr data)))
                                               (compare (car data)))
                                              (else
                                               `(,(rename 'OR)
                                                 ,@(map compare data)))))
                                   (,(rename 'BEGIN) ,@forms)
                                   ,@(maybe-more))))
                             (else
                              (syntax-error "Invalid CASE clause:"
                                            (select-car selector)
                                            clause))))))))))
       '(BEGIN EQV? IF LET QUOTE)))))

(define (non-standard-syntax-macrology)
  (make-extended-macrology
   (lambda (define-classifier define-expression-compiler define-transformer)
     define-transformer                 ;ignore

     (define-expression-compiler '(SYNTAX-QUOTE DATUM)
       (lambda (form environment history)
         ((quotation-compiler environment) (cadr form) history)))

     (define-classifier '(ER-MACRO-TRANSFORMER EXPRESSION ? (* NAME))
       (lambda (form environment history)
         (define (finish procedure auxiliary-names)
           (values (make-keyword form
                                 (make-transformer environment
                                                   auxiliary-names
                                                   procedure
                                                   form))
                   history))
         (let ((expression (cadr form))
               (auxiliary-names (if (pair? (cddr form)) (caddr form) #f)))
           (let ((mumble (meta-evaluate expression environment)))
             (cond ((procedure? mumble)
                    (finish mumble auxiliary-names))
                   ((and (pair? mumble)
                         (procedure? (car mumble))
                         (name-list? (cdr mumble)))
                    (if auxiliary-names
                        (syntax-error "Multiple auxiliary name lists:"
                                      history
                                      (cdr mumble)
                                      auxiliary-names)
                        (finish (car mumble) (cdr mumble))))
                   (else
                    (syntax-error "Invalid transformer procedure:"
                                  history
                                  mumble))))))))))

(define (name-list? object)
  (if (pair? object)
      (and (name? (car object))
           (name-list? (cdr object)))
      (null? object)))
