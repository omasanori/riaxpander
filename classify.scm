;;; -*- Mode: Scheme -*-

;;;; Explicit Renaming Macros
;;;; Classification

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define (classify form environment history)
  (cond ((pair? form)
         (classify-pair form environment history))
        ((name? form)
         (classify-name form environment history))
        (else
         ((datum-classifier environment) form environment history))))

;;; CLASSIFY-ERROR calls RECLASSIFY if the user has supplied a
;;; replacement form to classify instead.

(define (reclassify form history)
  ;++ Should this replace the reduction instead of adding a new one?
  (classify-reduction form
                      (reduction/environment
                       (history/current-reduction history))
                      history))

(define (classify-pair pair environment history)
  (receive (operator operator-history)
           (classify-subform car-selector (car pair) environment history)
    (cond ((keyword? operator)
           (classify/keyword operator pair environment history))
          ((not (list? (cdr pair)))     ;++ Allow the client to decide?
           (classify-error "Invalid combination -- not a proper list:"
                           history
                           pair))
          ((coerce-expression operator environment operator-history)
           => (lambda (operator)
                (classify-combination operator pair environment history)))
          (else
           (classify-error "Invalid operator in combination:"
                           history
                           operator
                           pair)))))

(define (classify-name name environment history)
  (cond ((syntactic-lookup environment name)
         => (lambda (denotation)
              (cond ((classify-variable denotation environment history)
                     => (lambda (classification)
                          (values classification history)))
                    ((or (classifier? denotation)
                         (transformer? denotation))
                     (values (make-keyword name denotation) history))
                    (else
                     (error "Invalid denotation:" denotation
                            name environment history)))))
        (else
         (values ((free-variable-classifier environment) name history)
                 history))))

(define (classify/keyword keyword form environment history)
  (let ((name (keyword/name keyword))
        (denotation (keyword/denotation keyword)))
    (cond ((classifier? denotation)
           (classify/classifier name denotation form environment history))
          ((transformer? denotation)
           (classify/transformer name denotation form environment history))
          (else
           (error "Invalid keyword denotation:" denotation keyword)))))

(define (classify/classifier name classifier form environment history)
  name                                  ;ignore
  ((classifier/procedure classifier) form environment history))

(define (classify/transformer name transformer form environment history)
  (receive (form* environment*)
           (apply-transformer name transformer form environment)
    (if (eq? form* form)
        (classify-error "Invalid syntax:" history form)
        (classify-reduction form* environment* history))))

(define (classify-combination operator combination environment history)
  ((combination-classifier environment)
   operator
   (classify-subexpressions cdr-selector
                            (cdr combination)
                            environment
                            history)
   environment
   history))

(define (classify-variable variable environment history)
  (cond ((local-variable? variable)
         ((local-variable-classifier environment)
          (local-variable/name variable)
          (local-variable/rename variable)
          environment
          history))
        ((top-level-variable? variable)
         ((top-level-variable-classifier environment)
          (top-level-variable/name variable)
          (top-level-variable/location variable)
          environment
          history))
        ((free-variable? variable)
         ((free-variable-classifier environment)
          (free-variable/name variable)
          environment
          history))
        (else #f)))

;;;; Classification Utilities

(define (classify-subform selector form environment history)
  (classify-subform* classify selector form environment history))

(define (classify-subform* classifier selector form environment history)
  (classifier form
              environment
              (history/add-subform history selector form environment)))

(define (classify-subforms selector forms environment history)
  (classify-subforms* classify selector forms environment history))

(define (classify-subforms* classifier selector forms environment history)
  (select-map selector forms
    (lambda (selector form)
      (receive (classification history)
               (classify-subform* classifier selector form environment history)
        history                         ;ignore
        classification))))

(define (classify-reduction form environment history)
  (classify form environment (history/add-reduction history form environment)))

(define (guarded-classifier predicate error-message)
  (lambda (form environment history)
    (let loop ((classifier (lambda () (classify form environment history))))
      (receive (classification history) (classifier)
        (if (predicate classification)
            (values classification history)
            (loop
             (lambda ()
               (classify-error error-message history classification))))))))

(define classify-definition
  (guarded-classifier (lambda (classification) (definition? classification))
                      "Non-definition in definition context:"))

(define (classify-subdefinition selector form environment history)
  (classify-subform* classify-definition selector form environment history))

(define (classify-subdefinitions selector forms environment history)
  (classify-subforms* classify-definition selector forms environment history))

(define classify-location
  (guarded-classifier (lambda (classification) (location? classification))
                      "Non-location in location context:"))

(define (classify-sublocation selector form environment history)
  (classify-subform* classify-location selector form environment history))

(define (classify-sublocations selector forms environment history)
  (classify-subforms* classify-location selector forms environment history))

(define classify-keyword
  (guarded-classifier (lambda (classification) (keyword? classification))
                      "Non-keyword in keyword context:"))

(define (classify-subkeyword selector form environment history)
  (classify-subform* classify-keyword selector form environment history))

(define (classify-subkeywords selector forms environment history)
  (classify-subforms* classify-keyword selector forms environment history))

;;;; Expression Classification

;;; Expressions require special handling, because we cannot simply
;;; view something as an expression later on (with some dispatch in a
;;; COMPILE-EXPRESSION procedure): we may need to continue
;;; classification if the expression is actually a sequence, but
;;; classification requires an environment, which is not stored in
;;; sequences.  So we turn sequences into expressions right here.

(define (classify-expression form environment history)
  (let loop ((classifier (lambda () (classify form environment history))))
    (receive (classification history) (classifier)
      (cond ((coerce-expression classification environment history)
             => (lambda (expression)
                  (values expression history)))
            (else
             (loop
              (lambda ()
                (classify-error "Non-expression in expression context:"
                                history
                                classification))))))))

(define (classify-subexpression selector form environment history)
  (classify-subform* classify-expression selector form environment history))

(define (classify-subexpressions selector forms environment history)
  (classify-subforms* classify-expression selector forms environment history))

(define (coerce-expression classification environment history)
  (cond ((expression? classification)
         classification)
        ((sequence? classification)
         (make-expression
          ((expression-sequence-compiler environment)
           (classify-sequence scan-expressions
                              classification
                              environment)
           history)))
        (else #f)))

;;;; Sequence Classification

;;; This page of the section is the easy page.  It is very
;;; straightforward and should not frighten the reader.  The only
;;; reason that we do not use CLASSIFY-SUBEXPRESSIONS is that we
;;; choose to flatten nested subsequences.  Beware, however, after the
;;; end of this page.

(define (classify-sequence scanner sequence environment)
  (scanner (sequence/selector sequence)
           (sequence/subforms sequence)
           environment
           (sequence/history sequence)))

(define (homogeneous-sequence-scanner predicate error-message)
  (lambda (selector forms environment history)

    (define (process-sequent classifier result)
      (receive (classification history) (classifier)
        (cond ((sequence? classification)
               (scan-sequence (sequence/selector classification)
                              (sequence/subforms classification)
                              (sequence/history classification)
                              result))
              ((predicate classification)
               (cons classification result))
              (else
               (process-sequent
                (lambda ()
                  (classify-error error-message history classification))
                result)))))

    (define (scan-sequence selector forms history result)
      (if (pair? forms)
          (scan-sequence (select-cdr selector)
                         (cdr forms)
                         history
                         (process-sequent
                          (lambda ()
                            (classify-subform (select-car selector)
                                              (car forms)
                                              environment
                                              history))
                          result))
          (reverse result)))

    (scan-sequence selector forms history '())))

(define scan-expressions
  (homogeneous-sequence-scanner (lambda (classification)
                                  (expression? classification))
                                "Non-expression in expression sequence:"))

(define scan-definitions
  (homogeneous-sequence-scanner (lambda (classification)
                                  (definition? classification))
                                "Non-definition in definition sequence:"))

;;;;; Top-Level Sequences

(define (scan-top-level selector forms environment history)
  (let loop ((selector selector) (forms forms) (result '()))
    (if (pair? forms)
        (loop (select-cdr selector)
              (cdr forms)
              (append-reverse
               (classify-top-level (select-car selector)
                                   (car forms)
                                   environment
                                   history)
               result))
        (reverse result))))

(define (classify-top-level selector form environment history)
  (receive (classification history)
           (classify-subform selector form environment history)
    (cond ((sequence? classification)
           (classify-sequence scan-top-level classification environment))
          ((definition? classification)
           (process-definition classification environment))
          (else
           (list classification)))))

;;;;; The Horror of Internal Definitions

;;; This code might perhaps be improved by multi-continuation
;;; procedure calls.  There is not a single lambda expression in the
;;; following pages that should require a heap closure, but there is
;;; also not a single Scheme compiler out there short of Stalin that
;;; is capable of concluding this.  What a pity.

(define (scan-body selector forms environment history)
  (let loop ((selector selector) (forms forms) (bindings '()))
    (define (finish expressions)
      (values (reverse bindings) expressions))
    (if (pair? forms)
        ((classify-body-form (select-car selector)
                             (car forms)
                             environment
                             history)
         (lambda (bindings*)            ;if-definitions
           (loop (select-cdr selector)
                 (cdr forms)
                 (append-reverse bindings* bindings)))
         (lambda (expressions)          ;if-expressions
           (finish (append expressions
                           (scan-expressions (select-cdr selector)
                                             (cdr forms)
                                             environment
                                             history)))))
        (finish '()))))

(define (classify-body-form selector form environment history)
  (lambda (if-definitions if-expressions)
    ((sequent-case selector form environment history)
     (lambda ()
       (if-definitions '()))
     (lambda (definitions)
       (if-definitions
        (append-map (lambda (definition)
                      (process-definition definition environment))
                    definitions)))
     if-expressions)))

;;;;; Subsequences

(define (scan-subsequence selector forms environment history)
  (lambda (if-empty if-definitions if-expressions)
    (let loop ((selector selector) (forms forms))
      (define (scan scanner)
        (scanner (select-cdr selector) (cdr forms) environment history))
      (if (pair? forms)
          ((sequent-case (select-car selector) (car forms) environment history)
           (lambda ()
             (loop (select-cdr selector) (cdr forms)))
           (lambda (definitions)
             (if-definitions
              (append definitions (scan scan-definitions))))
           (lambda (expressions)
             (if-expressions
              (append expressions (scan scan-expressions)))))
          (if-empty)))))

(define (sequent-case selector form environment history)
  (lambda (if-empty if-definitions if-expressions)
    (let loop ((classifier
                (lambda ()
                  (classify-subform selector form environment history))))
      (receive (classification history) (classifier)
        (cond ((sequence? classification)
               ((classify-sequence scan-subsequence classification environment)
                if-empty if-definitions if-expressions))
              ((definition? classification)
               (if-definitions (list classification)))
              ((expression? classification)
               (if-expressions (list classification)))
              (else
               (loop (lambda ()
                       (classify-error "Invalid sequence element:"
                                       history
                                       classification)))))))))

;;;; Definition Processing

;;; This should go somewhere else.  Perhaps the definition processing
;;; procedure ought to be a syntactic parameter.

(define (process-definition definition environment)
  ((definition/binder definition) (definition/name definition)
                                  (definition/subform-selector definition)
                                  (definition/subform definition)
                                  environment
                                  (definition/history definition)))

(define (variable-binder name selector form environment history)
  (receive (expression expression-history)
           (classify-subexpression selector form environment history)
    expression-history                  ;ignore
    (variable-binding environment name expression)))

(define (keyword-binder name selector form environment history)
  (receive (keyword keyword-history)
           (classify-subkeyword selector form environment history)
    keyword-history                     ;ignore
    (keyword-binding environment name keyword)))

(define (overloaded-binder name selector form environment history)
  (let loop ((classifier
              (lambda ()
                (classify-subform selector form environment history))))
    (receive (classification history) (classifier)
      history                           ;ignore
      (cond ((keyword? classification)
             (keyword-binding environment name classification))
            ((coerce-expression classification environment history)
             => (lambda (expression)
                  (variable-binding environment name expression)))
            (else
             (loop (lambda ()
                     (syntax-error "Invalid right-hand side of definition:"
                                   history
                                   classification))))))))

(define (variable-binding environment name expression)
  (list (make-binding (bind-variable! name environment) expression)))

(define (keyword-binding environment name keyword)
  (syntactic-bind! environment name (keyword/denotation keyword))
  '())
