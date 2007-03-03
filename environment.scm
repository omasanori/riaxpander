;;; -*- Mode: Scheme -*-

;;;; Explicit Renaming Macros
;;;; Syntactic Environments

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-record-type <syntactic-environment>
    (make-syntactic-environment operations parameters parent data)
    syntactic-environment?
  (operations syntactic-environment/operations)
  (parameters syntactic-environment/parameters)
  (parent syntactic-environment/parent)
  (data syntactic-environment/data set-syntactic-environment/data!))

(define-record-type <syntactic-operations>
    (make-syntactic-operations lookup
                               bind!
                               seal!
                               alias
                               disclose
                               for-each-binding)
    syntactic-operations?
  (lookup syntactic-operations/lookup)
  (bind! syntactic-operations/bind!)
  (seal! syntactic-operations/seal!)
  (alias syntactic-operations/alias)
  (disclose syntactic-operations/disclose)
  (for-each-binding syntactic-operations/for-each-binding))

(define (null-syntactic-environment parameters . context)
  (make-syntactic-environment null-syntactic-operations parameters #f context))

(define null-syntactic-operations
  (let ()
    (define (lose operation)
      (error "Null syntactic environment:" operation))
    (make-syntactic-operations
     (lambda (environment name)
       (lose `(SYNTACTIC-LOOKUP ,environment ,name)))
     (lambda (environment name denotation)
       (lose `(SYNTACTIC-BIND! ,environment ,name ,denotation)))
     (lambda (environment)
       (lose `(SYNTACTIC-SEAL! ,environment)))
     (lambda (environment name)
       (lose `(SYNTACTIC-ALIAS ,environment ,name)))
     (lambda (environment)
       `(NULL ,@(syntactic-environment/data environment)))
     (lambda (environment procedure)
       environment procedure            ;ignore
       (if #f #f)))))

(define (syntactic-parameter environment key)
  ((syntactic-environment/parameters environment) key))

(define (syntactic-lookup environment name)
  ((syntactic-operations/lookup (syntactic-environment/operations environment))
   environment name))

(define (syntactic-bind! environment name denotation)
  ((syntactic-operations/bind! (syntactic-environment/operations environment))
   environment name denotation))

(define (syntactic-seal! environment)
  ((syntactic-operations/seal! (syntactic-environment/operations environment))
   environment))

(define (syntactic-alias environment name)
  ((syntactic-operations/alias (syntactic-environment/operations environment))
   environment name))

(define (disclose-syntactic-environment environment)
  ((syntactic-operations/disclose
    (syntactic-environment/operations environment))
   environment))

(define (for-each-syntactic-binding environment procedure)
  (let walk ((environment environment))
    ((syntactic-operations/for-each-binding
      (syntactic-environment/operations environment))
     environment procedure)
    (cond ((syntactic-environment/parent environment)
           => walk))))

(define (bind-variable! name environment)
  (let ((variable
         (make-local-variable name (syntactic-alias environment name))))
    (syntactic-bind! environment name variable)
    variable))

(define (name=? environment-a name-a environment-b name-b)
  (let ((denotation-a (syntactic-lookup environment-a name-a))
        (denotation-b (syntactic-lookup environment-b name-b)))
    (cond ((and denotation-a denotation-b)
           (denotation=? denotation-a denotation-b))
          ((and (not denotation-a) (not denotation-b))
           (eq? (name/original-symbol name-a)
                (name/original-symbol name-b)))
          (else #f))))

;;;; Syntactic Parameters

;;; These are operations not intrinsic to the concept of syntactic
;;; environments; they are supplied by the client of the syntactic
;;; environment abstraction, primarily to control the format of the
;;; output.

;;; The access path for syntactic parameters is faster than that for
;;; name lookup, because we can elide the hierarchy.  When
;;; constructing a syntactic environment, it usually contains a copy
;;; of the reference to the parent's syntactic parameters, so there is
;;; no iterative indirection, as there is for name lookup.  Deeply
;;; nested binding forms, then, pose no problem for the performance of
;;; these procedures.

(define (local-variable-classifier environment)
  (syntactic-parameter environment local-variable-classifier))

(define (top-level-variable-classifier environment)
  (syntactic-parameter environment top-level-variable-classifier))

(define (free-variable-classifier environment)
  (syntactic-parameter environment free-variable-classifier))

(define (datum-classifier environment)
  (syntactic-parameter environment datum-classifier))

(define (combination-classifier environment)
  (syntactic-parameter environment combination-classifier))

(define (self-evaluating? datum environment)
  ((syntactic-parameter environment self-evaluating?) datum))

(define (quotation-compiler environment)
  (syntactic-parameter environment quotation-compiler))

(define (expression-sequence-compiler environment)
  (syntactic-parameter environment expression-sequence-compiler))

(define (conditional-compiler environment)
  (syntactic-parameter environment conditional-compiler))

(define (lambda-bvl-mapper environment)
  (syntactic-parameter environment lambda-bvl-mapper))

(define (lambda-compiler environment)
  (syntactic-parameter environment lambda-compiler))

(define (meta-evaluator environment)
  (syntactic-parameter environment meta-evaluator))

(define (meta-evaluate form environment)
  ((meta-evaluator environment) form environment))

;;;; Macrologies

(define (apply-macrology macrology environment)
  (macrology environment))

(define null-macrology
  (lambda (environment)
    environment                         ;ignore
    (values)))

(define (make-macrology procedure)
  (lambda (environment)
    (define (define-classifier name procedure)
      (syntactic-bind! environment name (make-classifier name procedure)))
    (define (define-transformer name procedure auxiliary-names)
      (syntactic-bind! environment
                       name
                       (make-transformer environment
                                         auxiliary-names
                                         procedure
                                         ;; No source record.
                                         #f)))
    (procedure define-classifier define-transformer)))

(define (make-classifier-macrology procedure)
  (make-macrology
   (lambda (define-classifier define-transformer)
     define-transformer                 ;ignore
     (procedure define-classifier))))

(define (make-transformer-macrology procedure)
  (make-macrology
   (lambda (define-classifier define-transformer)
     define-classifier                  ;ignore
     (procedure define-transformer))))

(define (compose-macrologies . macrologies)
  (compose-macrology-list macrologies))

(define (compose-macrology-list macrologies)
  (reduce compose-macrology null-macrology macrologies))

(define (compose-macrology macrology-a macrology-b)
  (lambda (environment)
    (macrology-a environment)
    (macrology-b environment)))

;;;; Extended Environments

(define (syntactic-extend environment)
  (make-syntactic-environment extended-syntactic-operations
                              (syntactic-environment/parameters environment)
                              environment
                              (cons '() (make-alias-token))))

(define extended-syntactic-operations
  (let ()
    (define (local-bindings environment)
      (car (syntactic-environment/data environment)))
    (define (set-local-bindings! environment bindings)
      (set-car! (syntactic-environment/data environment) bindings))
    (define (alias-token environment)
      (cdr (syntactic-environment/data environment)))
    (make-syntactic-operations
     (lambda (environment name)         ;lookup
       (cond ((assq name (local-bindings environment))
              => cdr)
             (else
              (syntactic-lookup (syntactic-environment/parent environment)
                                name))))
     (lambda (environment name denotation) ;bind!
       ;++ This should report more useful (and restartable) errors.
       (cond ((assq name (local-bindings environment))
              => (lambda (probe)
                   (error "Rebinding name:" environment name denotation)))
             (else
              (set-local-bindings! environment
                                   (cons (cons name denotation)
                                         (local-bindings environment))))))
     (lambda (environment)              ;seal!
       environment ;ignore
       (if #f #f))
     (lambda (environment name)         ;alias
       (make-alias name (alias-token environment) environment #f))
     (lambda (environment)              ;disclose
       `(EXTENDED ,(map car (local-bindings environment))))
     (lambda (environment procedure)    ;for-each-binding
       (for-each (lambda (binding)
                   (procedure (car binding) (cdr binding)))
                 (local-bindings environment))))))

;;;; Filtered Environments

(define (syntactic-filter default-environment token special-environment)
  (make-syntactic-environment
   filtered-syntactic-operations
   (syntactic-environment/parameters default-environment)
   default-environment
   (cons special-environment token)))

(define filtered-syntactic-operations
  (let ()
    (define (default-environment environment)
      (syntactic-environment/parent environment))
    (define (special-environment environment)
      (car (syntactic-environment/data environment)))
    (define (filter-token environment)
      (cdr (syntactic-environment/data environment)))
    (define (choose-parent environment name)
      ((if (and (alias? name)
                (eq? (alias/token name)
                     (filter-token environment)))
           special-environment
           default-environment)
       environment))
    (make-syntactic-operations
     (lambda (environment name)         ;lookup
       (syntactic-lookup (choose-parent environment name) name))
     (lambda (environment name denotation) ;bind!
       (error "Definitions illegal in filtered environment:"
              environment name denotation))
     (lambda (environment)              ;seal!
       environment ;ignore
       (if #f #f))
     (lambda (environment name)         ;alias
       ;; I'm not exactly sure what this means...
       (syntactic-alias (choose-parent environment name) name))
     (lambda (environment)              ;disclose
       `(FILTERED ,(filter-token environment)))
     (lambda (environment procedure)    ;for-each-binding
       (for-each-syntactic-binding (special-environment environment)
         (let ((token (filter-token environment)))
           (lambda (name denotation)
             (if (and (alias? name)
                      (eq? token (alias/token name)))
                 (procedure name denotation)))))))))
