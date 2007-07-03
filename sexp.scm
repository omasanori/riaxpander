;;; -*- Mode: Scheme -*-

;;;; Explicit Renaming Macros
;;;; S-Expression Client

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define (sexp/expand form environment)
  (sexp/expand* (list form) environment))

(define (sexp/expand* forms environment)
  ((lambda (results)
     (if (and (pair? results)
              (null? (cdr results)))
         (car results)
         `(BEGIN ,@results)))
   (map (lambda (item)
          (cond ((binding? item) (sexp/compile-binding item))
                ((expression? item) (sexp/compile-expression item))
                (else (error "Invalid item in output:" item))))
        (scan-top-level identity-selector
                        forms
                        environment
                        (make-top-level-history forms environment)))))

(define (sexp/meta-evaluate expression environment)
  ;; ENVIRONMENT is a syntactic environment, not an R5RS environment
  ;; specifier (or `evaluation environment', perhaps), and is
  ;; therefore unsuitable as an argument to EVAL.
  (eval (sexp/expand expression environment) (interaction-environment)))

(define (sexp/reduce-name name environment)
  ;++ This is a kludge.  The effect is to generate clean names from
  ;++ aliases for names that are bound in top-level environments, and
  ;++ to leave local variables ugly.
  (let ((environment*
         (let loop ((environment environment))
           (cond ((syntactic-environment/parent environment)
                  => loop)
                 (else environment)))))
    (let loop ((name name))
      (if (alias? name)
          (if (name=? environment name environment* (alias/name name))
              (loop (alias/name name))
              (name->symbol name))
          name))))

;;;; S-Expression Syntactic Environment

(define (make-sexp-environment)
  (let ((environment
         (make-syntactic-environment sexp/syntactic-operations
                                     sexp/syntactic-parameters
                                     #f
                                     '())))
    (apply-macrology (sexp/macrology) environment)
    environment))

(define (sexp/macrology)
  (compose-macrologies                  ;Alphabetically listed, for no reason.
   (macrology/standard-assignment)
   (macrology/standard-conditional sexp/compile-conditional)
   (macrology/standard-definition)
   (macrology/standard-derived-syntax)
   (macrology/standard-keyword-definition)
   (macrology/standard-lambda sexp/compile-lambda sexp/map-lambda-bvl)
   (macrology/standard-quotation sexp/compile-quotation)
   (macrology/standard-sequence)
   (macrology/standard-syntactic-binding)))

(define sexp/syntactic-operations
  (let ()
    (define (global-bindings environment)
      (syntactic-environment/data environment))
    (define (set-global-bindings! environment bindings)
      (set-syntactic-environment/data! environment bindings))
    (make-syntactic-operations
     (lambda (environment name)         ;lookup
       (cond ((assq name (global-bindings environment))
              => cdr)
             ((alias? name)
              (syntactic-lookup (alias/environment name)
                                (alias/name name)))
             (else #f)))
     (lambda (environment name denotation) ;bind!
       (set-global-bindings! environment
                             (cons (cons name denotation)
                                   (global-bindings environment))))
     (lambda (environment)              ;seal!
       environment ;ignore
       (if #f #f))
     (lambda (environment name)         ;alias
       environment ;ignore
       name)
     (lambda (environment)              ;disclose
       environment ;ignore
       '(SEXP))
     (lambda (environment procedure)    ;for-each-binding
       (for-each (lambda (binding)
                   (procedure (car binding) (cdr binding)))
                 (global-bindings environment))))))

(define sexp/syntactic-parameters
  (lambda (key)
    (cond ((eq? key variable-classifier) sexp/classify-variable)
          ((eq? key free-variable-classifier) sexp/classify-free-variable)
          ((eq? key datum-classifier) sexp/classify-datum)
          ((eq? key self-evaluating?) sexp/self-evaluating?)
          ((eq? key combination-classifier) sexp/classify-combination)
          ((eq? key meta-evaluator) sexp/meta-evaluate)
          (else #f))))

;;;;; S-Expression Syntactic Parameters

(define (sexp/classify-datum datum environment history)
  environment                           ;ignore
  (if (sexp/self-evaluating? datum)
      (values (make-expression (lambda () datum)) history)
      (classify-error "Inevaluable datum:" history datum)))

(define (sexp/self-evaluating? datum)
  (or (boolean? datum)
      (char? datum)
      (number? datum)
      (string? datum)))

(define (sexp/classify-variable name location reference environment history)
  name reference                        ;ignore
  (sexp/make-variable-location location environment history))

(define (sexp/classify-free-variable name environment history)
  (sexp/make-variable-location name environment history))

(define (sexp/make-variable-location name environment history)
  (values (make-location (lambda () (sexp/reduce-name name environment))
                         (lambda (expression assignment-history)
                           assignment-history ;ignore
                           `(SET! ,(sexp/reduce-name name environment)
                                  ,(sexp/compile-expression expression))))
          history))

(define (sexp/classify-combination operator operator-history
                                   selector forms environment history)
  (cond ((not (expression? operator))
         (classify-error "Non-expression in operator position of combination:"
                         operator-history
                         operator))
        ((not (list? forms))
         (classify-error "Invalid operands in combination -- improper list:"
                         history
                         forms))
        (else
         (values
          (make-expression
           (lambda ()
             (sexp/compile-combination
              operator
              (classify-subexpressions selector forms environment history)
              history)))
          history))))

;;;;; S-Expression Compilers

(define (sexp/compile-quotation datum history)
  history                               ;ignore
  (if (sexp/self-evaluating? datum)
      datum
      `',datum))

(define (sexp/compile-conditional condition consequent alternative history)
  history                               ;ignore
  `(IF ,(sexp/compile-expression condition)
       ,(sexp/compile-expression consequent)
       ,@(if alternative
             `(,(sexp/compile-expression alternative))
             '())))

(define (sexp/compile-lambda bvl body environment history)
  history                               ;ignore
  `(LAMBDA ,(sexp/%map-lambda-bvl bvl
              (lambda (variable)
                (sexp/reduce-name (variable/location variable) environment)))
     ,@(sexp/compile-lambda-body body)))

(define (sexp/compile-lambda-body body)
  (receive (bindings expressions)
           (classify/sequence scan-r5rs-body body)
    `(,@(map sexp/compile-binding bindings)
      ,@(map sexp/compile-expression expressions))))

(define (sexp/map-lambda-bvl bvl history procedure)
  (sexp/%map-lambda-bvl (sexp/guarantee-lambda-bvl bvl history) procedure))

(define (sexp/%map-lambda-bvl bvl procedure)
  (let recur ((bvl bvl))
    (cond ((pair? bvl)
           (cons (procedure (car bvl))
                 (recur (cdr bvl))))
          ((null? bvl)
           '())
          (else
           (procedure bvl)))))

(define (sexp/guarantee-lambda-bvl bvl history)
  (let ((lose
         (lambda ()
           (receive (bvl history)
                    (syntax-error "Malformed lambda bound variable list:"
                                  history
                                  bvl)
             (sexp/guarantee-lambda-bvl bvl history)))))
    (let ((original-bvl bvl))
      (let loop ((bvl bvl))
        (cond ((pair? bvl)
               (if (name? (car bvl))
                   (loop (cdr bvl))
                   (lose)))
              ((or (null? bvl) (name? bvl))
               original-bvl)
              (else
               (lose)))))))

;;;;; S-Expression Compilation Utilities

(define (sexp/compile-expression expression)
  (cond ((location? expression)
         ((location/expression-compiler expression)))
        ((sequence? expression)
         (sexp/compile-sequence (classify/sequence scan-expressions expression)
                                (sequence/history expression)))
        (else
         ((expression/compiler expression)))))

(define (sexp/compile-expressions expressions)
  (map sexp/compile-expression expressions))

(define (sexp/compile-combination operator operands history)
  history                               ;ignore
  `(,(sexp/compile-expression operator) ,@(sexp/compile-expressions operands)))

(define (sexp/compile-binding binding)
  `(DEFINE ,(sexp/reduce-name (variable/location (binding/variable binding))
                              (binding/environment binding))
     ,(receive (expression history) ((binding/classifier binding))
        history                         ;ignore
        (sexp/compile-expression expression))))

(define (sexp/compile-sequence expressions history)
  history                               ;ignore
  `(BEGIN ,@(map sexp/compile-expression expressions)))
