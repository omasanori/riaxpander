;;; -*- Mode: Scheme -*-

;;;; Explicit Renaming Macros
;;;; Chicken Port
;;;; <http://www.call-with-current-continuation.org/>

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(include "history")
(include "name")
(include "denotation")
(include "environment")
(include "transform")
(include "taxonomy")
(include "classify")
(include "standard")
(include "sexp")

(define (exrename:expand form)
  (set! *alias-uid* 0)
  ((lambda (results)
     (if (and (pair? results)
              (null? (cdr results)))
         (car results)
         `(BEGIN ,@results)))
   (map (lambda (item)
          (strip-syntax*
           (cond ((binding? item) (chicken/compile-binding item))
                 ((expression? item) (chicken/compile-expression item))
                 (else (error "Invalid top-level item:" item)))))
        (let ((forms (list form))
              (environment (make-chicken-environment)))
          (scan-top-level identity-selector
                          forms
                          environment
                          (make-top-level-history forms environment))))))

(define (install-expander)
  (set! ##sys#compiler-toplevel-macroexpand-hook exrename:expand)
  (set! ##sys#interpreter-toplevel-macroexpand-hook exrename:expand)
  (set! macroexpand
        (lambda (expression . me)
          me                            ;ignore -- what is this?
          (exrename:expand expression))))

(define (chicken/meta-evaluate expression environment)
  environment                           ;ignore
  ((##sys#eval-handler) expression))

(define (chicken/compile-expression expression)
  ((expression/compiler expression)))

(define (chicken/compile-expressions expressions)
  (map chicken/compile-expression expressions))

(define (chicken/compile-binding binding)
  `(DEFINE ,(name->symbol (local-variable/rename (binding/variable binding)))
     ,(chicken/compile-expression (binding/expression binding))))

(define (chicken/self-evaluating? datum)
  (not (or (pair? datum)
           (name? datum))))

(define (make-chicken-environment)
  (let ((environment
         (make-syntactic-environment chicken/syntactic-operations
                                     chicken/syntactic-parameters
                                     #f
                                     '())))
    (apply-macrology (chicken-macrology) environment)
    environment))

(define (chicken-macrology)
  (compose-macrologies
   (standard-macrology)
   (non-standard-syntax-macrology)))

(define chicken/syntactic-operations
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
             (else
              (make-free-variable name))))
     (lambda (environment name denotation) ;bind!
       ;++ Promote local variables to top-level variables here.
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
       '(CHICKEN))
     (lambda (environment procedure)    ;for-each-binding
       (for-each (lambda (binding)
                   (procedure (car binding) (cdr binding)))
                 (global-bindings environment))))))

(define chicken/syntactic-parameters
  (lambda (key)
    (cond ((eq? key local-variable-classifier) chicken/classify-local-variable)
          ((eq? key top-level-variable-classifier)
           chicken/classify-top-level-variable)
          ((eq? key free-variable-classifier) chicken/classify-free-variable)
          ((eq? key datum-classifier) chicken/classify-datum)
          ((eq? key self-evaluating?) chicken/self-evaluating?)
          ((eq? key combination-classifier) chicken/classify-combination)
          ((eq? key quotation-compiler) chicken/quotation-compiler)
          ((eq? key expression-sequence-compiler)
           chicken/expression-sequence-compiler)
          ((eq? key conditional-compiler) chicken/conditional-compiler)
          ((eq? key lambda-compiler) chicken/lambda-compiler)
          ((eq? key lambda-bvl-mapper) chicken/map-lambda-bvl)
          ((eq? key meta-evaluator) chicken/meta-evaluate)
          (else #f))))

(define (chicken/classify-datum datum environment history)
  environment                           ;ignore
  (values (make-expression (lambda () datum))
          history))

(define (chicken/classify-free-variable name environment history)
  environment history                   ;ignore
  (chicken/make-variable-location name))

(define (chicken/classify-top-level-variable name location environment history)
  location environment history          ;ignore
  (chicken/make-variable-location name))

(define (chicken/classify-local-variable name rename environment history)
  name environment history              ;ignore
  (chicken/make-variable-location rename))

(define (chicken/make-variable-location name)
  (make-location (lambda () (name->symbol name))
                 (lambda (expression assignment-history)
                   assignment-history   ;ignore
                   (lambda ()
                     `(SET! ,(name->symbol name)
                            ,(chicken/compile-expression expression))))))

(define (chicken/classify-combination operator operands environment history)
  (values (make-location
           (lambda ()
             (chicken/compile-combination
              (chicken/compile-expression operator)
              (chicken/compile-expressions operands)
              history))
           (lambda (expression assignment-history)
             (lambda ()
               (chicken/compile-combination
                `(##SYS#SETTER ,(chicken/compile-expression operator))
                (chicken/compile-expressions
                 (append operands (list expression)))
                assignment-history))))
          history))

(define (chicken/compile-combination operator operands history)
  (let ((input-form
         (and history (reduction/form (history/current-reduction history))))
        (output-form `(,operator ,@operands)))
    (chicken/clobber-source-record input-form output-form)
    output-form))

(define (chicken/clobber-source-record input-form output-form)
  (if (and ##sys#line-number-database input-form)
      (let ((input-operator (car input-form))
            (output-operator (car output-form)))
        (define (original-bucket)
          (##sys#hash-table-ref ##sys#line-number-database input-operator))
        (cond ((eq? input-operator output-operator)
               (cond ((assq input-form (or (original-bucket) '()))
                      => (lambda (cell)
                           (set-cdr! cell output-form)))))
              ((get-line-number input-form)
               => (lambda (line)
                    (##sys#hash-table-set!
                     ##sys#line-number-database
                     output-operator
                     (cons (cons output-form line) (original-bucket)))))))))

(define (chicken/quotation-compiler datum history)
  history                               ;ignore
  (lambda ()
    (if (chicken/self-evaluating? datum)
        datum
        `',datum)))

(define (chicken/expression-sequence-compiler expressions history)
  history                               ;ignore
  (lambda ()
    `(BEGIN ,@(chicken/compile-expressions expressions))))

(define (chicken/conditional-compiler condition consequent alternative history)
  history                               ;ignore
  (lambda ()
    `(IF ,(chicken/compile-expression condition)
         ,(chicken/compile-expression consequent)
         ,@(if alternative
               `(,(chicken/compile-expression alternative))
               '()))))

(define (chicken/lambda-compiler bvl body environment history)
  history                               ;ignore
  (lambda ()
    `(LAMBDA ,(chicken/%map-lambda-bvl bvl
                (lambda (variable)
                  (name->symbol (local-variable/rename variable))))
       ,@(chicken/compile-lambda-body body environment))))

(define (chicken/compile-lambda-body body environment)
  (receive (bindings expressions)
           (scan-body (sequence/selector body)
                      (sequence/subforms body)
                      (syntactic-extend environment)
                      (sequence/history body))
    `(,@(map chicken/compile-binding bindings)
      ,@(map chicken/compile-expression expressions))))

(define (chicken/map-lambda-bvl bvl history procedure)
  (if (not (chicken/valid-bvl? bvl))
      (syntax-error "Invalid lambda BVL:" history bvl)
      (chicken/%map-lambda-bvl bvl procedure)))

(define (chicken/valid-bvl? bvl)
  (or (##sys#extended-lambda-list? bvl)
      (let loop ((bvl bvl) (seen '()))
        (cond ((pair? bvl)
               (and (name? (car bvl))
                    (not (memq (car bvl) seen))
                    (loop (cdr bvl) (cons (car bvl) seen))))
              ((null? bvl)
               #t)
              (else
               (name? bvl))))))

(define (chicken/%map-lambda-bvl bvl procedure)
  (let recur ((bvl bvl))
    (cond ((pair? bvl)
           (cons (procedure (car bvl))
                 (recur (cdr bvl))))
          ((null? bvl)
           '())
          (else
           (procedure bvl)))))
