;;; -*- Mode: Scheme -*-

;;;; Explicit Renaming Macros
;;;; S-Expression Client

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define (sexp/expand form)
  (sexp/expand* (list form)))

(define (sexp/expand* forms)
  ((lambda (results)
     (if (and (pair? results)
              (null? (cdr results)))
         (car results)
         `(BEGIN ,@results)))
   (map (lambda (item)
          (strip-syntax*
           (cond ((binding? item) (sexp/compile-binding item))
                 ((expression? item) (sexp/compile-expression item))
                 (else (error "Invalid item in output:" item)))))
        (let ((environment (make-sexp-environment)))
          (scan-top-level identity-selector
                          forms
                          environment
                          (make-top-level-history forms environment))))))

(define (make-sexp-environment)
  (let ((environment
         (make-syntactic-environment sexp/syntactic-operations
                                     sexp/syntactic-parameters
                                     #f
                                     '())))
    (apply-macrology (sexp/macrology) environment)
    environment))

(define (sexp/macrology)
  (standard-macrology))

(define (sexp/meta-evaluate expression environment)
  environment                           ;ignore
  (eval expression (interaction-environment)))

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
             (else
              (make-free-variable name))))
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
    (cond ((eq? key local-variable-classifier) sexp/classify-local-variable)
          ((eq? key top-level-variable-classifier)
           sexp/classify-top-level-variable)
          ((eq? key free-variable-classifier) sexp/classify-free-variable)
          ((eq? key datum-classifier) sexp/classify-datum)
          ((eq? key self-evaluating?) sexp/self-evaluating?)
          ((eq? key combination-classifier) sexp/classify-combination)
          ((eq? key expression-sequence-compiler)
           sexp/expression-sequence-compiler)
          ((eq? key conditional-compiler) sexp/conditional-compiler)
          ((eq? key quotation-compiler) sexp/quotation-compiler)
          ((eq? key lambda-compiler) sexp/lambda-compiler)
          ((eq? key lambda-bvl-mapper) sexp/map-lambda-bvl)
          ((eq? key meta-evaluator) sexp/meta-evaluate)
          (else #f))))

;;;; S-Expression Parameters

(define (sexp/classify-datum datum environment history)
  environment                           ;ignore
  (if (sexp/self-evaluating? datum)
      (values (make-expression (lambda () datum))
              history)
      (syntax-error "Invalid form:" history datum)))

(define (sexp/self-evaluating? datum)
  (or (boolean? datum)
      (char? datum)
      (number? datum)
      (string? datum)))

(define (sexp/classify-free-variable name environment history)
  environment history                   ;ignore
  (sexp/make-variable-location name))

(define (sexp/classify-local-variable name rename environment history)
  name environment history              ;ignore
  (sexp/make-variable-location rename))

(define (sexp/classify-top-level-variable name location environment history)
  location environment history          ;ignore
  (sexp/make-variable-location name))

(define (sexp/make-variable-location name)
  (make-location (lambda ()
                   name)
                 (lambda (expression history)
                   history              ;ignore
                   (lambda ()
                     `(SET! ,name ,(sexp/compile-expression expression))))))

(define (sexp/classify-combination operator operands environment history)
  environment                           ;ignore
  (values (make-expression
           (lambda ()
             (sexp/compile-combination operator operands history)))
          history))

(define (sexp/compile-expression expression)
  ((expression/compiler expression)))

(define (sexp/compile-expressions expressions)
  (map sexp/compile-expression expressions))

(define (sexp/compile-binding binding)
  `(DEFINE ,(local-variable/rename (binding/variable binding))
     ,(sexp/compile-expression (binding/expression binding))))

(define (sexp/compile-combination operator operands history)
  history                               ;ignore
  `(,(sexp/compile-expression operator)
    ,@(sexp/compile-expressions operands)))

(define (sexp/quotation-compiler datum history)
  history                               ;ignore
  (if (sexp/self-evaluating? datum)
      (lambda () datum)
      (lambda () `',datum)))

(define (sexp/expression-sequence-compiler expressions history)
  history                               ;ignore
  (lambda ()
    `(BEGIN ,@(map sexp/compile-expression expressions))))

(define (sexp/conditional-compiler condition consequent alternative history)
  history                               ;ignore
  (lambda ()
    `(IF ,(sexp/compile-expression condition)
         ,(sexp/compile-expression consequent)
         ,@(if alternative
               `(,(sexp/compile-expression alternative))
               '()))))

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

(define (sexp/lambda-compiler bvl body environment history)
  (lambda ()
    `(LAMBDA ,(sexp/%map-lambda-bvl bvl local-variable/rename)
       ,@(sexp/compile-lambda-body body environment))))

(define (sexp/compile-lambda-body body environment)
  (receive (bindings expressions)
           (scan-body (sequence/selector body)
                      (sequence/subforms body)
                      (syntactic-extend environment)
                      (sequence/history body))
    `(,@(map sexp/compile-binding bindings)
      ,@(map sexp/compile-expression expressions))))

