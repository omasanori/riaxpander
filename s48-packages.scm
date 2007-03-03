;;; -*- Mode: Scheme; scheme48-package: (config) -*-

;;;; Explicit Renaming Macros
;;;; Scheme48 Package Definitions

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-structure syntactic syntactic-interface
  (open scheme
        receiving
        syntactic-standard-macrology
        syntactic-classifier
        syntactic-taxonomy
        syntactic-environments
        syntactic-denotations
        syntactic-names
        syntactic-history
        syntactic-errors
        )
  (optimize auto-integrate)
  (files sexp))

(define-structure syntactic-standard-macrology
    syntactic-standard-macrology-interface
  (open scheme
        receiving
        syntactic-classifier
        syntactic-transformation
        syntactic-taxonomy
        syntactic-environments
        syntactic-denotations
        syntactic-names
        syntactic-history
        syntactic-errors
        )
  (optimize auto-integrate)
  (files standard))

(define-structure syntactic-classifier syntactic-classifier-interface
  (open scheme
        receiving
        syntactic-transformation
        syntactic-taxonomy
        syntactic-denotations
        syntactic-environments
        syntactic-names
        syntactic-history
        syntactic-errors
        structure-refs
        )
  (access syntactic-error-hooks)
  (optimize auto-integrate)
  (begin
    (define (append-reverse list tail)
      (if (pair? list)
          (append-reverse (cdr list) (cons (car list) tail))
          tail))
    (define (append-map procedure list)
      (if (pair? list)
          (append (procedure (car list))
                  (append-map procedure (cdr list)))
          '())))
  (files classify)
  (begin
    ((structure-ref syntactic-error-hooks SET-RECLASSIFIER!)
     reclassify)))

(define-structure syntactic-transformation syntactic-transformation-interface
  (open scheme
        syntactic-environments
        syntactic-denotations
        syntactic-names
        )
  (optimize auto-integrate)
  (files transform))

(define-structure syntactic-taxonomy syntactic-taxonomy-interface
  (open scheme
        srfi-9+                         ;define-record-type
        syntactic-history
        )
  (optimize auto-integrate)
  (files taxonomy))

(define-structure syntactic-environments syntactic-environments-interface
  (open scheme
        srfi-9+                         ;define-record-type
        syntactic-denotations
        syntactic-names
        syntactic-errors
        )
  (optimize auto-integrate)
  (begin
    (define (reduce combiner identity list)
      (if (pair? list)
          (let loop ((result (car list))
                     (list (cdr list)))
            (if (pair? list)
                (loop (combiner (car list) result)
                      (cdr list))
                result))
          identity)))
  (files environment)
  (begin
    (define-record-discloser <syntactic-environment>
      (lambda (environment)
        (cons 'SYNTACTIC-ENVIRONMENT
              (disclose-syntactic-environment environment))))))

(define-structure syntactic-denotations syntactic-denotations-interface
  (open scheme
        srfi-9+                         ;define-record-type
        )
  (optimize auto-integrate)
  (files denotation)
  (begin
    (define-record-discloser <classifier>
      (lambda (classifier)
        (list 'CLASSIFIER
              (classifier/name classifier)
              (classifier/procedure classifier))))
    (define-record-discloser <transformer>
      (lambda (transformer)
        (list 'TRANSFORMER
              (transformer/procedure transformer)
              (transformer/source transformer))))
    (define-record-discloser <free-variable>
      (lambda (variable)
        (list 'VARIABLE '(FREE)
              (free-variable/name variable))))
    (define-record-discloser <top-level-variable>
      (lambda (variable)
        (list 'VARIABLE '(TOP-LEVEL)
              (top-level-variable/name variable)
              (top-level-variable/location variable))))
    (define-record-discloser <local-variable>
      (lambda (variable)
        (list 'VARIABLE '(LOCAL)
              (local-variable/name variable)
              (local-variable/rename variable))))
    (define-record-discloser <reserved>
      (lambda (reserved)
        (list 'RESERVED (reserved/name reserved))))))

(define-structure syntactic-names syntactic-names-interface
  (open scheme
        srfi-9+                         ;define-record-type
        )
  (optimize auto-integrate)
  (files name)
  (begin
    (define-record-discloser <alias>
      (lambda (alias)
        (list 'ALIAS
              (alias/name alias)
              (alias/introducer alias)
              (alias/uid alias))))))

(define-structure syntactic-history syntactic-history-interface
  (open scheme)
  (optimize auto-integrate)
  (begin
    (define (append-reverse list tail)
      (if (pair? list)
          (append-reverse (cdr list) (cons (car list) tail))
          tail))
    (define (last list)
      (let ((tail (cdr list)))
        (if (pair? tail)
            (last tail)
            (car list)))))
  (files history))

(define-structures ((syntactic-errors syntactic-errors-interface)
                    (syntactic-error-hooks (export set-reclassifier!)))
  (open scheme
        (modify signals (prefix scheme/))
        loopholes
        )
  (optimize auto-integrate)
  (begin

    (define error
      (lambda (message . irritants)
        (apply scheme/error message irritants)))

    (define syntax-error
      (lambda (message history . irritants)
        history                         ;ignore
        (apply scheme/error message irritants)))

    (define classify-error
      (lambda (message history . irritants)
        history                         ;ignore
        (apply scheme/error message irritants)))

    (define (set-reclassifier! reclassifier)
      reclassifier                      ;ignore
      (values))

    ))

(define-structure srfi-9+
    (export
      (define-record-type :syntax)
      define-record-discloser)
  (open scheme
        srfi-9
        (subset define-record-types (define-record-discloser))))
