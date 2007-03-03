;;; -*- Mode: Scheme -*-

;;;; Explicit Renaming Macros
;;;; MIT Scheme Port
;;;; <http://www.gnu.org/software/mit-scheme/>

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define condition-type:syntax-error
  (make-condition-type 'SYNTAX-ERROR condition-type:error
      '(MESSAGE IRRITANTS HISTORY)
    (lambda (condition output-port)
      (format-error-message (access-condition condition 'MESSAGE)
                            (access-condition condition 'IRRITANTS)
                            output-port))))

(define signal-syntax-error
  (let ((make-condition
         (condition-constructor condition-type:syntax-error
                                '(MESSAGE IRRITANTS HISTORY))))
    (lambda (message irritants history wrapper)
      (call-with-current-continuation
        (lambda (continuation)
          ((lambda (signal)
             (if history
                 (with-syntax-replacement-restart (wrapper continuation)
                   signal)
                 (signal)))
           (lambda ()
             (let ((condition
                    (make-condition continuation 'BOUND-RESTARTS
                                    message irritants history)))
               (signal-condition condition)
               (standard-error-handler condition)))))))))

(define (with-syntax-replacement-restart continuation thunk)
  (with-restart 'USE-VALUE "Use a different form."
      continuation
      (lambda ()
        (values (prompt-for-expression "New form (not evaluated)")))
    thunk))

(define (syntax-error message history . irritants)
  (signal-syntax-error message irritants history identity-procedure))

(define (classify-error message history . irritants)
  (signal-syntax-error message irritants history
    (lambda (continuation)
      (lambda (form)
        ;** Be careful of multiple return values here.  They're broken
        ;** in MIT Scheme, so continuations are unary.
        (continuation (hook/reclassify form history))))))

(define (hook/reclassify form history)
  form history                          ;ignore
  (error "Classifier not yet available."))

(with-working-directory-pathname
    (directory-pathname (current-load-pathname))
  (lambda ()
    (for-each load
              '(
                "history"
                "name"
                "denotation"
                "environment"
                "transform"
                "taxonomy"
                "classify"
                "standard"
                ;; For now, we just use S-expression output.  Later
                ;; there ought to be an scode generator.
                "sexp"
                ))))

;;; Make things print more nicely.

(set-record-type-unparser-method! <alias>
  (simple-unparser-method 'ALIAS
    (lambda (alias)
      (list (name->symbol alias)))))

(set-record-type-unparser-method! <free-variable>
  (simple-unparser-method 'VARIABLE
    (lambda (variable)
      (list 'FREE
            (name->symbol (free-variable/name variable))))))

(set-record-type-unparser-method! <top-level-variable>
  (simple-unparser-method 'VARIABLE
    (lambda (variable)
      (list 'TOP-LEVEL
            (name->symbol (top-level-variable/name variable))
            (top-level-variable/location variable)))))

(set-record-type-unparser-method! <local-variable>
  (simple-unparser-method 'VARIABLE
    (lambda (variable)
      (list 'LOCAL
            (name->symbol (local-variable/name variable))
            (name->symbol (local-variable/rename variable))))))

(set-record-type-unparser-method! <syntactic-environment>
  (simple-unparser-method 'SYNTACTIC-ENVIRONMENT
    disclose-syntactic-environment))

(set! hook/reclassify reclassify)
