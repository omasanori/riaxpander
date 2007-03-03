;;; -*- Mode: Scheme -*-

;;;; Explicit Renaming Macros
;;;; Names and Aliases

;;; A name is either a symbol or an alias.  The meaning of a symbol in
;;; some environment is whatever that environment maps the symbol to.
;;; If a macro M introduces an alias A for a name N in M's output, the
;;; meaning of A in the environment of the macro's usage is the
;;; meaning of N in M's closing environment.  The token is an
;;; identifier unique to the invocation of the macro that created the
;;; aliases.

(define (name? object)
  (or (symbol? object)
      (alias? object)))

(define-record-type <alias>
    (%make-alias name token environment introducer)
    alias?
  (name alias/name)
  (token alias/token)
  (environment alias/environment)
  (introducer alias/introducer))

(define (make-alias name token environment introducer)
  (%make-alias name token environment introducer))

(define (generate-alias name environment introducer)
  (make-alias name (make-alias-token) environment introducer))

(define (make-alias-token)
  (cons #f '()))

;++ thread synchronization (foo)

(define *alias-uid* 0)

(define (increment-alias-uid)
  (let ((uid *alias-uid*))
    (set! *alias-uid* (+ uid 1))
    uid))

(define (alias/uid alias)
  (let ((token (alias/token alias)))
    (or (car token)
        (let ((uid (increment-alias-uid)))
          (set-car! token uid)
          uid))))

(define (name/original-symbol name)
  (let loop ((name name))
    (if (not (alias? name))
        name
        (loop (alias/name name)))))

(define (name->symbol name)
  (if (symbol? name)
      name
      (string->symbol
       (let recur ((name name))
         (string-append (let ((name* (alias/name name)))
                          (if (symbol? name*)
                              (symbol->string name*)
                              (recur name*)))
                        "#"
                        (number->string (alias/uid name)))))))

(define (strip-syntax form)
  (%strip-syntax form name/original-symbol))

(define (strip-syntax* form)
  (%strip-syntax form name->symbol))

(define (%strip-syntax form name-stripper)
  (let recur ((form form))
    (cond ((name? form)
           (name-stripper form))
          ((pair? form)
           (let ((a (recur (car form)))
                 (d (recur (cdr form))))
             (if (and (eq? a (car form))
                      (eq? d (cdr form)))
                 form
                 (cons a d))))
          (else form))))
