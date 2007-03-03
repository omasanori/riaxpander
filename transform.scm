;;; -*- Mode: Scheme -*-

;;;; Explicit Renaming Macros
;;;; Transformer Application

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

;;; I am slightly apprehensive about this syntactic filtration,
;;; because it is essentially non-tail-recursion.

(define (apply-transformer name transformer form usage-environment)
  (let ((closing-environment (transformer/environment transformer))
        (token (make-alias-token)))
    (let ((filtered-environment
           (syntactic-filter usage-environment token closing-environment)))
      (values ((transformer/procedure transformer)
               form
               (make-alias-generator token closing-environment name)
               (make-name-comparator filtered-environment))
              filtered-environment))))

(define (make-alias-generator token environment introducer)
  (let ((cache '()))
    (lambda (name)
      (cdr
       (or (assq name cache)
           (let ((entry
                  (cons name
                        (make-alias name token environment introducer))))
             (set! cache (cons entry cache))
             entry))))))

(define (make-name-comparator environment)
  (lambda (name-a name-b)
    (or (eq? name-a name-b)
        (and (name? name-a)
             (name? name-b)
             (name=? environment name-a environment name-b)))))
