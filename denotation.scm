;;; -*- Mode: Scheme -*-

;;;; Explicit Renaming Macros
;;;; Denotations

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-record-type <classifier>
    (make-classifier name procedure)
    classifier?
  (name classifier/name)
  (procedure classifier/procedure))

(define-record-type <transformer>
    (make-transformer environment auxiliary-names procedure source)
    transformer?
  (environment transformer/environment)
  (auxiliary-names transformer/auxiliary-names)
  (procedure transformer/procedure)
  (source transformer/source))

(define-record-type <variable>
    (make-variable name location)
    variable?
  (name variable/name)
  (location variable/location))

(define (denotation=? denotation-a denotation-b)
  (or (eq? denotation-a denotation-b)
      (and (variable? denotation-a)
           (variable? denotation-b)
           (eq? (variable/location denotation-a)
                (variable/location denotation-b)))))
