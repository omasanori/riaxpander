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

(define-record-type <top-level-variable>
    (make-top-level-variable name location)
    top-level-variable?
  (name top-level-variable/name)
  (location top-level-variable/location))

(define-record-type <local-variable>
    (make-local-variable name rename)
    local-variable?
  (name local-variable/name)
  (rename local-variable/rename))

(define-record-type <free-variable>
    (make-free-variable name)
    free-variable?
  (name free-variable/name))

(define-record-type <reserved>
    (make-reserved name)
    reserved?
  (name reserved/name))

(define (denotation=? denotation-a denotation-b)
  (or (eq? denotation-a denotation-b)
      (and (top-level-variable? denotation-a)
           (top-level-variable? denotation-b)
           (eq? (top-level-variable/location denotation-a)
                (top-level-variable/location denotation-b)))
      (and (free-variable? denotation-a)
           (free-variable? denotation-b)
           (eq? (free-variable/name denotation-a)
                (free-variable/name denotation-b)))))
