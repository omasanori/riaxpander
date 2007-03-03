;;; -*- Mode: Scheme -*-

;;;; Explicit Renaming Macros
;;;; Form Classification Taxonomy 

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

;;; This file implements the types of possible values returned by CLASSIFY.

;;; Keywords are wrappers for syntactic denotations that may appear
;;; only at the heads of forms.  Keywords should be seen only inside
;;; the classifier.

(define-record-type <keyword>
    (make-keyword name denotation)
    keyword?
  (name keyword/name)
  (denotation keyword/denotation))

;;; Expressions, well, express values.  Locations represent the places
;;; where values can be stored, of which there may be access and
;;; assignment.  Locations, then, may double as expressions.
;;; Variable references may be classified as expressions or locations
;;; depending on the whims of the syntactic environment.

;;; It would be nice to have record subtypes here.  Locations are a
;;; subtype of expressions, really.

(define-record-type <expression>
    (make-expression compiler)
    direct-expression?
  (compiler direct-expression/compiler))

;;; The names of the fields in locations are misleading.  The
;;; expression compiler is supplied by the client, and may be
;;; represented however it pleases; the assignment compiler is a
;;; binary procedure that takes the expression whose value to store in
;;; the location, and the history of the assignment, and returns an
;;; expression compiler in the previous sense.

(define-record-type <location>
    (make-location expression-compiler assignment-compiler)
    location?
  (expression-compiler location/expression-compiler)
  (assignment-compiler location/assignment-compiler))

(define (expression? object)
  (or (direct-expression? object)
      (location? object)))

(define (expression/compiler expression)
  (if (direct-expression? expression)
      (direct-expression/compiler expression)
      (location/expression-compiler expression)))

;;;; Sequences and Definitions

;;; Sequences suffer from a split personality disorder: they're not
;;; quite sure whether they are top-level collections of expressions
;;; and definitions together, whether they are sequences of internal
;;; definitions, or whether they are sequenced expressions.  The
;;; discrimination of sequences into these three categories is delayed
;;; until their context is known, and the subforms are not classified
;;; until then either.

(define-record-type <sequence>
    (make-sequence selector form history)
    sequence?
  (selector sequence/selector)
  (form sequence/form)
  (history sequence/history))

(define (sequence/subforms sequence)
  (apply-selector (sequence/selector sequence) (sequence/form sequence)))

;;; Definitions represent, well, definitions.  They subsume both
;;; variable definitions and syntactic definitions; the distinction
;;; between the two is specified by the binder, which is a procedure
;;; that modifies an environment appropriately.

(define-record-type <definition>
    (make-definition binder name-selector subform-selector form history)
    definition?
  (binder definition/binder)
  (name-selector definition/name-selector)
  (subform-selector definition/subform-selector)
  (form definition/form)
  (history definition/history))

(define (definition/name definition)
  (apply-selector (definition/name-selector definition)
                  (definition/form definition)))

(define (definition/subform definition)
  (apply-selector (definition/subform-selector definition)
                  (definition/form definition)))

;++ What about declarations?  I think they fall into a category
;++ similar to definitions.

;;; After a variable definition has been processed, it yields a fully
;;; classified `binding' in the output.  This is a bit of a kludge,
;;; but I don't have time to think of a better way to do this.
;;; Actually, it shouldn't be fully classified anyway, to do check
;;; internal definitions thoroughly.

(define-record-type <binding>
    (make-binding variable expression)
    binding?
  (variable binding/variable)
  (expression binding/expression))
