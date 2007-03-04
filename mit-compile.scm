;;; -*- Mode: Scheme -*-

;;;; Explicit Renaming Macros
;;;; MIT Scheme Compile Script

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

((lambda (specifiers)
   (for-each (lambda (specifier)
               (compile-file (car specifier) (cdr specifier)))
             specifiers))
 '(
   ("history")
   ("name")
   ("denotation")
   ("environment" "name" "denotation")
   ("transform" "name" "denotation" "environment")
   ("taxonomy")
   ("classify"
    "history" "name" "denotation" "environment"
    "transform" "taxonomy")
   ("standard"
    "history" "name" "denotation" "environment"
    "transform" "taxonomy" "classify")
   ("sexp"
    "history" "name" "denotation" "environment"
    "transform" "taxonomy" "classify" "standard")
   ("mit-load"
    "history" "name" "denotation" "environment"
    "transform" "taxonomy" "classify" "standard" "sexp")
   ))