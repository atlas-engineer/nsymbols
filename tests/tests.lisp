;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package #:nsymbols/tests)

(define-test cl-inspection ()
  (assert-eql 977 (length (nsymbols:package-symbols :cl :visibility :external)))
  (let ((structures (nsymbols:package-structures :cl :external)))
    (assert-true (member 'structure-object structures)))
  (let ((classes (nsymbols:package-classes :cl :external)))
    (assert-true (member 'class classes))
    (assert-true (member 'structure-class classes))
    (assert-true (member 'standard-class classes))
    (assert-true (member 'method classes))))

(define-test nsymbols-inspection ()
  (let ((functions (nsymbols:package-functions :nsymbols :internal)))
    (assert-eql 2 (length functions))
    (assert-true (member 'nsymbols::designator functions)))
  (let ((macros (nsymbols:package-macros :nsymbols)))
    (assert-eql 1 (length macros))
    (assert-eq 'nsymbols:define-symbol-type (first macros)))
  ;; CCL compiles conditions as classes.
  (assert-eql #-ccl 0
              #+ccl 1
              (length (nsymbols:package-classes :nsymbols)))
  (assert-eql 0 (length (nsymbols:package-structures :nsymbols)))
  (assert-eql 3 (length (nsymbols:package-variables :nsymbols)))
  (assert-eql 2 (length (nsymbols:package-variables :nsymbols :external)))
  (assert-eql 1 (length (nsymbols:package-variables :nsymbols :internal))))

(defun hello ()
  nil)

(defclass what () ())

(defvar hey 'hey)

(define-test resolve-symbol ()
  (assert-equalp "HEY"
                 (symbol-name (nsymbols:resolve-symbol 'hey :variable :nsymbols/tests)))
  (assert-equalp "WHAT"
                 (symbol-name (nsymbols:resolve-symbol :what :class :nsymbols/tests)))
  (assert-equalp "HELLO"
                 (symbol-name (nsymbols:resolve-symbol "HELLO" :function :nsymbols/tests)))
  (assert-eq nil (nsymbols:resolve-symbol "HELLO" :variable :nsymbols/tests))
  (assert-eq 'cl:most-positive-fixnum
             (nsymbols:resolve-symbol :most-positive-fixnum :variable :cl)))

(nsymbols:define-symbol-type hey (variable)
  (eq nsymbols:%symbol% hey))

(define-test custom-type ()
  (assert-true (nsymbols:resolve-symbol :hey :hey :nsymbols/tests))
  (assert-eq 'hey (nsymbols:resolve-symbol :hey :hey :nsymbols/tests)))
