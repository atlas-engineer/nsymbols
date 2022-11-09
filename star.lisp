;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package #:nsymbols)

(defun package-functions* (packages &optional (visibility :any))
  "Returns a list of function objects for PACKAGES, visible with VISIBILITY."
  (mapcar #'symbol-function (package-functions packages visibility)))
(export 'package-functions*)

(defun package-generic-functions* (packages &optional (visibility :any))
  "Returns a list of generic function objects for PACKAGES, visible with VISIBILITY."
  (mapcar #'symbol-function (package-generic-functions packages visibility)))
(export 'package-generic-functions*)

(defun package-methods* (packages &optional (visibility :any))
  "Returns a list of method objects (via closer-mop) for PACKAGES, visible with VISIBILITY."
  (apply #'append (mapcar #'closer-mop:generic-function-methods
                          (mapcar #'symbol-function (package-generic-functions packages visibility)))))
(export 'package-methods*)

(defun package-classes* (packages &optional (visibility :any))
  "Returns a list of class objects (as per find-class) for PACKAGES, visible with VISIBILITY."
  (mapcar #'find-class (package-classes packages visibility)))
(export 'package-classes*)

(defun package-structures* (packages &optional (visibility :any))
  "Returns a list of structure objects (as per find-class) for PACKAGES, visible with VISIBILITY."
  (mapcar #'find-class (package-structures packages visibility)))
(export 'package-structures*)
