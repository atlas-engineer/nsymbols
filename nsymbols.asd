;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(defsystem "nsymbols"
  :description "A set of convenience functions to list class, variable, function, and other symbols."
  :author "Atlas Engineer LLC"
  :homepage "https://github.com/atlas-engineer/nsymbols"
  :bug-tracker "https://github.com/atlas-engineer/nsymbols/issues"
  :source-control (:git "https://github.com/atlas-engineer/nsymbols.git")
  :license  "BSD-3 Clause"
  :version "0.3.1"
  :serial t
  :components ((:file "package")
               (:file "nsymbols"))
  :in-order-to ((test-op (test-op "nsymbols/tests"))))

(defsystem "nsymbols/star"
  :description "Versions of regular `nsymbols' package operations, but with more intuitive results."
  :depends-on ("nsymbols" "closer-mop")
  :components ((:file "star")))

(defsystem "nsymbols/tests"
  :depends-on ("nsymbols" "lisp-unit2")
  :serial t
  :pathname "tests/"
  :components ((:file "package")
               (:file "tests"))
  :perform (test-op (op c)
                    (eval-input
                     "(lisp-unit2:run-tests
                       :package :nsymbols/tests
                       :run-contexts #'lisp-unit2:with-summary-context)")))
