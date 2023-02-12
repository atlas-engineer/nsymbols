;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(defsystem "nsymbols"
  :description "A set of convenience functions to list class, variable, function, and other symbols."
  :author "Atlas Engineer LLC"
  :homepage "https://github.com/atlas-engineer/nsymbols"
  :license  "BSD-3 Clause"
  :version "0.3.0"
  :depends-on ()
  :serial t
  :components ((:file "package")
               (:file "nsymbols"))
  :in-order-to ((test-op (test-op "nsymbols/tests"))))

(defsystem "nsymbols/star"
  :description "Versions of regular `nsymbols' package operations, but with more intuitive results."
  :depends-on (#:nsymbols #:closer-mop)
  :components ((:file "star")))

(defsystem "nsymbols/tests"
  :depends-on (#:nsymbols #:lisp-unit2)
  :serial t
  :pathname "tests/"
  :components ((:file "package")
               (:file "tests"))
  :perform (test-op (o c)
                    (let* ((*debugger-hook* nil)
                           (test-results (symbol-call :lisp-unit2 :run-tests
                                                      :package :nsymbols/tests
                                                      :run-contexts (find-symbol "WITH-SUMMARY-CONTEXT" :lisp-unit2))))
                      (when (or
                             (uiop:symbol-call :lisp-unit2 :failed test-results)
                             (uiop:symbol-call :lisp-unit2 :errors test-results))
                        ;; Arbitrary but hopefully recognizable exit code.
                        (quit 18)))))
