;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package #:nsymbols)

(define-condition multiple-resolved-symbols-error (error)
  ((designator :initarg :designator
               :accessor designator)
   (results :initarg :results
            :accessor results))
  (:report (lambda (condition stream)
             (format stream "Multiple ~a symbols found:"
                     (designator condition))
             (dolist (result (results condition))
               (terpri stream)
               (prin1 result stream)))))
(export 'multiple-resolved-symbols-error)

(deftype symbol-visibility ()
  `(member :internal :external :inherited :any))

(deftype string-designator ()
  `(or string symbol))

;; Basically a copy of `trivial-types:package-designator', but without characters.
(deftype package-designator ()
  `(or string-designator package))

(declaim (ftype (function (symbol) symbol-visibility)
                symbol-visibility))
(defun symbol-visibility (symbol)
  (nth-value 1 (find-symbol (symbol-name symbol) (symbol-package symbol))))
(export 'symbol-visibility)

(declaim (ftype (function (symbol-visibility list) list)
                filter-symbols))
(defun filter-symbols (visibility symbols)
  (if (eq visibility :any)
      symbols
      (remove-if-not (lambda (s) (eq visibility (symbol-visibility s)))
                     symbols)))
(export 'filter-symbols)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *symbol-types* (make-hash-table :test 'equalp)
    "The table from the name of the symbol type (as a symbol name string)
to the predicate of this type as a function symbol."))

(deftype symbol-type (type)
  (let ((type-name (string type)))
    (if (gethash type-name *symbol-types*)
        `(satisfies ,(gethash type-name *symbol-types*))
        (error "There's no such symbol type: ~a" type))))

(declaim
 (ftype (function ((or list package-designator)
                   &key (:visibility symbol-visibility)
                   (:type string-designator))
                  (values list &optional))
        package-symbols))
(defun package-symbols (packages &key (visibility :any) (type :any))
  "Return the list of all symbols from PACKAGES.
If VISIBILITY is specified, only include symbols with that visibility.
If TYPE is specified, only include symbols of that type.

PACKAGES can be a single package designator or a list of package designators.
VISIBILITY can be one of :ANY, :INTERNAL, :EXTERNAL, or :INHERITED."
  (let* ((packages (uiop:ensure-list packages))
         (symbols
           (delete-if-not
            (symbol-function (gethash (string type) *symbol-types*))
            (loop for package in (mapcar #'find-package packages)
                  append
                  (if (eq :external visibility)
                      (loop for s being the external-symbol in package
                            collect s)
                      (loop for s being the symbol in package
                            when (eq (symbol-package s) package)
                              collect s))))))
    (case visibility
      ((:any :external)
       symbols)
      ((:internal :inherited)
       (filter-symbols visibility symbols)))))
(export 'package-symbols)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar %symbol% nil
    "Special variable to bind symbols during type-checking to.")
  (export '%symbol%))

(defmacro define-symbol-type (name (&rest parents) &body predicate-body)
  "Define a new symbol type.

Generates several functions and exports them out of :nsymbols package
for convenience (%NAME% is substituted with NAME):
- %NAME%-SYMBOL-P -- a predicate checking the symbol for being of this type.
- %NAME%-SYMBOL -- a type to check the symbol.
- PACKAGE-%NAME%S -- a configurable package symbol listing.

In other words, it generates VARIABLE-SYMBOL-P, VARIABLE-SYMBOL, and
PACKAGE-VARIABLES for VARIABLE symbol type.

NAME is a symbol designator to reference this symbol type in
`resolve-symbol', or a list of two symbol designators:
- First symbol designator is the symbol type name.
- Second designator is its plural version, if atypical.

PREDICATE-BODY is the body of the boolean-returning function to find
the required symbols among the list of other symbols. You can refer to
the symbol being checked with a special variable %SYMBOL%."
  (let* ((proper-name (string (if (listp name)
                                  (first name)
                                  name)))
         (plural-name (if (listp name)
                          (string (second name))
                          (uiop:strcat proper-name "S")))
         (predicate-name (intern (uiop:strcat proper-name "-SYMBOL-P") :nsymbols))
         (type-name (intern (uiop:strcat proper-name "-SYMBOL") :nsymbols))
         (package-operation-name (intern (uiop:strcat "PACKAGE-" plural-name) :nsymbols)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defun ,predicate-name (%symbol%)
         (and (symbolp %symbol%)
              ,@predicate-body))
       (setf (gethash ,proper-name *symbol-types*)
             (quote ,predicate-name))
       (deftype ,type-name ()
         (quote (and ,@(loop for parent in parents
                             collect `(symbol-type ,parent))
                     (satisfies ,predicate-name))))
       (defun ,package-operation-name (packages &optional (visibility :any))
         (package-symbols packages :visibility visibility :type ,proper-name))
       (export (quote ,predicate-name) :nsymbols)
       (export (quote ,type-name) :nsymbols)
       (export (quote ,package-operation-name) :nsymbols))))
(export 'define-symbol-type)

;; Recognize :ANY/"ANY"/'ANY in `package-symbols' and `resolve-symbol'.
(setf (gethash "ANY" *symbol-types*) 'identity)

(define-symbol-type variable ()
  (boundp %symbol%))

(define-symbol-type function ()
  (fboundp %symbol%))

(define-symbol-type generic-function (function)
  (typep (ignore-errors (symbol-function %symbol%)) 'standard-generic-function))

(define-symbol-type macro (function)
  (macro-function %symbol%))

;; FIXME: make "class" an umbrella for structures + "actual classes"?
;; How do we call "actual classes" then?
(define-symbol-type (class classes) ()
  (and (find-class %symbol% nil)
       (typep (find-class %symbol%) 'standard-class)))

(define-symbol-type structure ()
  (and (find-class %symbol% nil)
       (typep (find-class %symbol%) 'structure-class)))

(defvar *default-packages* '(:cl :cl-user)
  "Package designator or a list of package designators for `resolve-symbol' to use by default.")
(export '*default-packages*)

(declaim
 (ftype (function (string-designator string-designator &optional (or package-designator (cons package-designator *)) boolean)
                  (values symbol list &optional))
        resolve-symbol))
(defun resolve-symbol (designator type &optional (packages *default-packages*) error-p)
  "Find the symbol (of symbol type TYPE) designated by DESIGNATOR in PACKAGES (and subpackages).
PACKAGES should be a package designator or a list thereof.
ERROR-P, when present and true, raises a continuable error of type
`multiple-resolved-symbols-error' if there is more than one symbol
found matching the DESIGNATOR."
  (let* ((packages (uiop:ensure-list packages))
         (designator (string designator))
         (symbols (package-symbols packages :type type)))
    (let* ((results (remove-if-not (lambda (sym) (string= designator (symbol-name sym)))
                                   symbols)))
      (cond
        ((and (> (length results) 1) error-p)
         (cerror "Proceed with the first matching symbol" 'multiple-resolved-symbols-error
                 :designator designator :symbols results)))
      (values (first results)
              results))))
(export 'resolve-symbol)
