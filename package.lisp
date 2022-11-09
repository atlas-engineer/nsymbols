;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

(uiop:define-package #:nsymbols
  (:use #:common-lisp)
  (:documentation "`nsymbols' exports functions and variables to filter symbols in packages.

There are functions directly listing symbols:
- `package-symbols' (the one all the other functions build upon).
- `package-variables'.
- `package-functions'.
- `package-generic-functions'.
- `package-macros'.
- `package-classes'.
- `package-structures'.

And there are respective CL types for those symbols to check symbol validity:
- `variable-symbol'.
- `function-symbol'.
- `generic-function-symbol'.
- `macro-symbol'.
- `class-symbol'.
- `structure-symbol'.

These types use the predicates to check symbol type:
- `variable-symbol-p'.
- `function-symbol-p'.
- `generic-function-symbol-p'.
- `macro-symbol-p'.
- `class-symbol-p'.
- `structure-symbol-p'.

Adding to the regular package-* listings, there is a \"nsymbols/star\"
ASDF system with:
- `package-functions*'.
- `package-generic-functions*'.
- `package-methods*'.
- `package-classes*'.
- `package-structures*'
which return the respective function, class, method etc. objects
instead of symbols for those.

As you can see, those are pretty regular. That's due to using
`define-symbol-type' that saves a lot of work in defining new types of
symbols. More so: it allows assigning a name to a symbol type, so that
you can, for example, refer to this type in `package-symbols' or
`resolve-symbol'.

;; Define an absurd type of symbols starting with A.
(nsymbols:define-symbol-type starts-with-a ()
 (char-equal #\a (elt (symbol-name nsymbols:%symbol%) 0)))

;; Use it
(nsymbols:package-starts-with-as :nsymbols)
;; => (NSYMBOLS::ALL-PACKAGES)

Now, `resolve-symbol' is a convenient symbol search function that
searches for a symbol by name in given packages, based on its type and
visibility. Which allows you to fine-tune the search to whatever set
of symbols you want:

;; Search any type of HELLO symbol in `*default-packages*':
(nsymbols:resolve-symbol :hello :any)

;; Search any type of * in CL:
(nsymbols:resolve-symbol :* :any :cl)

;; Search function named * in CL:
(nsymbols:resolve-symbol :* :function :cl)

;; Search a custom type defined earlier:
(nsymbols:resolve-symbol \"ALL-PACKAGES\" :starts-with-a :nsymbols)"))

(push :nsymbols *features*)
