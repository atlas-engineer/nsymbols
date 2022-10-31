;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3 Clause

;;; Commentary:
;;
;; GNU Guix development package.  To build and install, clone this repository,
;; switch directory to here and run:
;;
;;   guix package --install-from-file=guix.scm
;;
;; To use as the basis for a development environment, run:
;;
;;   guix shell --container -D -f build-scripts/guix.scm
;;
;; Replace --container by --pure if you still want ASDF to see external
;; libraries in ~/common-lisp, etc.
;; To build a local executable and then run it:
;;; Code:

(use-modules (guix packages)
             ((guix licenses) #:prefix license:)
             (guix git-download)
             (guix build-system asdf)
             (gnu packages)
             (gnu packages lisp)
             (gnu packages lisp-check)
             (gnu packages lisp-xyz))

(define-public sbcl-nsymbols
  (package
   (name "sbcl-nsymbols")
   (version "0.1.0")
   (source
    (local-file (dirname (current-filename)) #:recursive? #t)
    ;;;; Or this, in case of contributing to Guix.
    ;; (origin
    ;;   (method git-fetch)
    ;;   (uri (git-reference
    ;;         (url "https://github.com/atlas-engineer/nsymbols")
    ;;         (commit version)))
    ;;   (file-name (git-file-name "cl-nsymbols" version))
    ;;   (sha256
    ;;    (base32
    ;;     "SPECIFY-HASH")))
    )
   (build-system asdf-build-system/sbcl)
   ;; We use `cl-*' inputs and not `sbcl-*' ones so that CCL users can also use
   ;; this Guix manifests.
   ;;
   ;; Another reason is to not fail when an input dependency is found in
   ;; ~/common-lisp, which would trigger a rebuild of the SBCL input in the
   ;; store, which is read-only and would thus fail.
   ;;
   ;; The official Guix package should use `sbcl-*' inputs though.
   (native-inputs (list cl-lisp-unit2 sbcl))
   (synopsis
    "Functions to search, filter, and group symbols in chosen packages.")
   (home-page "https://github.com/atlas-engineer/nsymbols")
   (description "Nsymbols extends the regular package API of ANSI CL with more
operations, allowing one to list:

@itemize
@item @code{package-symbols}.
@item @code{package-variables}.
@item @code{package-functions}.
@item @code{package-generic-functions}.
@item @code{package-macros}.
@item @code{package-classes}.
@item @code{package-structures}.
@item And other symbol types, given code@{define-symbol-type} for those.
@end itemize

Nsymbols can also find symbols by their name/matching symbol with
@code{resolve-symbol}. All these operations are aware of symbol
visibility in the given packages, due to a @code{symbol-visibility}
function.")
   (license license:bsd-3)))

(define-public cl-nsymbols
  (sbcl-package->cl-source-package sbcl-nsymbols))

(define-public ecl-nsymbols
  (sbcl-package->ecl-package sbcl-nsymbols))

cl-nsymbols
