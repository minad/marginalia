;;; marginalia.el --- Enrich existing commands with completion annotations -*- lexical-binding: t -*-

;; Author: Omar Antolín Camarena, Daniel Mendler
;; Maintainer: Omar Antolín Camarena, Daniel Mendler
;; Created: 2020
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
;; Homepage: https://github.com/minad/marginalia

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Enrich existing commands with completion annotations

;;; Code:

(require 'subr-x)

;;;; Customization

(defgroup marginalia nil
  "Enrich existing commands with completion annotations."
  :group 'convenience
  :prefix "marginalia-")

(defface marginalia-key
  '((t :inherit font-lock-keyword-face :weight normal))
  "Face used to highlight keys in `marginalia-mode'."
  :group 'marginalia)

(defface marginalia-variable
  '((t :inherit marginalia-key))
  "Face used to highlight variable values in `marginalia-mode'."
  :group 'marginalia)

(defface marginalia-annotation
  '((t :inherit completions-annotations :weight normal))
  "Face used to highlight documentation string in `marginalia-mode'."
  :group 'marginalia)

(defcustom marginalia-annotation-width 80
  "Width of annotation string."
  :type 'integer
  :group 'marginalia)

(defcustom marginalia-annotate-alist
  '((command . marginalia-annotate-command-binding)
    (customize-group . marginalia-annotate-customize-group)
    (variable . marginalia-annotate-variable)
    (face . marginalia-annotate-face)
    (symbol . marginalia-annotate-symbol)
    (variable . marginalia-annotate-variable)
    (package . marginalia-annotate-package))
  "Associating categories with annotation functions for minibuffer completion.
The annotation function must return a string,
which is appended to the completion candidate.
Annotations are only shown if `marginalia-mode' is enabled."
  :type '(alist :key-type symbol :value-type function)
  :group 'marginalia)

(defcustom marginalia-command-category-alist
  '((execute-extended-command . command)
    (customize-face . face)
    (customize-face-other-window . face)
    (customize-group . customize-group)
    (customize-group-other-window . customize-group)
    (customize-option . variable)
    (customize-option-other-window . variable)
    (customize-set-variable . variable)
    (customize-variable . variable)
    (customize-variable-other-window . variable)
    (describe-function . symbol)
    (describe-variable . variable)
    (describe-face . face)
    (describe-symbol . symbol)
    (helpful-callable . symbol)
    (helpful-command . symbol)
    (helpful-function . symbol)
    (helpful-macro . symbol)
    (helpful-symbol . symbol)
    (helpful-variable . variable)
    (describe-package . package)
    (package-install . package)
    (package-delete . package)
    (package-reinstall . package))
  "Associate commands with a completion category."
  :type '(alist :key-type symbol :value-type symbol)
  :group 'marginalia)

;;;; Pre-declarations for external packages

(defvar package--builtins)
(defvar package-alist)
(defvar package-archive-contents)
(declare-function package-desc-summary "package")
(declare-function package--from-builtin "package")

;;;; Marginalia mode

(defvar marginalia--this-command nil
  "Last command symbol saved in order to allow annotations.")

(defun marginalia--truncate (str width)
  "Truncate string STR to WIDTH."
  (truncate-string-to-width (car (split-string str "\n")) width 0 32 "…"))

(defun marginalia-annotate-command-binding (cand)
  "Annotate command CAND with keybinding."
  ;; Taken from Emacs 28, read-extended-command--annotation
  (when-let* ((binding
               (with-current-buffer (window-buffer (minibuffer-selected-window))
                 (where-is-internal (intern cand) overriding-local-map t)))
              (desc (and (not (stringp binding)) (key-description binding))))
    (propertize (format " (%s)" desc) 'face 'marginalia-key)))

(defun marginalia-annotate-command-full (cand)
  "Annotate command CAND with the keybinding and its documentation string."
  (concat
   (marginalia-annotate-command-binding cand)
   (marginalia-annotate-symbol cand)))

(defun marginalia--annotation (ann)
  "Format annotation string ANN."
  (concat " "
          (propertize
           " "
           'display
           '(space :align-to (- right-fringe marginalia-annotation-width)))
          (propertize (marginalia--truncate ann marginalia-annotation-width)
                      'face 'marginalia-annotation)))

(defun marginalia-annotate-symbol (cand)
  "Annotate symbol CAND with its documentation string."
  (when-let (doc (let ((sym (intern cand)))
                   (cond
                    ((fboundp sym) (ignore-errors (documentation sym)))
                    ((facep sym) (documentation-property sym 'face-documentation))
                    (t (documentation-property sym 'variable-documentation)))))
    (marginalia--annotation doc)))

(defun marginalia-annotate-variable (cand)
  "Annotate variable CAND with its documentation string."
  (let ((sym (intern cand)))
    (when-let (doc (documentation-property sym 'variable-documentation))
      (concat " "
              (propertize
               " "
               'display
               '(space :align-to (- right-fringe marginalia-annotation-width 30)))
              (propertize (marginalia--truncate (format "%S" (if (boundp sym)
                                                              (symbol-value sym)
                                                            'unbound))
                                             40)
                          'face 'marginalia-variable)
              "    "
              (propertize (marginalia--truncate doc marginalia-annotation-width)
                          'face 'marginalia-annotation)))))

(defun marginalia-annotate-face (cand)
  "Annotate face CAND with documentation string and face example."
  (let ((sym (intern cand)))
    (when-let (doc (documentation-property sym 'face-documentation))
      (concat " "
              (propertize
               " "
               'display
               '(space :align-to (- right-fringe marginalia-annotation-width 30)))
              (propertize "abcdefghijklmNOPQRSTUVWXYZ" 'face sym)
              "    "
              (propertize (marginalia--truncate doc marginalia-annotation-width)
                          'face 'marginalia-annotation)))))

(defun marginalia-annotate-package (cand)
  "Annotate package CAND with its description summary."
  (when-let* ((pkg (intern (replace-regexp-in-string "-[[:digit:]\\.-]+$" "" cand)))
              ;; taken from embark.el, originally `describe-package-1`
              (desc (or (car (alist-get pkg package-alist))
                        (if-let ((built-in (assq pkg package--builtins)))
                            (package--from-builtin built-in)
                          (car (alist-get pkg package-archive-contents))))))
    (marginalia--annotation (package-desc-summary desc))))

(defun marginalia-annotate-customize-group (cand)
  "Annotate customization group CAND with its documentation string."
  (when-let (doc (documentation-property (intern cand) 'group-documentation))
    (marginalia--annotation doc)))

(defun marginalia--annotate-candidates (candidates)
  "Annotate CANDIDATES with richer information."
  (if-let* ((cat (marginalia--category-type))
            (annotate (alist-get cat marginalia-annotate-alist)))
      (mapcar (lambda (cand) (concat cand (funcall annotate cand))) candidates)
    candidates))

(defun marginalia--completion-metadata-get (fun metadata prop)
  "Advice for `completion-metadata-get'.
Replaces the category and annotation function.
FUN is the original function.
METADATA is the metadata.
PROP is the property which is looked up."
  ;; TODO add more category classifiers from Embark
  (pcase prop
    ('annotation-function
     (or (when-let (cat (marginalia--category-type))
           (alist-get cat marginalia-annotate-alist))
         (funcall fun metadata prop)))
    ('category
     (or (and marginalia--this-command
              (alist-get marginalia--this-command marginalia-command-category-alist))
         (funcall fun metadata prop)))
    (_ (funcall fun metadata prop))))

(defun marginalia--minibuffer-setup ()
  "Setup minibuffer for `marginalia-mode'.
Remember `this-command' for annotation."
  (setq-local marginalia--this-command this-command))

(defun marginalia--metadata ()
  "Return current minibuffer completion metadata."
  (completion-metadata
   (buffer-substring-no-properties (field-beginning) (point))
   minibuffer-completion-table
   minibuffer-completion-predicate))

(defun marginalia--category-type ()
  "Return minibuffer completion category per metadata."
  (completion-metadata-get (marginalia--metadata) 'category))

;;;###autoload
(define-minor-mode marginalia-mode
  "Annotate completion candidates with richer information."
  :global t

  ;; Reset first to get a clean slate.
  (advice-remove #'completion-metadata-get #'marginalia--completion-metadata-get)
  (remove-hook 'minibuffer-setup-hook #'marginalia--minibuffer-setup)

  ;; Now add our tweaks.
  (when marginalia-mode
    ;; Ensure that we remember this-command in order to select the annotation function.
    (add-hook 'minibuffer-setup-hook #'marginalia--minibuffer-setup)

    ;; Replace the metadata function.
    (advice-add #'completion-metadata-get :around #'marginalia--completion-metadata-get)))

;;;###autoload
(defun marginalia-set-command-annotation (cmd ann)
  "Modify marginalia configuration such that annotation function ANN is used for command CMD."
  (setq marginalia-command-category-alist
        (cons (cons cmd cmd)
              (assq-delete-all cmd marginalia-command-category-alist)))
  (setq marginalia-command-category-alist
        (cons (cons cmd ann)
              (assq-delete-all cmd marginalia-annotate-alist))))

(provide 'marginalia)
;;; marginalia.el ends here
