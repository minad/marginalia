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
(eval-when-compile (require 'cl-lib))

;;;; Customization

(defgroup marginalia nil
  "Enrich existing commands with completion annotations."
  :group 'convenience
  :prefix "marginalia-")


(defcustom marginalia-truncate-width 80
  "Maximum truncation width of annotation fields.

This value is adjusted in the `minibuffer-setup-hook' depending on the `window-width'."
  :type 'integer)

(defcustom marginalia-annotators
  '(marginalia-annotators-light marginalia-annotators-heavy)
  "Choose an annotator association list for minibuffer completion.
The first entry in the list is used for annotations.
You can cycle between the annotators using `marginalia-cycle-annotators'.
Annotations are only shown if `marginalia-mode' is enabled.
An entry of nil disables marginalia's annotations (leaving you
only with the annotations that come with Emacs) without disabling
`marginalia-mode'; this can be convenient for users of
`marginalia-cycle-annotators'."
  :type '(repeat (choice (const :tag "Light" marginalia-annotators-light)
                         (const :tag "Heavy" marginalia-annotators-heavy)
                         (const :tag "None" nil)
                         (symbol :tag "Other"))))

(defcustom marginalia-annotators-light
  '((command . marginalia-annotate-binding)
    (customize-group . marginalia-annotate-customize-group)
    (variable . marginalia-annotate-variable)
    (face . marginalia-annotate-face)
    (minor-mode . marginalia-annotate-minor-mode)
    (symbol . marginalia-annotate-symbol)
    (variable . marginalia-annotate-variable)
    (environment-variable . marginalia-annotate-environment-variable)
    (input-method . marginalia-annotate-input-method)
    (coding-system . marginalia-annotate-coding-system)
    (charset . marginalia-annotate-charset)
    (package . marginalia-annotate-package)
    (imenu . marginalia-annotate-imenu)
    (virtual-buffer . marginalia-annotate-virtual-buffer-class))
  "Lightweight annotator functions.
Associates completion categories with annotation functions.
Each annotation function must return a string,
which is appended to the completion candidate.
See also `marginalia-annotators-heavy'."
  :type '(alist :key-type symbol :value-type function))

(defcustom marginalia-annotators-heavy
  (append
   '((file . marginalia-annotate-file)
     (buffer . marginalia-annotate-buffer)
     (virtual-buffer . marginalia-annotate-virtual-buffer-full)
     (command . marginalia-annotate-command))
   marginalia-annotators-light)
  "Heavy annotator functions.

Associates completion categories with annotation functions.
Each annotation function must return a string,
which is appended to the completion candidate.
See also `marginalia-annotators-light'."
  :type '(alist :key-type symbol :value-type function))

(defcustom marginalia-classifiers
  '(marginalia-classify-by-command-name
    marginalia-classify-original-category
    marginalia-classify-by-prompt
    marginalia-classify-symbol)
  "List of functions to determine current completion category.
Each function should take no arguments and return a symbol
indicating the category, or nil to indicate it could not
determine it."
  :type 'hook)

(defcustom marginalia-prompt-categories
  '(("\\<group\\>" . customize-group)
    ("\\<M-x\\>" . command)
    ("\\<package\\>" . package)
    ("\\<face\\>" . face)
    ("\\<environment variable\\>" . environment-variable)
    ("\\<variable\\>" . variable)
    ("\\<input method\\>" . input-method)
    ("\\<charset\\>" . charset)
    ("\\<coding system\\>" . coding-system)
    ("\\<minor mode\\>" . minor-mode))
  "Associates regexps to match against minibuffer prompts with categories."
  :type '(alist :key-type regexp :value-type symbol))

(defcustom marginalia-command-categories
  '((imenu . imenu))
  "Associate commands with a completion category."
  :type '(alist :key-type symbol :value-type symbol))

(defgroup marginalia-faces nil
  "Faces used by `marginalia-mode'."
  :group 'marginalia
  :group 'faces)

(defface marginalia-key
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight keys in `marginalia-mode'.")

(defface marginalia-lighter
  '((t :inherit marginalia-size))
  "Face used to highlight lighters in `marginalia-mode'.")

(defface marginalia-on
  '((t :inherit success))
  "Face used to signal enabled modes.")

(defface marginalia-off
  '((t :inherit error))
  "Face used to signal disabled modes.")

(defface marginalia-documentation
  '((t :inherit completions-annotations))
  "Face used to highlight documentation string in `marginalia-mode'.")

(defface marginalia-variable
  '((t :inherit marginalia-key))
  "Face used to highlight variable values in `marginalia-mode'.")

(defface marginalia-mode
  '((t :inherit marginalia-key))
  "Face used to highlight major modes in `marginalia-mode'.")

(defface marginalia-date
  '((t :inherit marginalia-key))
  "Face used to highlight dates in `marginalia-mode'.")

(defface marginalia-version
  '((t :inherit marginalia-size))
  "Face used to highlight package version in `marginalia-mode'.")

(defface marginalia-archive
  '((t :inherit warning))
  "Face used to highlight package archives in `marginalia-mode'.")

(defface marginalia-installed
  '((t :inherit success))
  "Face used to highlight package status in `marginalia-mode'.")

(defface marginalia-size
  '((t :inherit font-lock-constant-face))
  "Face used to highlight sizes in `marginalia-mode'.")

(defface marginalia-modified
  '((t :inherit font-lock-negation-char-face))
  "Face used to highlight modification indicators in `marginalia-mode'.")

(defface marginalia-file-name
  '((t :inherit marginalia-documentation))
  "Face used to highlight file names in `marginalia-mode'.")

(defface marginalia-file-modes
  '((t :inherit font-lock-string-face))
  "Face used to highlight file modes in `marginalia-mode'.")

(defface marginalia-file-owner
  '((t :inherit font-lock-preprocessor-face))
  "Face used to highlight file owners in `marginalia-mode'.")

;;;; Pre-declarations for external packages

(defvar package--builtins)
(defvar package-alist)
(defvar package-archive-contents)
(declare-function package--from-builtin "package")
(declare-function package-desc-archive "package")
(declare-function package-desc-dir "package")
(declare-function package-desc-summary "package")
(declare-function package-desc-version "package")
(declare-function package-installed-p "package")
(declare-function package-version-join "package")

;;;; Marginalia mode

(defvar marginalia--separator "    "
  "Field separator.")

(defvar marginalia--this-command nil
  "Last command symbol saved in order to allow annotations.")

(defvar marginalia--original-category nil
  "Original category reported by completion metadata.")

(defsubst marginalia--truncate (str width)
  "Truncate string STR to WIDTH."
  (truncate-string-to-width (car (split-string str "\n")) width 0 32 "…"))

(defsubst marginalia--align (str)
  "Align STR at the right margin."
  (concat " "
          (propertize
           " "
           'display
           `(space :align-to (- right-fringe ,(length str))))
          str))

(cl-defmacro marginalia--field (field &key truncate format face width)
  "Format FIELD as a string according to some options.

TRUNCATE is the truncation width.
FORMAT is a format string. This must be used if the field value is not a string.
FACE is the name of the face, with which the field should be propertized.
WIDTH is the format width. This can be specified as alternative to FORMAT."
  (cl-assert (not (and width format)))
  (when width (setq format (format "%%%ds" (- width))))
  (if format
      (setq field `(format ,format ,field))
    (setq field `(or ,field "")))
  (when truncate (setq field `(marginalia--truncate ,field ,truncate)))
  (when face (setq field `(propertize ,field 'face ,face)))
  field)

(defmacro marginalia--fields (&rest fields)
  "Format annotation FIELDS as a string with separators in between."
  `(marginalia--align (concat ,@(cdr (mapcan (lambda (field)
                                               (list 'marginalia--separator `(marginalia--field ,@field)))
                                             fields)))))

(defun marginalia--documentation (str)
  "Format documentation string STR."
  (when str
    (marginalia--fields
     (str :truncate marginalia-truncate-width :face 'marginalia-documentation))))

(defvar-local marginalia-annotate-binding--hash nil
  "Hash table storing the keybinding of every command.
This hash table is needed to speed up `marginalia-annotate-binding'.")

(defun marginalia-annotate-binding (cand)
  "Annotate command CAND with keybinding."
  (with-current-buffer (window-buffer (minibuffer-selected-window))
    ;; Precomputing the keybinding of every command is faster than looking it up every time using
    ;; `where-is-internal'. `where-is-internal' generates a lot of garbage, leading to garbage
    ;; collecting pauses when interacting with the minibuffer. See
    ;; https://github.com/minad/marginalia/issues/16.
    (unless marginalia-annotate-binding--hash
      (setq marginalia-annotate-binding--hash (make-hash-table))
      (mapatoms (lambda (sym)
                  (when-let (key (and (commandp sym) (where-is-internal sym nil t)))
                    (puthash sym key marginalia-annotate-binding--hash)))))
    (when-let* ((sym (intern-soft cand))
                (binding (gethash sym marginalia-annotate-binding--hash)))
      (propertize (format " (%s)" (key-description binding)) 'face 'marginalia-key))))

;; This annotator is consult-specific, it will annotate the `consult-buffer' command.
(defun marginalia-annotate-virtual-buffer-class (cand)
  "Annotate virtual-buffer CAND with the buffer class."
  (marginalia--fields
   ((pcase (elt cand 0)
      (?b "Buffer")
      (?f "File")
      (?m "Bookmark")
      (?v "View"))
    :width -8 :face 'marginalia-documentation)))

;; This annotator is consult-specific, it will annotate the `consult-buffer' command.
(defun marginalia-annotate-virtual-buffer-full (cand)
  "Annotate virtual-buffer CAND with the buffer class."
  (let ((cand-without-prefix (replace-regexp-in-string "^[^ ]+ " "" cand)))
    (pcase (elt cand 0)
      (?b (marginalia-annotate-buffer cand-without-prefix))
      (?f (marginalia-annotate-file cand-without-prefix))
      (_ (marginalia-annotate-virtual-buffer-class cand)))))

(defconst marginalia--advice-regexp
  (rx bos
      (1+ (seq (? "This function has ")
               (or ":before" ":after" ":around" ":override"
                   ":before-while" ":before-until" ":after-while"
                   ":after-until" ":filter-args" ":filter-return")
               " advice: " (0+ nonl) "\n"))
      "\n")
  "Regexp to match lines about advice in function documentation strings.")

;; Taken from advice--make-docstring, is this robust?
(defun marginalia--advised (fun)
  "Return t if function FUN is advised."
  (let ((flist (indirect-function fun)))
    (advice--p (if (eq 'macro (car-safe flist)) (cdr flist) flist))))

;; Symbol class characters from Emacs 28 `help--symbol-completion-table-affixation'
;; ! and * are our additions
(defun marginalia--symbol-class (s)
  "Return symbol class characters for symbol S.
f function
c command
! advised
m macro
u custom
v variable
* modified
a face"
  (format
   "%-6s"
   (concat
    (when (fboundp s)
      (concat
       (cond
        ((commandp s) "c")
        ((eq (car-safe (symbol-function s)) 'macro) "m")
        (t "f"))
       (when (marginalia--advised s) "!")))
    (when (boundp s)
      (concat
       (if (custom-variable-p s) "u" "v")
       (when (and (boundp s) (default-boundp s) (not (equal (symbol-value s) (default-value s)))) "*")))
    (when (facep s) "a")
    (when (and (fboundp 'cl-find-class) (cl-find-class s)) "t"))))

(defun marginalia--function-doc (sym)
  "Documentation string of function SYM."
  (when-let (doc (ignore-errors (documentation sym)))
    (replace-regexp-in-string marginalia--advice-regexp "" doc)))

(defun marginalia-annotate-symbol (cand)
  "Annotate symbol CAND with its documentation string."
  (when-let (sym (intern-soft cand))
    (concat
     (marginalia-annotate-binding cand)
     (marginalia--fields
      ((marginalia--symbol-class sym) :face 'marginalia-modified)
      ((cond
        ((fboundp sym) (marginalia--function-doc sym))
        ((facep sym) (documentation-property sym 'face-documentation))
        (t (documentation-property sym 'variable-documentation)))
       :truncate marginalia-truncate-width :face 'marginalia-documentation)))))

(defun marginalia-annotate-command (cand)
  "Annotate command CAND with its documentation string.
Similar to `marginalia-annotate-symbol', but does not show symbol class."
  (when-let (sym (intern-soft cand))
    (concat
     (marginalia-annotate-binding cand)
     (marginalia--documentation (marginalia--function-doc sym)))))

(defun marginalia-annotate-imenu (cand)
  "Annotate imenu CAND with its documentation string."
  (when (provided-mode-derived-p (buffer-local-value 'major-mode
                                                     (window-buffer (minibuffer-selected-window)))
                                 'emacs-lisp-mode)
    (marginalia-annotate-symbol (replace-regexp-in-string "^.* " "" cand))))

(defun marginalia-annotate-variable (cand)
  "Annotate variable CAND with its documentation string."
  (when-let (sym (intern-soft cand))
    (marginalia--fields
     ((marginalia--symbol-class sym) :face 'marginalia-modified)
     ((let ((print-escape-newlines t)
            (print-escape-control-characters t)
            (print-escape-multibyte t))
        (prin1-to-string (if (boundp sym) (symbol-value sym) 'unbound)))
      :truncate (/ marginalia-truncate-width 3) :face 'marginalia-variable)
     ((documentation-property sym 'variable-documentation)
      :truncate marginalia-truncate-width :face 'marginalia-documentation))))

(defun marginalia-annotate-environment-variable (cand)
  "Annotate environment variable CAND with its current value."
  (when-let (val (getenv cand))
    (marginalia--fields
     (val :truncate marginalia-truncate-width :face 'marginalia-variable))))

(defun marginalia-annotate-face (cand)
  "Annotate face CAND with documentation string and face example."
  (when-let (sym (intern-soft cand))
    (marginalia--fields
     ("abcdefghijklmNOPQRSTUVWXYZ" :face sym)
     ((documentation-property sym 'face-documentation)
      :truncate marginalia-truncate-width :face 'marginalia-documentation))))

(defun marginalia-annotate-minor-mode (cand)
  "Annotate minor-mode CAND with status and documentation string."
  (let* ((sym (intern-soft cand))
         (mode (if (and sym (boundp sym))
                   sym
                 (with-selected-window
                     (or (minibuffer-selected-window) (selected-window))
                   (lookup-minor-mode-from-indicator cand))))
         (lighter (cdr (assq mode minor-mode-alist)))
         (lighter-str (and lighter (string-trim (format-mode-line (cons t lighter))))))
    (concat
     (marginalia--fields
      ((if (and (boundp mode) (symbol-value mode))
           (propertize "On" 'face 'marginalia-on)
         (propertize "Off" 'face 'marginalia-off)) :width 3)
      (lighter-str :width 14 :face 'marginalia-lighter)
      ((marginalia--function-doc mode)
       :truncate marginalia-truncate-width :face 'marginalia-documentation)))))

(defun marginalia-annotate-package (cand)
  "Annotate package CAND with its description summary."
  (when-let* ((pkg (intern (replace-regexp-in-string "-[[:digit:]\\.-]+$" "" cand)))
              ;; taken from embark.el, originally `describe-package-1`
              (desc (or (car (alist-get pkg package-alist))
                        (if-let (built-in (assq pkg package--builtins))
                            (package--from-builtin built-in)
                          (car (alist-get pkg package-archive-contents))))))
    (marginalia--fields
     ((package-version-join (package-desc-version desc)) :width 16 :face 'marginalia-version)
     ((cond
       ((eq (package-desc-dir desc) 'builtin) (propertize "builtin" 'face 'marginalia-installed))
       ((package-installed-p desc) (propertize "installed" 'face 'marginalia-installed))
       (t (propertize (package-desc-archive desc) 'face 'marginalia-archive))) :width 9)
     ((package-desc-summary desc) :truncate marginalia-truncate-width :face 'marginalia-documentation))))

(defun marginalia-annotate-customize-group (cand)
  "Annotate customization group CAND with its documentation string."
  (marginalia--documentation (documentation-property (intern cand) 'group-documentation)))

(defun marginalia-annotate-input-method (cand)
  "Annotate input method CAND with its description."
  (marginalia--documentation (nth 4 (assoc cand input-method-alist))))

(defun marginalia-annotate-charset (cand)
  "Annotate charset CAND with its description."
  (marginalia--documentation (charset-description (intern cand))))

(defun marginalia-annotate-coding-system (cand)
  "Annotate coding system CAND with its description."
  (marginalia--documentation (coding-system-doc-string (intern cand))))

(defun marginalia-annotate-buffer (cand)
  "Annotate buffer CAND with modification status, file name and major mode."
  (when-let (buffer (get-buffer cand))
    (marginalia--fields
     ((concat
       (if (buffer-modified-p buffer) "*" " ")
       (if (buffer-local-value 'buffer-read-only buffer) "%" " "))
      :face 'marginalia-modified)
     ((buffer-local-value 'major-mode buffer) :width 30 :face 'marginalia-mode)
     ((when-let (file (buffer-file-name buffer))
        (abbreviate-file-name file))
      :truncate (/ marginalia-truncate-width 2)
      :face 'marginalia-file-name))))

;; At some point we might want to revisit how this function is implemented. Maybe we come up with a
;; more direct way to implement it. While Emacs does not use the notion of "full candidate", there
;; is a function `completion-boundaries' to compute them, and in (info "(elisp)Programmed
;; Completion") it is documented how a completion table should respond to boundaries requests.
;; See the discussion at https://github.com/minad/marginalia/commit/4ba98045dd33bcf1396a888dbbae2dc801dce7c5
(defun marginalia--full-candidate (cand)
  "Return completion candidate CAND in full.
For some completion tables, the completion candidates offered are
meant to be only a part of the full minibuffer contents. For
example, during file name completion the candidates are one path
component of a full file path.

This function returns what would be the minibuffer contents after
using `minibuffer-force-complete' on the candidate CAND."
  (if (minibufferp)
      (let* ((contents (minibuffer-contents))
             (pt (- (point) (minibuffer-prompt-end)))
             (bounds (completion-boundaries
                      (substring contents 0 pt)
                      minibuffer-completion-table
                      minibuffer-completion-predicate
                      (substring contents pt))))
        (concat (substring contents 0 (car bounds))
                cand
                (substring contents (+ pt (cdr bounds)))))
    ;; not in a minibuffer, trust that cand already conveys all
    ;; necessary information (there's not much else we can do)
    cand))

(defun marginalia-annotate-file (cand)
  "Annotate file CAND with its size and modification time."
  (when-let ((attributes (file-attributes (marginalia--full-candidate cand) 'string)))
    (marginalia--fields
     ((file-attribute-modes attributes) :face 'marginalia-file-modes)
     ((format "%s:%s"
              (file-attribute-user-id attributes)
              (file-attribute-group-id attributes))
      :width 12 :face 'marginalia-file-owner)
     ((file-size-human-readable (file-attribute-size attributes)) :width 7 :face 'marginalia-size)
     ((format-time-string
       "%b %d %H:%M"
       (file-attribute-modification-time attributes)) :face 'marginalia-date))))

(defun marginalia-classify-by-command-name ()
  "Lookup category for current command."
  (and marginalia--this-command
       (alist-get marginalia--this-command marginalia-command-categories)))

(defun marginalia-classify-original-category ()
  "Return original category reported by completion metadata."
  marginalia--original-category)

(defun marginalia-classify-symbol ()
  "Determine if currently completing symbols."
  (when-let (mct minibuffer-completion-table)
    (when (or (eq mct 'help--symbol-completion-table)
              (obarrayp mct)
              (and (consp mct) (symbolp (car mct)))) ; assume list of symbols
      'symbol)))

(defun marginalia-classify-by-prompt ()
  "Determine category by matching regexps against the minibuffer prompt.
This runs through the `marginalia-prompt-categories' alist
looking for a regexp that matches the prompt."
  (when-let (prompt (minibuffer-prompt))
    (setq prompt
          (replace-regexp-in-string "(.*default.*)\\|\\[.*\\]" "" prompt))
    (cl-loop for (regexp . category) in marginalia-prompt-categories
             when (string-match-p regexp prompt)
             return category)))

(defun marginalia--completion-metadata-get (metadata prop)
  "Meant as :before-until advice for `completion-metadata-get'.
METADATA is the metadata.
PROP is the property which is looked up."
  (pcase prop
    ('annotation-function
     ;; we do want the advice triggered for completion-metadata-get
     (when-let (cat (completion-metadata-get metadata 'category))
       (alist-get cat (symbol-value (car marginalia-annotators)))))
    ('category
     ;; using alist-get bypasses any advice on completion-metadata-get
     ;; to avoid infinite recursion
     (let ((marginalia--original-category (alist-get 'category metadata)))
       (run-hook-with-args-until-success 'marginalia-classifiers)))))

(defun marginalia--minibuffer-setup ()
  "Setup minibuffer for `marginalia-mode'.
Remember `this-command' for annotation."
  (let ((w (window-width)))
    (setq-local marginalia-truncate-width (min (/ w 2) marginalia-truncate-width))
    (setq-local marginalia--separator (if (> w 100) "    " " "))
    (setq-local marginalia--this-command this-command)))

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
    (advice-add #'completion-metadata-get :before-until #'marginalia--completion-metadata-get)))

;; If you want to cycle between annotators while being in the minibuffer, the completion-system
;; should refresh the candidate list. Currently there is no support for this in marginalia, but it
;; is possible to advise the `marginalia-cycle-annotators' function with the necessary refreshing
;; logic. See the discussion in https://github.com/minad/marginalia/issues/10 for reference.
;;;###autoload
(defun marginalia-cycle-annotators ()
  "Cycle between annotators in `marginalia-annotators'.
If called from the minibuffer the annotator cycling is local,
that it is, it does not affect subsequent minibuffers.  When called
from a regular buffer the effect is global."
  (interactive)
  (let ((annotators (append (cdr marginalia-annotators)
                            (list (car marginalia-annotators)))))
    ;; If `marginalia-cycle-annotators' has been invoked from inside the minibuffer, only change
    ;; the annotators locally. This is useful if the command is used as an action. If the command is
    ;; not triggered from inside the minibuffer, cycle the annotator globally. Hopefully this is
    ;; not too confusing.
    (if (minibufferp)
        (setq-local marginalia-annotators annotators)
      (setq marginalia-annotators annotators))))

(provide 'marginalia)
;;; marginalia.el ends here
