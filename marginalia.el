;;; marginalia.el --- Enrich existing commands with completion annotations -*- lexical-binding: t -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Omar Antolín Camarena <omar@matem.unam.mx>, Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Omar Antolín Camarena <omar@matem.unam.mx>, Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2020
;; Version: 0.8
;; Package-Requires: ((emacs "26.1"))
;; Homepage: https://github.com/minad/marginalia

;; This file is part of GNU Emacs.

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

(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib))

;;;; Customization

(defgroup marginalia nil
  "Enrich existing commands with completion annotations."
  :group 'convenience
  :group 'minibuffer
  :prefix "marginalia-")

(defcustom marginalia-truncate-width 80
  "Maximum truncation width of annotation fields.

This value is adjusted in the `minibuffer-setup-hook' depending
on the `window-width'."
  :type 'integer)

(defcustom marginalia-separator-threshold 120
  "Use wider separator for window widths larger than this value."
  :type 'integer)

;; See https://github.com/minad/marginalia/issues/42 for the discussion
;; regarding the alignment.
(defcustom marginalia-align-offset nil
  "Additional offset at the right margin used by `marginalia--align'.

This value should be set to nil to enable auto-configuration.
It can also be set to an integer value of 1 or larger to force an offset."
  :type '(choice (const nil) integer))

(defcustom marginalia-margin-min 8
  "Minimum whitespace margin at the right side."
  :type 'integer)

(defcustom marginalia-margin-threshold 200
  "Use whitespace margin for window widths larger than this value."
  :type 'integer)

(defcustom marginalia-max-relative-age (* 60 60 24 14)
  "Maximum relative age in seconds displayed by the file annotator.

Set to `most-positive-fixnum' to always use a relative age, or 0 to never show a relative age."
  :type 'integer)

(defcustom marginalia-annotator-registry
  (mapcar
   (lambda (x) (append x '(builtin none)))
   '((command marginalia-annotate-command marginalia-annotate-binding)
     (embark-keybinding marginalia-annotate-embark-keybinding)
     (customize-group marginalia-annotate-customize-group)
     (variable marginalia-annotate-variable)
     (function marginalia-annotate-function)
     (face marginalia-annotate-face)
     (color marginalia-annotate-color)
     (unicode-name marginalia-annotate-char)
     (minor-mode marginalia-annotate-minor-mode)
     (symbol marginalia-annotate-symbol)
     (environment-variable marginalia-annotate-environment-variable)
     (input-method marginalia-annotate-input-method)
     (coding-system marginalia-annotate-coding-system)
     (charset marginalia-annotate-charset)
     (package marginalia-annotate-package)
     (imenu marginalia-annotate-imenu)
     (bookmark marginalia-annotate-bookmark)
     (file marginalia-annotate-file)
     (project-file marginalia-annotate-project-file)
     (buffer marginalia-annotate-buffer)
     (consult-multi marginalia-annotate-consult-multi)))
  "Annotator function registry.
Associates completion categories with annotation functions.
Each annotation function must return a string,
which is appended to the completion candidate."
  :type '(alist :key-type symbol :value-type (repeat symbol)))

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
  '(("\\<customize group\\>" . customize-group)
    ("\\<M-x\\>" . command)
    ("\\<package\\>" . package)
    ("\\<bookmark\\>" . bookmark)
    ("\\<color\\>" . color)
    ("\\<face\\>" . face)
    ("\\<environment variable\\>" . environment-variable)
    ("\\<function\\>" . function)
    ("\\<variable\\>" . variable)
    ("\\<input method\\>" . input-method)
    ("\\<charset\\>" . charset)
    ("\\<coding system\\>" . coding-system)
    ("\\<minor mode\\>" . minor-mode)
    ("\\<[Ll]ibrary\\>" . library))
  "Associates regexps to match against minibuffer prompts with categories."
  :type '(alist :key-type regexp :value-type symbol))

(defcustom marginalia-censor-variables
  '("pass")
  "The values of variables matching any of these regular expressions is not shown."
  :type '(repeat (choice symbol regexp)))

(defcustom marginalia-command-categories
  '((imenu . imenu))
  "Associate commands with a completion category."
  :type '(alist :key-type symbol :value-type symbol))

(defcustom marginalia-bookmark-type-transformers
  `(("\\`bookmark-\\(.*?\\)-handler\\'" . "\\1")
    ("default" . "File")
    ("\\`\\(.*?\\)-+bookmark-jump\\(?:-handler\\)?\\'" . "\\1")
    (".*" . ,#'capitalize))
  "List of bookmark type transformers."
  :type '(alist :key-type regexp :value-type (choice string function)))

(defgroup marginalia-faces nil
  "Faces used by `marginalia-mode'."
  :group 'marginalia
  :group 'faces)

(defface marginalia-key
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight keys.")

(defface marginalia-type
  '((t :inherit marginalia-key))
  "Face used to highlight types.")

(defface marginalia-char
  '((t :inherit marginalia-key))
  "Face used to highlight character annotations.")

(defface marginalia-lighter
  '((t :inherit marginalia-size))
  "Face used to highlight minor mode lighters.")

(defface marginalia-on
  '((t :inherit success))
  "Face used to signal enabled modes.")

(defface marginalia-off
  '((t :inherit error))
  "Face used to signal disabled modes.")

(defface marginalia-documentation
  '((t :inherit completions-annotations))
  "Face used to highlight documentation strings.")

(defface marginalia-value
  '((t :inherit marginalia-key))
  "Face used to highlight general variable values.")

(defface marginalia-null
  '((t :inherit font-lock-comment-face))
  "Face used to highlight null or unbound variable values.")

(defface marginalia-true
  '((t :inherit font-lock-builtin-face))
  "Face used to highlight true variable values.")

(defface marginalia-function
  '((t :inherit font-lock-function-name-face))
  "Face used to highlight function symbols.")

(defface marginalia-symbol
  '((t :inherit font-lock-type-face))
  "Face used to highlight general symbols.")

(defface marginalia-list
  '((t :inherit font-lock-constant-face))
  "Face used to highlight list expressions.")

(defface marginalia-mode
  '((t :inherit marginalia-key))
  "Face used to highlight buffer major modes.")

(defface marginalia-date
  '((t :inherit marginalia-key))
  "Face used to highlight dates.")

(defface marginalia-version
  '((t :inherit marginalia-number))
  "Face used to highlight package versions.")

(defface marginalia-archive
  '((t :inherit warning))
  "Face used to highlight package archives.")

(defface marginalia-installed
  '((t :inherit success))
  "Face used to highlight the status of packages.")

(defface marginalia-size
  '((t :inherit marginalia-number))
  "Face used to highlight sizes.")

(defface marginalia-number
  '((t :inherit font-lock-constant-face))
  "Face used to highlight numeric values.")

(defface marginalia-string
  '((t :inherit font-lock-string-face))
  "Face used to highlight string values.")

(defface marginalia-modified
  '((t :inherit font-lock-negation-char-face))
  "Face used to highlight buffer modification indicators.")

(defface marginalia-file-name
  '((t :inherit marginalia-documentation))
  "Face used to highlight file names.")

(defface marginalia-file-owner
  '((t :inherit font-lock-preprocessor-face))
  "Face used to highlight file owner and group names.")

(defface marginalia-file-priv-no
  '((t :inherit shadow))
  "Face used to highlight the no file privilege attribute.")

(defface marginalia-file-priv-dir
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight the dir file privilege attribute.")

(defface marginalia-file-priv-link
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight the link file privilege attribute.")

(defface marginalia-file-priv-read
  '((t :inherit font-lock-type-face))
  "Face used to highlight the read file privilege attribute.")

(defface marginalia-file-priv-write
  '((t :inherit font-lock-builtin-face))
  "Face used to highlight the write file privilege attribute.")

(defface marginalia-file-priv-exec
  '((t :inherit font-lock-function-name-face))
  "Face used to highlight the exec file privilege attribute.")

(defface marginalia-file-priv-other
  '((t :inherit font-lock-constant-face))
  "Face used to highlight some other file privilege attribute.")

(defface marginalia-file-priv-rare
  '((t :inherit font-lock-variable-name-face))
  "Face used to highlight a rare file privilege attribute.")

;;;; Pre-declarations for external packages

(defvar bookmark-alist)
(declare-function bookmark-get-handler "bookmark")
(declare-function bookmark-get-filename "bookmark")
(declare-function bookmark-get-front-context-string "bookmark")

(defvar package--builtins)
(defvar package-archive-contents)
(declare-function package--from-builtin "package")
(declare-function package-desc-archive "package")
(declare-function package-desc-status "package")
(declare-function package-desc-summary "package")
(declare-function package-desc-version "package")
(declare-function package-version-join "package")
(declare-function project-current "project")

(declare-function color-rgb-to-hex "color")
(declare-function color-rgb-to-hsl "color")
(declare-function color-hsl-to-rgb "color")

(declare-function selectrum--get-full "ext:selectrum")

;;;; Marginalia mode

(defvar marginalia--fontified-file-modes nil
  "List of fontified file modes.")

(defvar-local marginalia--cache nil
  "The cache, pair of list and hashtable.")

(defvar marginalia--cache-size 100
  "Size of the cache, set to 0 to disable the cache.
Disabling the cache is useful on non-incremental UIs like default completion or
for performance profiling of the annotators.")

(defvar marginalia--separator "    "
  "Field separator.")

(defvar marginalia--margin nil
  "Right margin.")

(defvar-local marginalia--this-command nil
  "Last command symbol saved in order to allow annotations.")

(defvar-local marginalia--base-position 0
  "Last completion base position saved to get full file paths.")

(defvar marginalia--metadata nil
  "Completion metadata from the current completion.")

(defun marginalia--truncate (str width)
  "Truncate string STR to WIDTH."
  (truncate-string-to-width
   (if-let (pos (string-match-p "\n" str))
       (substring str 0 pos)
     str)
   width 0 32 t))

(defun marginalia--align (str)
  "Align STR at the right margin."
  (unless (string-blank-p str)
    (when marginalia--margin
      (setq str (concat str marginalia--margin)))
    (concat " "
            (propertize
             " "
             'display
             `(space :align-to (- right ,marginalia-align-offset ,(string-width str))))
            str)))

(cl-defmacro marginalia--field (field &key truncate format face width)
  "Format FIELD as a string according to some options.

TRUNCATE is the truncation width.
FORMAT is a format string. This must be used if the field value is not a string.
FACE is the name of the face, with which the field should be propertized.
WIDTH is the format width. This can be specified as alternative to FORMAT."
  (cl-assert (not (and width format)))
  (when width
    (setq field `(or ,field "")
          format (format "%%%ds" (- width))))
  (setq field (if format
                  `(format ,format ,field)
                `(or ,field "")))
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

(defun marginalia-annotate-binding (cand)
  "Annotate command CAND with keybinding."
  (when-let* ((sym (intern-soft cand))
              (key (and (commandp sym) (where-is-internal sym nil 'first-only))))
    (propertize (format " (%s)" (key-description key)) 'face 'marginalia-key)))

(defun marginalia--annotator (cat)
  "Return annotation function for category CAT."
  (pcase (car (alist-get cat marginalia-annotator-registry))
    ('none (lambda (_) nil))
    ('builtin nil)
    (fun fun)))

;; This annotator is consult-specific, it will annotate commands with `consult-multi' category
(defun marginalia-annotate-consult-multi (cand)
  "Annotate consult-multi CAND with the buffer class."
  (if-let* ((multi (get-text-property 0 'consult-multi cand))
            (annotate (marginalia--annotator (car multi))))
      ;; Use the Marginalia annotator corresponding to the consult-multi category.
      (funcall annotate (cdr multi))
    ;; Apply the original annotation function on the original candidate, if there is one.
    ;; NOTE: Use `alist-get' instead of `completion-metadata-get' to bypass our
    ;; `marginalia--completion-metadata-get' advice!
    (when-let (annotate (alist-get 'annotation-function marginalia--metadata))
      (funcall annotate cand))))

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

Function:
f function
c command
C interactive-only command
m macro
p pure
s side-effect-free
@ autoloaded
! advised
- obsolete

Variable:
u custom
v variable
l local
* modified
- obsolete

Other:
a face
t cl-type"
  (format
   "%-6s"
   (concat
    (when (fboundp s)
      (concat
       (cond
        ((get s 'pure) "p")
        ((get s 'side-effect-free) "s"))
       (cond
        ((commandp s) (if (get s 'interactive-only) "C" "c"))
        ((eq (car-safe (symbol-function s)) 'macro) "m")
        (t "f"))
       (and (autoloadp (symbol-function s)) "@")
       (and (marginalia--advised s) "!")
       (and (get s 'byte-obsolete-info) "-")))
    (when (boundp s)
      (concat
       (and (local-variable-if-set-p s) "l")
       (if (custom-variable-p s) "u" "v")
       (and (ignore-errors (not (equal (symbol-value s) (default-value s)))) "*")
       (and (get s 'byte-obsolete-variable) "-")))
    (and (facep s) "a")
    (and (fboundp 'cl-find-class) (cl-find-class s) "t"))))

(defun marginalia--function-doc (sym)
  "Documentation string of function SYM."
  (when-let (str (ignore-errors (documentation sym)))
    (save-match-data
      (if (string-match marginalia--advice-regexp str)
          (substring str (match-end 0))
        str))))

;; Derived from elisp-get-fnsym-args-string
(defun marginalia--function-args (sym)
  "Return function arguments for SYM."
  (let ((tmp))
    (elisp-function-argstring
      (cond
       ((listp (setq tmp (gethash (indirect-function sym)
                                  advertised-signature-table t)))
        tmp)
       ((setq tmp (help-split-fundoc
		   (ignore-errors (documentation sym t))
		   sym))
	(substitute-command-keys (car tmp)))
       (t (help-function-arglist sym))))))

(defun marginalia-annotate-symbol (cand)
  "Annotate symbol CAND with its documentation string."
  (when-let (sym (intern-soft cand))
    (concat
     (marginalia-annotate-binding cand)
     (marginalia--fields
      ((marginalia--symbol-class sym) :face 'marginalia-type)
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

(defun marginalia-annotate-embark-keybinding (cand)
  "Annotate Embark keybinding CAND with its documentation string.
Similar to `marginalia-annotate-command', but does not show the
keybinding since CAND includes it."
  (when-let (cmd (get-text-property 0 'embark-command cand))
    (marginalia--documentation (marginalia--function-doc cmd))))

(defun marginalia-annotate-imenu (cand)
  "Annotate imenu CAND with its documentation string."
  (when (derived-mode-p 'emacs-lisp-mode)
    ;; Strip until the last whitespace in order to support flat imenu
    (marginalia-annotate-symbol (replace-regexp-in-string "^.* " "" cand))))

(defun marginalia-annotate-function (cand)
  "Annotate function CAND with its documentation string."
  (when-let (sym (intern-soft cand))
    (when (functionp sym)
      (concat
       (marginalia-annotate-binding cand)
       (marginalia--fields
        ((marginalia--symbol-class sym) :face 'marginalia-type)
        ((marginalia--function-args sym) :face 'marginalia-value
         :truncate (/ marginalia-truncate-width 2))
        ((marginalia--function-doc sym) :truncate marginalia-truncate-width
         :face 'marginalia-documentation))))))

(defun marginalia--variable-value (sym)
  "Return the variable value of SYM as string."
  (cond
   ((not (boundp sym))
    (propertize "<unbound>" 'face 'marginalia-null))
   ((and marginalia-censor-variables
         (let ((name (symbol-name sym)))
           (seq-find (lambda (r)
                       (if (symbolp r)
                           (eq r sym)
                         (string-match-p r name)))
                     marginalia-censor-variables)))
    (propertize "*****" 'face 'marginalia-null))
   (t (let ((val (symbol-value sym)))
        (pcase (symbol-value sym)
          ('nil (propertize "nil" 'face 'marginalia-null))
          ('t (propertize "t" 'face 'marginalia-true))
          ((pred keymapp) (propertize "<keymap>" 'face 'marginalia-value))
          ((pred hash-table-p) (propertize "<hash-table>" 'face 'marginalia-value))
          ((and (pred functionp) (pred symbolp))
           ;; NOTE: We are not consistent here, values are generally printed unquoted. But we
           ;; make an exception for function symbols to visually distinguish them from symbols.
           ;; I am not entirely happy with this, but we should not add quotation to every type.
           (propertize (format "#'%s" val) 'face 'marginalia-function))
          ((pred symbolp) (propertize (symbol-name val) 'face 'marginalia-symbol))
          ((pred numberp) (propertize (number-to-string val) 'face 'marginalia-number))
          (_ (let ((print-escape-newlines t)
                   (print-escape-control-characters t)
                   (print-escape-multibyte t)
                   (print-level 10)
                   (print-length marginalia-truncate-width))
               (propertize
                (prin1-to-string
                 (if (stringp val)
                     ;; Get rid of string properties to save some of the precious space
                     (substring-no-properties
                      val 0
                      (min (length val) marginalia-truncate-width))
                   val))
                'face
                (cond
                 ((listp val) 'marginalia-list)
                 ((stringp val) 'marginalia-string)
                 (t 'marginalia-value))))))))))

(defun marginalia-annotate-variable (cand)
  "Annotate variable CAND with its documentation string."
  (when-let (sym (intern-soft cand))
    (marginalia--fields
     ((marginalia--symbol-class sym) :face 'marginalia-type)
     ((marginalia--variable-value sym) :truncate (/ marginalia-truncate-width 2))
     ((documentation-property sym 'variable-documentation)
      :truncate marginalia-truncate-width :face 'marginalia-documentation))))

(defun marginalia-annotate-environment-variable (cand)
  "Annotate environment variable CAND with its current value."
  (when-let (val (getenv cand))
    (marginalia--fields
     (val :truncate marginalia-truncate-width :face 'marginalia-value))))

(defun marginalia-annotate-face (cand)
  "Annotate face CAND with its documentation string and face example."
  (when-let (sym (intern-soft cand))
    (marginalia--fields
     ("abcdefghijklmNOPQRSTUVWXYZ" :face sym)
     ((documentation-property sym 'face-documentation)
      :truncate marginalia-truncate-width :face 'marginalia-documentation))))

(defun marginalia-annotate-color (cand)
  "Annotate face CAND with its documentation string and face example."
  (when-let (rgb (color-name-to-rgb cand))
    (pcase-let* ((`(,r ,g ,b) rgb)
                 (`(,h ,s ,l) (apply #'color-rgb-to-hsl rgb))
                 (cr (color-rgb-to-hex r 0 0))
                 (cg (color-rgb-to-hex 0 g 0))
                 (cb (color-rgb-to-hex 0 0 b))
                 (ch (apply #'color-rgb-to-hex (color-hsl-to-rgb h 1 0.5)))
                 (cs (apply #'color-rgb-to-hex (color-hsl-to-rgb h s 0.5)))
                 (cl (apply #'color-rgb-to-hex (color-hsl-to-rgb 0 0 l))))
      (marginalia--fields
       ("      " :face `(:background ,(apply #'color-rgb-to-hex rgb)))
       ((format "%s%s%s %s"
                (propertize "r" 'face `(:background ,cr :foreground ,(readable-foreground-color cr)))
                (propertize "g" 'face `(:background ,cg :foreground ,(readable-foreground-color cg)))
                (propertize "b" 'face `(:background ,cb :foreground ,(readable-foreground-color cb)))
                (color-rgb-to-hex r g b 2)))
       ((format "%s%s%s %3s° %3s%% %3s%%"
                (propertize "h" 'face `(:background ,ch :foreground ,(readable-foreground-color ch)))
                (propertize "s" 'face `(:background ,cs :foreground ,(readable-foreground-color cs)))
                (propertize "l" 'face `(:background ,cl :foreground ,(readable-foreground-color cl)))
                (round (* 360 h))
                (round (* 100 s))
                (round (* 100 l))))))))

(defun marginalia-annotate-char (cand)
  "Annotate character CAND with its general character category and character code."
  (when-let (char (char-from-name cand t))
    (concat
     (propertize (format " (%c)" char) 'face 'marginalia-char)
     (marginalia--fields
      (char :format "%06X" :face 'marginalia-number)
      ((char-code-property-description
        'general-category
        (get-char-code-property char 'general-category))
       :width 30 :face 'marginalia-documentation)))))

(defun marginalia-annotate-minor-mode (cand)
  "Annotate minor-mode CAND with status and documentation string."
  (let* ((sym (intern-soft cand))
         (mode (if (and sym (boundp sym))
                   sym
                 (lookup-minor-mode-from-indicator cand)))
         (lighter (cdr (assq mode minor-mode-alist)))
         (lighter-str (and lighter (string-trim (format-mode-line (cons t lighter))))))
    (concat
     (marginalia--fields
      ((if (and (boundp mode) (symbol-value mode))
           (propertize "On" 'face 'marginalia-on)
         (propertize "Off" 'face 'marginalia-off)) :width 3)
      ((if (local-variable-if-set-p mode) "Local" "Global") :width 6 :face 'marginalia-type)
      (lighter-str :width 20 :face 'marginalia-lighter)
      ((marginalia--function-doc mode)
       :truncate marginalia-truncate-width :face 'marginalia-documentation)))))

(defun marginalia-annotate-package (cand)
  "Annotate package CAND with its description summary."
  (when-let* ((pkg-alist (and (bound-and-true-p package-alist) package-alist))
              (pkg (intern-soft (replace-regexp-in-string "-[[:digit:]\\.-]+\\'" "" cand)))
              ;; taken from `describe-package-1'
              (desc (or (car (alist-get pkg pkg-alist))
                        (if-let (built-in (assq pkg package--builtins))
                            (package--from-builtin built-in)
                          (car (alist-get pkg package-archive-contents))))))
    (marginalia--fields
     ((package-version-join (package-desc-version desc)) :width 16 :face 'marginalia-version)
     ((cond
       ((package-desc-archive desc) (propertize (package-desc-archive desc) 'face 'marginalia-archive))
       (t (propertize (or (package-desc-status desc) "orphan") 'face 'marginalia-installed))) :width 10)
     ((package-desc-summary desc) :truncate marginalia-truncate-width :face 'marginalia-documentation))))

(defun marginalia--bookmark-type (bm)
  "Return bookmark type string of BM.

The string is transformed according to `marginalia-bookmark-type-transformers'."
  (let ((handler (or (bookmark-get-handler bm) 'bookmark-default-handler)))
    ;; Some libraries use lambda handlers instead of symbols. For
    ;; example the function `xwidget-webkit-bookmark-make-record' is
    ;; affected. I consider this bad style since then the lambda is
    ;; persisted.
    (when-let (str (and (symbolp handler) (symbol-name handler)))
      (dolist (transformer marginalia-bookmark-type-transformers str)
        (when (string-match-p (car transformer) str)
          (setq str
                (if (stringp (cdr transformer))
                    (replace-regexp-in-string (car transformer) (cdr transformer) str)
                  (funcall (cdr transformer) str))))))))

(defun marginalia-annotate-bookmark (cand)
  "Annotate bookmark CAND with its file name and front context string."
  (when-let ((bm (assoc cand bookmark-alist)))
    (let ((front (bookmark-get-front-context-string bm)))
      (marginalia--fields
       ((marginalia--bookmark-type bm) :width 10 :face 'marginalia-type)
       ((bookmark-get-filename bm) :truncate 40 :face 'marginalia-file-name)
       ((if (or (not front) (string= front ""))
            ""
          (concat (string-trim
                   (replace-regexp-in-string
                    "[ \t]+" " "
                    (replace-regexp-in-string "\n" "\\\\n" front))) "…"))
        :truncate 20 :face 'marginalia-documentation)))))

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

(defun marginalia--buffer-status (buffer)
  "Return the status of BUFFER as a string."
  (format-mode-line '((:propertize "%1*%1+%1@" face marginalia-modified)
                      marginalia--separator
                      (7 (:propertize "%I" face marginalia-size))
                      marginalia--separator
                      ;; InactiveMinibuffer has 18 letters, but there are longer names.
                      ;; For example Org-Agenda produces very long mode names.
                      ;; Therefore we have to truncate.
                      (20 (-20 (:propertize mode-name face marginalia-mode))))
                    nil nil buffer))

(defun marginalia--buffer-file (buffer)
  "Return the file or process name of BUFFER."
  (if-let (proc (get-buffer-process buffer))
          (format "(%s %s) %s"
                  proc (process-status proc)
                  (abbreviate-file-name (buffer-local-value 'default-directory buffer)))
        (abbreviate-file-name
         (or (cond
              ;; see ibuffer-buffer-file-name
              ((buffer-file-name buffer))
              ((when-let (dir (and (local-variable-p 'dired-directory buffer)
                                   (buffer-local-value 'dired-directory buffer)))
                 (expand-file-name (if (stringp dir) dir (car dir))
                                   (buffer-local-value 'default-directory buffer))))
              ((local-variable-p 'list-buffers-directory buffer)
               (buffer-local-value 'list-buffers-directory buffer)))
             ""))))

(defun marginalia-annotate-buffer (cand)
  "Annotate buffer CAND with modification status, file name and major mode."
  (when-let (buffer (get-buffer cand))
    (marginalia--fields
     ((marginalia--buffer-status buffer))
     ((marginalia--buffer-file buffer)
      :truncate (/ marginalia-truncate-width 2)
      :face 'marginalia-file-name))))

(defun marginalia--full-candidate (cand)
  "Return completion candidate CAND in full.
For some completion tables, the completion candidates offered are
meant to be only a part of the full minibuffer contents. For
example, during file name completion the candidates are one path
component of a full file path."
    (if-let (win (active-minibuffer-window))
        (with-current-buffer (window-buffer win)
          (if (bound-and-true-p selectrum-is-active)
              (selectrum--get-full cand)
            (concat (substring (minibuffer-contents-no-properties)
                               0 marginalia--base-position)
                    cand)))
      ;; no minibuffer is active, trust that cand already conveys all
      ;; necessary information (there's not much else we can do)
      cand))

(defun marginalia--remote-protocol (path)
  "Return the remote protocol of PATH."
  (save-match-data
    (setq path (substitute-in-file-name path))
    (and (string-match "\\`/\\([^/|:]+\\):" path)
         (match-string 1 path))))

(defun marginalia--annotate-local-file (cand)
  "Annotate local file CAND."
  (when-let (attrs (ignore-errors
                     ;; may throw permission denied errors
                     (file-attributes (substitute-in-file-name
                                       (marginalia--full-candidate cand))
                                      'integer)))
    (marginalia--fields
     ((marginalia--file-owner attrs)
      :width 12 :face 'marginalia-file-owner)
     ((marginalia--file-modes attrs))
     ((file-size-human-readable (file-attribute-size attrs))
      :face 'marginalia-size :width -7)
     ((marginalia--time (file-attribute-modification-time attrs))
      :face 'marginalia-date :width -12))))

(defun marginalia-annotate-file (cand)
  "Annotate file CAND with its size, modification time and other attributes.
These annotations are skipped for remote paths."
  (if-let (remote (or (marginalia--remote-protocol cand)
                      (when-let (win (active-minibuffer-window))
                        (with-current-buffer (window-buffer win)
                          (marginalia--remote-protocol (minibuffer-contents-no-properties))))))
      (marginalia--fields (remote :format "*%s*" :face 'marginalia-documentation))
    (marginalia--annotate-local-file cand)))

(defun marginalia--file-owner (attrs)
  "Return file owner given ATTRS."
  (let ((uid (file-attribute-user-id attrs))
        (gid (file-attribute-group-id attrs)))
    (if (or (/= (user-uid) uid) (/= (group-gid) gid))
        (format "%s:%s" (or (user-login-name uid) uid) (or (group-name gid) gid))
      "")))

(defun marginalia--file-modes (attrs)
  "Return fontified file modes given the ATTRS."
  ;; Without caching this can a be significant portion of the time
  ;; `marginalia-annotate-file' takes to execute. Caching improves performance
  ;; by about a factor of 20.
  (setq attrs (file-attribute-modes attrs))
  (or (car (member attrs marginalia--fontified-file-modes))
      (progn
        (setq attrs (substring attrs)) ;; copy because attrs is about to be modified
        (dotimes (i (length attrs))
          (put-text-property
           i (1+ i) 'face
           (pcase (aref attrs i)
             (?- 'marginalia-file-priv-no)
             (?d 'marginalia-file-priv-dir)
             (?l 'marginalia-file-priv-link)
             (?r 'marginalia-file-priv-read)
             (?w 'marginalia-file-priv-write)
             (?x 'marginalia-file-priv-exec)
             ((or ?s ?S ?t ?T) 'marginalia-file-priv-other)
             (_ 'marginalia-file-priv-rare))
           attrs))
        (push attrs marginalia--fontified-file-modes)
        attrs)))

(defconst marginalia--time-relative
  `((100 "sec" 1)
    (,(* 60 100) "min" 60.0)
    (,(* 3600 30) "hour" 3600.0)
    (,(* 3600 24 400) "day" ,(* 3600.0 24.0))
    (nil "year" ,(* 365.25 24 3600)))
  "Formatting used by the function `marginalia--time-relative'.")

;; Taken from `seconds-to-string'.
(defun marginalia--time-relative (time)
  "Format TIME as a relative age."
  (setq time (float-time (time-since time)))
  (if (<= time 0)
      "0 secs ago"
    (let ((sts marginalia--time-relative) here)
      (while (and (car (setq here (pop sts))) (<= (car here) time)))
      (setq time (round time (caddr here)))
      (format "%s %s%s ago" time (cadr here) (if (= time 1) "" "s")))))

(defun marginalia--time-absolute (time)
  "Format TIME as an absolute age."
  (let ((system-time-locale "C"))
    (format-time-string
     (if (> (decoded-time-year (decode-time (current-time)))
            (decoded-time-year (decode-time time)))
         " %Y %b %d"
       "%b %d %H:%M")
     time)))

(defun marginalia--time (time)
  "Format file age TIME, suitably for use in annotations."
  (if (< (float-time (time-since time)) marginalia-max-relative-age)
      (marginalia--time-relative time)
    (marginalia--time-absolute time)))

(defmacro marginalia--project-root ()
  "Return project root."
  (require 'project)
  `(when-let (proj (project-current))
     ,(if (fboundp 'project-root)
          '(project-root proj)
        '(car (project-roots proj)))))

(defun marginalia-annotate-project-file (cand)
  "Annotate file CAND with its size, modification time and other attributes."
  ;; TODO project-find-file can be called from outside all projects in
  ;; which case it prompts for a project first; we don't support that
  ;; case yet, since there is no current project.
  (when-let (root (marginalia--project-root))
    (marginalia-annotate-file (expand-file-name cand root))))

(defun marginalia-classify-by-command-name ()
  "Lookup category for current command."
  (and marginalia--this-command
       (alist-get marginalia--this-command marginalia-command-categories)))

(defun marginalia-classify-original-category ()
  "Return original category reported by completion metadata."
  ;; NOTE: Use `alist-get' instead of `completion-metadata-get' to bypass our
  ;; `marginalia--completion-metadata-get' advice!
  (alist-get 'category marginalia--metadata))

(defun marginalia-classify-symbol ()
  "Determine if currently completing symbols."
  (when-let (mct minibuffer-completion-table)
    (when (or (eq mct 'help--symbol-completion-table)
              (obarrayp mct)
              (and (not (functionp mct)) (consp mct) (symbolp (car mct)))) ; assume list of symbols
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

(defmacro marginalia--context (metadata &rest body)
  "Setup annotator context with completion METADATA around BODY."
  (declare (indent 1))
  (let ((w (make-symbol "w"))
        (c (make-symbol "c"))
        (o (make-symbol "o")))
    ;; Take the window width of the current window (minibuffer window!)
    `(let ((marginalia--metadata ,metadata)
           (,c marginalia--cache)
           (,w (window-width))
           ;; Compute marginalia-align-offset. If the right-fringe-width is
           ;; zero, use an additional offset of 1 by default! See
           ;; https://github.com/minad/marginalia/issues/42 for the discussion
           ;; regarding the alignment.
           (,o (if (eq 0 (nth 1 (window-fringes))) 1 0)))
       ;; We generally run the annotators in the original window.
       ;; `with-selected-window' is necessary because of `lookup-minor-mode-from-indicator'.
       ;; Otherwise it would probably suffice to only change the current buffer.
       ;; We need the `selected-window' fallback for Embark Occur.
       (with-selected-window (or (minibuffer-selected-window) (selected-window))
         (let ((marginalia--cache ,c) ;; Take the cache from the minibuffer
               (marginalia-truncate-width (min (/ ,w 2) marginalia-truncate-width))
               (marginalia-align-offset (or marginalia-align-offset ,o))
               (marginalia--separator (if (>= ,w marginalia-separator-threshold) "    " " "))
               (marginalia--margin (when (>= ,w (+ marginalia-margin-min marginalia-margin-threshold))
                                     (make-string (- ,w marginalia-margin-threshold) 32))))
           ,@body)))))

(defun marginalia--cache-reset ()
  "Reset the cache."
  (when marginalia--cache
    (setq marginalia--cache (and (> marginalia--cache-size 0)
                                 (cons nil (make-hash-table :test #'equal
                                                            :size marginalia--cache-size))))))

(defun marginalia--cached (fun key)
  "Cached application of function FUN with KEY.

The cache keeps around the last `marginalia--cache-size' computed annotations.
The cache is mainly useful when scrolling in completion UIs like Vertico or
Selectrum."
  (if marginalia--cache
      (let ((ht (cdr marginalia--cache)))
        (or (gethash key ht)
            (let ((val (funcall fun key)))
              (setcar marginalia--cache (cons key (car marginalia--cache)))
              (puthash key val ht)
              (when (>= (hash-table-count ht) marginalia--cache-size)
                (let ((end (last (car marginalia--cache) 2)))
                  (remhash (cadr end) ht)
                  (setcdr end nil)))
              val)))
    (funcall fun key)))

(defun marginalia--completion-metadata-get (metadata prop)
  "Meant as :before-until advice for `completion-metadata-get'.
METADATA is the metadata.
PROP is the property which is looked up."
  (pcase prop
    ('annotation-function
     ;; we do want the advice triggered for completion-metadata-get
     (when-let* ((cat (completion-metadata-get metadata 'category))
                 (annotate (marginalia--annotator cat)))
       (lambda (cand)
         (marginalia--context metadata
           (marginalia--cached annotate cand)))))
    ('affixation-function
     ;; We do want the advice triggered for `completion-metadata-get'.
     ;; Return wrapper around `annotation-function'.
     (when-let* ((cat (completion-metadata-get metadata 'category))
                 (annotate (marginalia--annotator cat)))
       (lambda (cands)
         (marginalia--context metadata
           (mapcar (lambda (x) (list x "" (or (marginalia--cached annotate x) ""))) cands)))))
    ('category
     ;; Find the completion category by trying each of our classifiers.
     ;; Store the metadata for `marginalia-classify-original-category'.
     (let ((marginalia--metadata metadata))
       (run-hook-with-args-until-success 'marginalia-classifiers)))))

(defun marginalia--minibuffer-setup ()
  "Setup minibuffer for `marginalia-mode'.
Remember `this-command' for `marginalia-classify-by-command-name'."
  (setq marginalia--cache t marginalia--this-command this-command)
  (marginalia--cache-reset))

(defun marginalia--base-position (completions)
  "Record the base position of COMPLETIONS."
  ;; NOTE: As a small optimization track the base position only for file completions,
  ;; since `marginalia--full-candidate' is only used for files as of now.
  (when minibuffer-completing-file-name
    (let ((base (or (cdr (last completions)) 0)))
      (unless (= marginalia--base-position base)
        (marginalia--cache-reset)
        (setq marginalia--base-position base))))
  completions)

;;;###autoload
(define-minor-mode marginalia-mode
  "Annotate completion candidates with richer information."
  :global t
  (if marginalia-mode
      (progn
        ;; Ensure that we remember this-command in order to select the annotation function.
        (add-hook 'minibuffer-setup-hook #'marginalia--minibuffer-setup)
        ;; Replace the metadata function.
        (advice-add #'completion-metadata-get :before-until #'marginalia--completion-metadata-get)
        ;; Record completion base position, for marginalia--full-candidate
        (advice-add #'completion-all-completions :filter-return #'marginalia--base-position))
    (advice-remove #'completion-all-completions #'marginalia--base-position)
    (advice-remove #'completion-metadata-get #'marginalia--completion-metadata-get)
    (remove-hook 'minibuffer-setup-hook #'marginalia--minibuffer-setup)))

;;;###autoload
(defun marginalia-cycle ()
  "Cycle between annotators in `marginalia-annotator-registry'."
  (interactive)
  (if-let* ((win (active-minibuffer-window))
            (buf (window-buffer win)))
      (with-current-buffer buf
        (let* ((pt (max 0 (- (point) (minibuffer-prompt-end))))
               (metadata (completion-metadata (buffer-substring-no-properties
                                               (minibuffer-prompt-end)
                                               (+ (minibuffer-prompt-end) pt))
                                              minibuffer-completion-table
                                              minibuffer-completion-predicate))
               (cat (completion-metadata-get metadata 'category)))
          (unless cat
            (user-error "Marginalia: Unknown completion category"))
          (setq cat (assq cat marginalia-annotator-registry))
          (unless cat
            (user-error "Marginalia: No annotators found"))
          (marginalia--cache-reset)
          (setcdr cat (append (cddr cat) (list (cadr cat))))
          ;; When the builtin annotator is selected and no builtin function is available, skip to
          ;; the next annotator. Note that we cannot use `completion-metadata-get' to access the
          ;; metadata since we must bypass the `marginalia--completion-metadata-get' advice.
          (when (and (eq (cadr cat) 'builtin)
                     (not (assq 'annotation-function metadata))
                     (not (assq 'affixation-function metadata))
                     (not (plist-get completion-extra-properties :annotation-function))
                     (not (plist-get completion-extra-properties :affixation-function)))
            (setcdr cat (append (cddr cat) (list (cadr cat)))))
          (message "Marginalia: Use annotator `%s' for category `%s'" (cadr cat) (car cat))))
    (user-error "Marginalia: No active minibuffer")))

(provide 'marginalia)
;;; marginalia.el ends here
