# marginalia.el - Marginalia in the minibuffer

![marginalia-mode with Selectrum](https://github.com/minad/marginalia/blob/main/marginalia-mode.png?raw=true)

This package provides `marginalia-mode` which adds marginalia to the minibuffer
completions. [Marginalia](https://en.wikipedia.org/wiki/Marginalia) are marks or
annotations placed at the margin of the page of a book or in this case helpful
colorful annotations placed at the margin of the minibuffer for your completion
candidates. The annotations are added based on the completion category. For
example `find-file` reports the `file` category and `M-x` reports the `command`
category. You can choose between more or less detailed annotators, by
setting the variable `marginalia-annotators`.

Since many commands do not report a completion category themselves, Marginalia
provides a classifier system, which tries to guess the correct category based
for example on the prompt (see the variable `marginalia-prompt-categories`).
Usually these heuristic classifiers work well, but if they do not there is
always the possibility to overwrite categories by command name. This way you can
associate a fixed category with the completion initiated by the command (see the
variable `marginalia-command-categories`). The list of available classifiers is
specified by the variable `marginalia-classifiers`.

## Configuration

~~~ elisp
;; Enable richer annotations using the Marginalia package
(use-package marginalia
  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode)

  ;; Enable richer, more heavy, annotations.
  ;; E.g. M-x will show the documentation string additional to the keybinding.
  ;; By default only the keybinding is shown as annotation.
  (setq marginalia-annotators '(marginalia-annotators-heavy)))
~~~
