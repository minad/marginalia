# marginalia.el - Marginalia in the minibuffer

This package provides `marginalia-mode` which adds marginalia to the minibuffer
completions. [Marginalia](https://en.wikipedia.org/wiki/Marginalia) are marks or
annotations placed at the margin of the page of a book or in this case helpful
colorful annotations placed at the margin of the minibuffer for your completion
candidates. The annotations are added based on the completion category. For
example `find-file` reports the `file` category and `M-x` reports the `command`
category. Furthermore the package allows to associate completion categories to
commands, since many commands (in contrast to `find-file` and `M-x`) do not
specify a completion category themselves.

![marginalia-mode with Selectrum](https://github.com/minad/marginalia/blob/main/marginalia-mode.png?raw=true)

## Configuration

~~~ elisp
;; Enable richer annotations using the Marginalia package
(use-package marginalia
  ;; The :init configuration is always executed (Not lazy!)
  :init t

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode)

  ;; Enable richer annotations for M-x.
  ;; Only keybindings are shown by default, in order to reduce noise for this very common command.
  ;; * marginalia-annotate-symbol: Annotate with the documentation string
  ;; * marginalia-annotate-command-binding (default): Annotate only with the keybinding
  ;; * marginalia-annotate-command-full: Annotate with the keybinding and the documentation string
  ;; (setf (alist-get 'command marginalia-annotate-alist) #'marginalia-annotate-command-full)
)
~~~
