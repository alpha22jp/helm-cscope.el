# helm-cscope.el [![MELPA](http://melpa.org/packages/helm-cscope-badge.svg)](http://melpa.org/#/helm-cscope) [![MELPA Stable](http://stable.melpa.org/packages/helm-cscope-badge.svg)](http://stable.melpa.org/#/helm-cscope)

`helm-cscope.el` is a [helm](https://github.com/emacs-helm/helm) interface for [xcscope.el](https://github.com/dkogan/xcscope.el). You can search cscope database and narrow selection using helm interface.

Helm-cscope shares most functions with xcscope.el. It just provide `M-x helm-cscope-find-*` commands as helm version alternatives for `M-x cscope-find-*`.

## Requirements

* Emacs 24 or higher
* xcscope.el
* helm.el
* cscope command

## Installation

This package can be installed via MELPA as below.

<kbd>M-x package-install [RET] helm-cscope [RET]</kbd>

## Configuration

Nothing required if you have already configured xcscope and helm. You can use `M-x helm-cscope-find-*` commands just after installation.

## Customization and tips

### Key bindings

`helm-cscope` doesn't do any key bindings by default. Here is an example of key mappings.

```init.el
(add-hook 'c-mode-common-hook 'helm-cscope-mode)
(add-hook 'helm-cscope-mode-hook
          (lambda ()
            (local-set-key (kbd "M-.") 'helm-cscope-find-global-definition)
            (local-set-key (kbd "M-@") 'helm-cscope-find-calling-this-funtcion)
            (local-set-key (kbd "M-s") 'helm-cscope-find-this-symbol)
            (local-set-key (kbd "M-,") 'helm-cscope-pop-mark)))))
```

### Multiple database support

`helm-cscope` can handle the variable `cscope-database-regexps` in the same manner as `xcscope.el` aside from "t" and "(t)" are ignored. `helm-cscope` shows the search results as separeted helm source for each DBLIST item.

## History

version 0.1.1 (2015-03-20)

* Fix: incorrect handling of `cscope-database-regexps`.
* Modify: add fuzzy-match option to helm source.

version 0.1.0 (2015-03-15)

* Initial release.
