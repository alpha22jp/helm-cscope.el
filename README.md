# helm-cscope.el

`helm-cscope.el` is a [helm](https://github.com/emacs-helm/helm) interface for [xcscope.el](https://github.com/dkogan/xcscope.el). You can search cscope database and narrow selection using helm interface.

Helm-cscope shares most functions with xcscope.el. It just provide `M-x helm-cscope-find-*` commands as helm version alternatives for `M-x cscope-find-*`.

## Requirements

* Emacs 24 or higher
* xcscope.el
* helm.el

## Installation

This package can be installed via MELPA as below.

<kbd>M-x package-install [RET] helm-cscope [RET]</kbd>

## Configuration

Nothing required if you have already configured xcscope and helm. You can use `M-x helm-cscope-find-*` commands just after installation.

## History

version 0.1.0 (2015-03-15)

* Initial release.
