# gauche-paren-type.el

An experimental Emacs minor mode to highlight parentheses in Gauche codes
based on their meaning.

# Requirement

Gauche 0.9.4 + [some patch](https://github.com/leque/Gauche/commit/b7a58224e87b6c4dc6bc64782e9a55f81d75d814).

# Installation

Copy `gauche-paren-type.el` to a directory in your `*load-path*`,
and `dump-call-type.el` to a directory in your `PATH`.

`(require 'gauche-paren-type)` in Emacs and type `M-x gauche-paren-type`
in a buffer visiting `*.scm` file.
