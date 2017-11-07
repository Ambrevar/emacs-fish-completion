# Emacs fish completion

This package extends the `pcomplete` completion framework with completion from the
[fish shell](http://fishshell.com/).

The `fish` shell has smart completion for a wide range of programs.  `fish` does
not require any special configuration to work with this package.

Eshell, which uses `pcomplete` for completion, can be made to fall back on
`fish` when it does not find any completion candidate with its native completion
support.

`M-x shell` can be made to use `fish`.  This will disable the underlying shell
completion.

## Setup

To enable fish completion in all Eshell and `M-x shell` buffers, add this to
your Emacs configuration:

	(when (and (executable-find "fish")
	           (require 'fish-completion nil t))
	  (global-fish-completion-mode))

The condition will prevent the package from loading if `fish` is not found
(change the executable name according to you local installation).

Alternatively, you can simply load the package with `(require 'fish-completion)`
and call `fish-completion-mode` manually.

Optionally, if the package
[bash-completion](https://github.com/szermatt/emacs-bash-completion) is
installed, `fish-completion-complete` can be configured to fall back on bash to
further try completing.  See `fish-completion-fallback-on-bash-p`.
