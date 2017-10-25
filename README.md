# Emacs fish completion

This package extends the `pcomplete` completion framework with completion from the
[fish shell](http://fishshell.com/).

The fish shell has smart completion for a wide range of programs.

Eshell, which uses `pcomplete` for completion, can be made to fallback on fish
when it does not find any completion candidate with its native completion
support.

## Installation

To enable fish fallback completion in all Eshell buffers, add this to your Emacs
configuartion:

	(when (and (executable-find "fish")
	           (require 'fish-completion nil t))
	   (fish-completion-eshell-global-toggle))

The condition will prevent the package from loading if `fish` is not found
(change the executable name according to you local installation).

Alternatively, you can simply load the package with `(require 'fish-completion)`
and call `fish-completion-eshell-toggle` manually.
