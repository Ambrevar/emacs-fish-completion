;;; fish-completion.el --- Add fish completion to pcomplete (shell and Eshell)  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <ambrevar@gmail.com>
;; Homepage: https://gitlab.com/Ambrevar/emacs-fish-completion
;; Version: 0.1

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; You can setup this package globally with:
;;
;; (when (and (executable-find "fish")
;;            (require 'fish-completion nil t))
;;   (global-fish-completion-mode))
;;
;; Alternatively, you can call the `fish-completion-mode' manually or in shell /
;; Eshell mode hook.
;;
;; The package `bash-completion' is an optional dependency: if available,
;; `fish-completion-complete' can be configured to fall back on bash to further
;; try completing.  See `fish-completion-fallback-on-bash-p'.

;;; Bugs:
;; If the fish user config changes directory on startup, file completion will
;; not be right.  One work-around is to add a "cd default-directory" before the
;; "complete", but that's brittle because of unreliable shell escaping.
;; Upstream does not allow for skipping the user config:
;; https://github.com/fish-shell/fish-shell/issues/4165.

;;; Code:

(require 'em-cmpl)

(defvar fish-completion-command "fish"
  "The `fish' executable.")

(defvar fish-completion--old-completion-function nil)
(make-variable-buffer-local 'fish-completion--old-completion-function)

(defvar fish-completion-fallback-on-bash-p nil
  "Fall back on bash completion if possible.

This requires the bash-completion package.")

;;;###autoload
(define-minor-mode fish-completion-mode
  "Turn on/off fish shell completion in all future shells or Eshells.

In `shell', completion is replaced by fish completion.
In `eshell', fish completion is only used when `pcomplete' fails."
  :init-value nil
  (if (not fish-completion-mode)
      (setq pcomplete-default-completion-function fish-completion--old-completion-function)
    (setq fish-completion--old-completion-function pcomplete-default-completion-function
          pcomplete-default-completion-function 'fish-completion-shell-complete)))

(defun turn-on-fish-completion-mode ()
  ;; The first Eshell session will initialize the modules and reload
  ;; `eshell-mode'.  Since the module em-cmpl sets
  ;; `pcomplete-default-completion-function', this will override this global
  ;; minor mode.  To avoid the override, we re-run `fish-completion-mode' in
  ;; `eshell-mode-hook' locally (first session only).  Other Eshell sessions do
  ;; not need this workaround.
  (when (eq major-mode 'eshell-mode)
      (add-hook 'eshell-mode-hook (lambda () (fish-completion-mode 1)) nil t))
  (fish-completion-mode 1))

(define-globalized-minor-mode global-fish-completion-mode fish-completion-mode turn-on-fish-completion-mode)

(defun fish-completion-shell-complete ()
  "Complete `shell' or `eshell' prompt with `fish-completion-complete'."
  (fish-completion-complete (buffer-substring-no-properties
                             (save-excursion (if (eq major-mode 'shell-mode) (comint-bol) (eshell-bol)) (point)) (point))))

(defun fish-completion-complete (raw-prompt)
  "Complete RAW-PROMPT (any string) using the fish shell.

If `fish-completion-fallback-on-bash-p' is non-nil and if the
`bash-completion' package is available, fall back on bash in case
no completion was found with fish."
  (while (pcomplete-here
          (let ((comp-list
                 (let* (;; Keep spaces at the end with OMIT-NULLS=nil in `split-string'.
                        (toks (split-string raw-prompt split-string-default-separators nil))
                        ;; The first non-empty `car' is the command.  Discard
                        ;; leading empty strings.
                        (tokens (progn (while (string= (car toks) "")
                                         (setq toks (cdr toks)))
                                       toks))
                        ;; Fish does not support subcommand completion.  We make
                        ;; a special case of 'sudo' and 'env' since they are
                        ;; the most common cases involving subcommands.  See
                        ;; https://github.com/fish-shell/fish-shell/issues/4093.
                        (prompt (if (not (member (car tokens) '("sudo" "env")))
                                    raw-prompt
                                  (setq tokens (cdr tokens))
                                  (while (and tokens
                                              (or (string-match "^-.*" (car tokens))
                                                  (string-match "=" (car tokens))))
                                    ;; Skip env/sudo parameters, like LC_ALL=C.
                                    (setq tokens (cdr tokens)))
                                  (mapconcat 'identity tokens " "))))
                   ;; Completion result can be a filename.  pcomplete expects
                   ;; cannonical file names (i.e. without '~') while fish preserves
                   ;; non-cannonical results.  If the result contains a directory,
                   ;; expand it.
                   (mapcar (lambda (e) (car (split-string e "\t")))
                           (split-string
                            (with-output-to-string
                              (with-current-buffer standard-output
                                (call-process fish-completion-command nil t nil "-c" (format "complete -C%s" (shell-quote-argument prompt)))))
                            "\n" t)))))
            (if (and (not comp-list)
                     fish-completion-fallback-on-bash-p
                     (require 'bash-completion nil t))
                (nth 2 (bash-completion-dynamic-complete-nocomint (save-excursion (eshell-bol) (point)) (point)))
              (if (and comp-list (file-exists-p (car comp-list)))
                  (pcomplete-dirs-or-entries)
                comp-list))))))

(provide 'fish-completion)
;;; fish-completion.el ends here
