;;; fish-completion.el --- Add Evil bindings to calendar

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <ambrevar@gmail.com>
;; Package-Version: 20171025.0000
;; Homepage: https://github.com/Ambrevar/emacs-fish-completion
;; Version: 0

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
;;   (fish-completion-eshell-global-toggle))
;;
;; Alternatively, you can call the `fish-completion-eshell-toggle' manually.

;;; Bugs:
;; If the user fish config change directory on startup, file completion will
;; not be right.  One work-around is to add a "cd default-directory" before the
;; "complete", but that's brittle because of unreliable shell escaping.
;; Upstream does not allow for skipping the user config:
;; https://github.com/fish-shell/fish-shell/issues/4165.

;;; Code:

(require 'em-cmpl)

(defvar fish-completion-command "fish"
  "The `fish' executable.")

;; TODO: Make minor mode for buffer-local completion?  Probably not worth it.

;;;###autoload
(defun fish-completion-eshell-global-toggle ()
  "Turn on/off fish shell completion in all future Eshells.
Eshell fallbacks on fish whenever it cannot complete normally."
  (interactive)
  (if (or (eq eshell-default-completion-function 'fish-completion-eshell-complete)
          (not (executable-find fish-completion-command)))
      (progn
        (setq eshell-default-completion-function (eval (car (get 'eshell-default-completion-function 'standard-value))))
        (message "fish completion disabled in all future Eshells"))
    (setq eshell-default-completion-function 'fish-completion-eshell-complete)
    (message "fish completion enabled in all future Eshells")))

;;;###autoload
(defun fish-completion-eshell-toggle ()
  "Turn on/off fish shell completion in current Eshell.
Eshell fallbacks on fish whenever it cannot complete normally."
  (interactive)
  (if (or (eq pcomplete-default-completion-function 'fish-completion-eshell-complete)
          (not (executable-find fish-completion-command)))
      (progn
        (setq pcomplete-default-completion-function (eval (car (get 'eshell-default-completion-function 'standard-value))))
        (message "fish completion disabled in current Eshell"))
    (setq pcomplete-default-completion-function 'fish-completion-eshell-complete)
    (message "fish completion enabled in current Eshell")))

(defun fish-completion-eshell-complete ()
  "Complete Eshell's prompt with `fish-completion-complete'."
  (fish-completion-complete (buffer-substring-no-properties
                             (save-excursion (eshell-bol) (point)) (point))))

(defun fish-completion-complete (raw-prompt)
  "Complete RAW-PROMPT (any string) using the fish shell."
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
                                (call-process fish-completion-command nil t nil "-c" (format "complete -C'%s'" prompt))))
                            "\n" t)))))
            (if (and comp-list (file-name-directory (car comp-list)))
                (pcomplete-dirs-or-entries)
              comp-list)))))

(provide 'fish-completion)
;;; fish-completion.el ends here
