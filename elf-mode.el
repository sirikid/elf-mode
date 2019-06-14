;;; elf-mode.el --- Show symbols in binaries -*- lexical-binding: t -*-

;; Copyright (C) 2016 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/elf-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: matching

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Toggle `elf-mode' to show the symbols that the binary uses instead
;; of the actual binary contents.
;;
;; Use `elf-setup-default' to make `elf-mode' get called
;; automatically.

;;; Code:

(defvar elf-mode-command "readelf --syms -W %s"
  "The shell command to use for `elf-mode'.")

(defun elf-revert-buffer ()
  (interactive)
  (when (eq 'elf-mode major-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert
       (shell-command-to-string
        (format elf-mode-command (buffer-file-name))))
      (set-buffer-modified-p nil))))

(defvar-local elf-mode-disassemble-command
  "objdump --disassemble=%s %s")

(defun elf-mode-set-disassemble-command (s)
  (interactive "sDisassemble command: ")
  (unless (string-empty-p s)
    (setq elf-mode-disassemble-command s)))

(defun elf-mode-disassemble (o)
  (interactive)
  (let* ((symbol (buffer-substring (overlay-start o) (overlay-end o)))
         (buffer-name (format "%s(%s)" (buffer-name) symbol))
         (command (format elf-mode-disassemble-command symbol (buffer-file-name))))
    (with-current-buffer (pop-to-buffer buffer-name)
      (shell-command command (current-buffer))
      (flush-lines "^[[:space:]]*$" (point-min) (point-max))
      (set-buffer-modified-p nil)
      (asm-mode)
      (read-only-mode))))

(defun elf-add-func-refs ()
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "FUNC")
      (dotimes (_i 3)
        (skip-chars-forward " \t")
        (search-forward " "))
      (let ((beg (point))
            (end (progn (forward-word) (point))))
        (make-button
         beg end
         'action #'elf-mode-disassemble
         'mouse-action #'elf-mode-disassemble)))))

(defconst elf-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    st))

;;;###autoload
(define-derived-mode elf-mode special-mode "Elf"
  "TODO"
  :syntax-table elf-mode-syntax-table
  (elf-revert-buffer)
  (elf-add-func-refs)
  (read-only-mode))

;;;###autoload
(add-to-list 'magic-mode-alist '("\177ELF" . elf-mode))

(provide 'elf-mode)
;;; elf-mode.el ends here
