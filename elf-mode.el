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

(require 'cl-macs)
(require 'seq)
(require 'subr-x)
(require 'thingatpt)

(defvar-local elf-mode-disassemble-command
  "objdump --disassemble=%s %s")

(defun elf-mode-set-disassemble-command (s)
  (interactive "sDisassemble command: ")
  (unless (string-empty-p s)
    (setq elf-mode-disassemble-command s)))

(defun elf-mode-disassemble (marker)
  (interactive)
  (let* ((symbol (save-excursion
                   (goto-char marker)
                   (word-at-point)))
         (buffer-name (format "%s(%s)" (buffer-name) symbol))
         (command (format elf-mode-disassemble-command symbol (buffer-file-name))))
    (with-current-buffer (pop-to-buffer buffer-name)
      (shell-command command (current-buffer))
      (flush-lines "^[[:space:]]*$" (point-min) (point-max))
      (set-buffer-modified-p nil)
      (asm-mode)
      (read-only-mode))))

(defvar elf-mode-command "readelf --syms -W %s"
  "The shell command to use for `elf-mode'.")

(defun elf-mode-entries ()
  (interactive)
  (let* ((command (format elf-mode-command (buffer-file-name)))
         (output (shell-command-to-string command))
         (lines (seq-drop (split-string output "\n" t) 2)))
    (mapcar
     (lambda (line)
       (let ((entry (apply #'vector (split-string line "[[:space:]]+" t))))
         (aset entry 0 (format "%8s" (aref entry 0)))
         (aset entry 2 (format "%8s" (aref entry 2)))
         (cl-case (length entry)
           (7 (setq entry (vconcat entry [""])))
           (8 (when (string= "FUNC" (aref entry 3))
                (aset entry 7 `(,(aref entry 7) . (action elf-mode-disassemble)))))
           (otherwise (error "Oops")))
         (list (aref entry 0) entry)))
     lines)))

(defconst elf-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    st))

;;;###autoload
(define-derived-mode elf-mode tabulated-list-mode "Elf"
  "TODO"
  :syntax-table elf-mode-syntax-table
  (setq tabulated-list-format [("Num" 8 t) ("Value" 8 t) ("Size"  8 t) ("Type"  8 t) ("Bind"  8 t) ("Vis"   8 t) ("Index" 8 t) ("Name"  8 t)]
        tabulated-list-entries #'elf-mode-entries)
  (define-key elf-mode-map (kbd "C-c C-c") #'elf-mode-set-disassemble-command)
  (tabulated-list-init-header)
  (tabulated-list-revert))

;;;###autoload
(add-to-list 'magic-mode-alist '("\177ELF" . elf-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.elf\\'" . elf-mode))

(provide 'elf-mode)
;;; elf-mode.el ends here
