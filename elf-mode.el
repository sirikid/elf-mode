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

;;;###autoload
(define-derived-mode elf-mode fundamental-mode "Elf"
  "TODO"
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (shell-command-to-string
             (format elf-mode-command (buffer-file-name))))
    (set-buffer-modified-p nil)
    (read-only-mode 1)))

;;;###autoload
(add-to-list 'magic-mode-alist '("\177ELF" . elf-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.elf\\'" . elf-mode))

(provide 'elf-mode)
;;; elf-mode.el ends here
