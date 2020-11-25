;;; readelf-mode.el --- Show readelf output instead of ELF binaries -*- lexical-binding: t -*-

;; Copyright (C) 2020  Ivan Sokolov

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Author: Ivan Sokolov <ivan-p-sokolov@ya.ru>
;; URL: https://github.com/sirikid/readelf-mode
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: data

;;; Commentary:

;;; Code:

(require 'cl-lib)

(defgroup readelf '()
  "Major mode for viewing ELF files."
  :group 'emacs)

(defcustom readelf-mode-executable "readelf"
  "Path to `readelf' binary."
  :group 'readelf-mode
  :type 'string)

(defcustom readelf-mode-flags '("--syms" "-W" file)
  "Arguments for `readelf'."
  :group 'readelf-mode
  :type '(repeat (choice string
                         (const :tag "File" file))))

(defun readelf-mode-revert-buffer ()
  "Revert buffer function for `readelf-mode'."
  (interactive)
  (when (derived-mode-p 'readelf-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (apply #'call-process readelf-mode-executable nil t t
             (mapcar (lambda (part)
                       (cl-case part
                         (file (buffer-file-name))
                         (t part)))
                     readelf-mode-flags))
      (goto-char (point-min))
      (save-excursion
        (flush-lines "^[[:space:]]*$"))
      (set-buffer-modified-p nil))))

;;;###autoload
(define-derived-mode readelf-mode special-mode "Readelf"
  "Major mode for viewing ELF files."
  :group 'readelf
  (read-only-mode)
  (readelf-mode-revert-buffer))

;;;###autoload
(add-to-list 'magic-mode-alist '("\177ELF" . readelf-mode))

(provide 'readelf-mode)
;;; readelf-mode.el ends here
