;;; pocket-export.el --- Export Pocket items to CSV -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2017 Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net
;; Created: 2017-10-04
;; Version: 0.1-pre
;; Keywords: pocket
;; Package-Requires: ((emacs "25.1") (request "0.2") (dash "2.13.0") (kv "0.0.19") (s "1.10") (pocket-lib "0.1")
;; URL: https://github.com/alphapapa/pocket-export.el

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'subr-x)

(require 'dash)
(require 'kv)
(require 's)

(require 'pocket-lib)

;;;; Variables

;;;;; Customization


;;;; Functions

;;;;; Commands

(defun pocket-export-insert-items-as-csv (count)
  "Insert COUNT Pocket items as CSV into the current buffer."
  (interactive (list (read-number "Number of items: ")))
  (insert (pocket-export--csv (cdr (assoc 'list (pocket-lib-get :count count))))))

;;;;; Support

(cl-defun pocket-export--csv (items &key (with-header t))
  "Return Pocket ITEMS as CSV string."
  (cl-labels ((qv (value)
                  ;; Quote value
                  (format "\"%s\""
                          (cl-typecase value
                            (string (eq value))
                            (integer (number-to-string value)))))
              (eq (string)
                  ;; Escape quotes
                  (when string
                    (s-replace "\"" "\\\"" string))))
    (let* ((keys (kvalist->keys (cdar items)))
           (header (s-join "," (cl-loop for key in keys
                                        for name = (symbol-name key)
                                        collect (qv name))))
           (row-strings (cl-loop for item in items
                                 for item = (cdr item)
                                 collect (s-join "," (cl-loop for key in keys
                                                              collect (qv (cdr (assoc key item)))))))
           (data-string (s-join "\n" row-strings)))
      (concat (when with-header
                (concat header "\n"))
              data-string))))

;;;; Footer

(provide 'pocket-export)

;;; pocket-export.el ends here
