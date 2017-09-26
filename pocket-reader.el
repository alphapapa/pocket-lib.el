;;; pocket-reader.el --- Library for accessing getpocket.com API  -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2017 Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net
;; Created: 2017-09-25
;; Version: 0.1-pre
;; Keywords: pocket
;; Package-Requires: ((emacs "25.1") (dash "2.13.0") (kv "0.0.19") (pocket-lib "0.1"))
;; URL: https://github.com/alphapapa/pocket-reader.el

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

;; This package is based on pocket-api.el by DarkSun/lujun9972 at
;; <https://github.com/lujun9972/pocket-api.el>, which is based on
;; el-pocket by Tod Davies at <https://github.com/pterygota/el-pocket>.

;; It has essentially been completely written; no code remains except
;; `pocket-reader-default-extra-headers' a few lines in the call to
;; `request', and the consumer-key is currently the same.


;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'url-parse)

(require 'dash)
(require 'kv)

(require 'pocket-lib)

;;;; Variables



;;;;; Customization

(defgroup pocket-reader nil
  "Library for accessing GetPocket.com API."
  :group 'external)

;;;; Mode

(define-derived-mode pocket-reader-mode tabulated-list-mode
  "Pocket Reader"
  :group 'pocket-reader

  (setq tabulated-list-format (vector (list "Added" 10 nil)
                                      (list "*" 1 nil) ; FIXME: Sort by star
                                      (list "Title" 55 t)
                                      (list "Site" 15 t)))
  (setq tabulated-list-entries #'pocket-reader-list-entries)
  (setq tabulated-list-sort-key '("Added" . nil))

  (tabulated-list-init-header))

(defconst pocket-reader-keys
  '(:item_id
    :favorite
    :time_added
    :time_updated
    :time_read
    :resolved_title
    :excerpt
    :has_video
    :has_image
    :word_count
    :amp_url
    :resolved_url))

;;;; Functions

(defun pocket-reader ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*pocket-reader*"))
  (pocket-reader-mode)
  (tabulated-list-print 'remember-pos 'update))

(defun pocket-reader-list-entries ()
  ;; This buffer-local variable specifies the entries displayed in the
  ;; Tabulated List buffer.  Its value should be either a list, or a
  ;; function.
  ;;
  ;; If the value is a list, each list element corresponds to one entry,
  ;; and should have the form ‘(ID CONTENTS)’, where
  ;;
  ;; • ID is either ‘nil’, or a Lisp object that identifies the
  ;; entry.  If the latter, the cursor stays on the same entry when
  ;; re-sorting entries.  Comparison is done with ‘equal’.
  ;;
  ;; • CONTENTS is a vector with the same number of elements as
  ;; ‘tabulated-list-format’.  Each vector element is either a
  ;;  string, which is inserted into the buffer as-is, or a list
  ;;  ‘(LABEL . PROPERTIES)’, which means to insert a text button by
  ;;   calling ‘insert-text-button’ with LABEL and PROPERTIES as
  ;;   arguments (*note Making Buttons::).
  ;;
  ;;   There should be no newlines in any of these strings.

  ;; FIXME: Add error handling.
  (let* ((items (cdr (cl-third (pocket-lib-get))))
         (item-plists (--map (cl-loop with item = (kvalist->plist (cdr it))
                                      for key in pocket-reader-keys
                                      for val = (plist-get item key)
                                      when val
                                      append (list key val))
                             items)))
    (cl-loop for it in item-plists
             for title = (propertize (plist-get it :resolved_title)
                                     :resolved_url (plist-get it :resolved_url))
             collect (list (plist-get it :item_id)
                           (vector (pocket-reader--format-timestamp (string-to-number (plist-get it :time_added)))
                                   (pocket-reader--favorited-to-display (plist-get it :favorite))
                                   title
                                   (pocket-reader--url-domain (plist-get it :resolved_url)))))))

(defun pocket-reader--url-domain (url)
  "Return domain for URL.
Common prefixes like www are removed."
  (replace-regexp-in-string (rx bos (and (or "www") ".")) ""
                            (url-host (url-generic-parse-url url))))

(defun pocket-reader--favorited-to-display (val)
  "If VAL is 1, return the star character as a string, otherwise the empty string."
  (pcase val
    ("0" "")
    ("1" "*")))

(defun pocket-reader--format-timestamp (timestamp)
  ""
  (format-time-string "%Y-%m-%d" timestamp))

;;;; Footer

(provide 'pocket-reader)

;;; pocket-reader.el ends here
