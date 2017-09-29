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

;; (require 'pocket-lib)

;;;; Variables

(defvar pocket-reader-mode-map
  (let ((map (make-keymap))
        (mappings '(
                    "RET" pocket-reader-show-url
                    "TAB" pocket-reader-pop-to-url
                    "a" pocket-reader-archive
                    "u" pocket-reader-readd
                    "*" pocket-reader-favorite-toggle
                    "f" pocket-reader-favorite-toggle
                    "s" pocket-reader-search
                    )))
    (cl-loop for (key fn) on mappings by #'cddr
             do (define-key map (kbd key) fn))
    map))

(defvar pocket-reader-items nil
  "Items to be shown.
This is stored in a var so we can fetch the items and calculate
settings for tabulated-list-mode based on it.")

;;;;; Customization

(defgroup pocket-reader nil
  "Library for accessing GetPocket.com API."
  :group 'external)

(defcustom pocket-reader-show-url-default-function
  #'org-web-tools-read-url-as-org
  "Default function to open items."
  :type 'function)

(defcustom pocket-reader-pop-to-url-default-function
  (lambda (url)
    (funcall #'org-web-tools-read-url-as-org url :show-buffer-fn #'pop-to-buffer))
  "Default function to pop-to items."
  :type 'function)

(defcustom pocket-reader-archive-on-open t
  "Mark items as read when opened."
  :type 'boolean)

(defcustom pocket-reader-show-count 50
  "Show this many items in the list."
  :type 'integer)

;;;;;; Faces

(defface pocket-reader-unread `((default :weight bold)) "Face for unread items")
(defface pocket-reader-archived `((default :weight normal)) "Face for archived items")

;;;; Macros

(defmacro with-pocket-reader (&rest body)
  "Run BODY in pocket-reader buffer."
  `(with-current-buffer "*pocket-reader*"
     (let ((inhibit-read-only t))
       ,@body)))

;;;; Mode

(define-derived-mode pocket-reader-mode tabulated-list-mode
  "Pocket Reader"
  :group 'pocket-reader

  (setq pocket-reader-items (pocket-reader-list-entries))

  (pocket-reader--set-tabulated-settings)
  (setq tabulated-list-entries pocket-reader-items)
  (setq tabulated-list-sort-key '("Added" . nil))

  (tabulated-list-init-header))

(defconst pocket-reader-keys
  '(:item_id
    :status
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

(defun pocket-reader ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*pocket-reader*"))
  (pocket-reader-mode)
  (tabulated-list-print 'remember-pos 'update)

  ;; Apply face
  (pocket-reader-apply-faces))

;;;; Functions

;;;;; Commands

(defun pocket-reader-show-url ()
  "Open URL of current item with default function."
  (interactive)
  (let ((url (pocket-reader-get-property :resolved_url)))
    (when (funcall pocket-reader-show-url-default-function url)
      ;; Item opened successfully
      (when pocket-reader-archive-on-open
        (with-pocket-reader
         (pocket-reader-archive))))))

(defun pocket-reader-pop-to-url ()
  "Open URL of current item with default pop-to function."
  (interactive)
  (let ((url (pocket-reader-get-property :resolved_url)))
    (when (funcall pocket-reader-pop-to-url-default-function url)
      ;; Item opened successfully
      (when pocket-reader-archive-on-open
        (with-pocket-reader
         (pocket-reader-archive))))))

(defun pocket-reader-archive ()
  "Mark current item as read."
  (interactive)
  (when (pocket-reader--action 'archive)
    ;; Item successfully archived
    (with-pocket-reader
     (set-text-properties (line-beginning-position) (line-end-position)
                          '(face pocket-reader-archived)))))

(defun pocket-reader-readd ()
  "Mark current item as unread."
  (interactive)
  (when (pocket-reader--action 'readd)
    ;; Item successfully archived
    (with-pocket-reader
     (set-text-properties (line-beginning-position) (line-end-position)
                          '(face pocket-reader-unread)))))

(defun pocket-reader-favorite-toggle ()
  "Toggle current item's favorite status."
  (interactive)
  (let ((action (if (string-empty-p (elt (tabulated-list-get-entry) 1))
                    ;; Not favorited; add it
                    'favorite
                  ;; Favorited; remove it
                  'unfavorite)))
    (when (pocket-reader--action action)
      ;; Item successfully toggled
      (tabulated-list-set-col 1
                              (cl-case action
                                (favorite "*")
                                (unfavorite ""))
                              t))))

(defun pocket-reader-search ()
  "Search Pocket items."
  (interactive)
  (when-let ((query (read-from-minibuffer "Query: ")))
    (setq tabulated-list-entries (pocket-reader-list-entries :search query))
    (tabulated-list-revert)
    (pocket-reader-apply-faces)))

;;;;; Helpers

(defun pocket-reader-apply-faces ()
  ;; TODO: Maybe we should use a custom print function but this is simpler
  (with-pocket-reader
   (goto-char (point-min))
   (while (not (eobp))
     (when (equal "0" (pocket-reader-get-property :status))
       (add-text-properties (line-beginning-position) (line-end-position)
                            '(face pocket-reader-unread)))
     (forward-line 1))
   (goto-char (point-min))))

(defun pocket-reader--action (action)
  "Execute ACTION on current item.
ACTION should be a string or symbol which is the name of an
action in the Pocket API."
  (with-pocket-reader
   (let* ((id (string-to-number (pocket-reader-get-property 'tabulated-list-id)))
          (item (list (cons 'item_id id))))
     (pocket-lib--action action item))))

(defun pocket-reader--set-tabulated-settings ()
  (let* ((site-width (cl-loop for item in pocket-reader-items
                              maximizing (length (elt (cadr item) 3))))
         (title-width (- (window-text-width) 11 2 site-width 1)))
    (setq tabulated-list-format (vector (list "Added" 10 nil)
                                        (list "*" 1 nil) ; FIXME: Sort by star
                                        (list "Title" title-width t)
                                        (list "Site" site-width t)))))

(defun pocket-reader-get-property (property)
  "Return value of PROPERTY for current item."
  (let ((pos (next-single-property-change (line-beginning-position) property nil (line-end-position))))
    (get-text-property pos property)))

(defun pocket-reader-list-entries (&optional &key search)
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
  (let* ((items (cdr (cl-third (pocket-lib-get
                                 :count pocket-reader-show-count
                                 :search search))))
         (item-plists (--map (cl-loop with item = (kvalist->plist (cdr it))
                                      for key in pocket-reader-keys
                                      for val = (plist-get item key)
                                      when val
                                      append (list key val))
                             items)))
    (cl-loop for it in item-plists
             for title = (apply #'propertize (plist-get it :resolved_title)
                                (cl-loop for key in pocket-reader-keys
                                         append (list key (plist-get it key))))
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
