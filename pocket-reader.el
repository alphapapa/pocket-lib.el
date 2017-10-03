;;; pocket-reader.el --- Library for accessing getpocket.com API  -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2017 Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net
;; Created: 2017-09-25
;; Version: 0.1-pre
;; Keywords: pocket
;; Package-Requires: ((emacs "25.1") (dash "2.13.0") (kv "0.0.19") (pocket-lib "0.1") (s "1.10") (ov "1.0.6") (rainbow-identifiers "0.2.2"))
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
(require 'seq)

(require 'dash)
(require 'kv)
(require 'ov)
(require 's)
(require 'rainbow-identifiers)

(require 'org-web-tools)
(require 'pocket-lib)

;;;; Variables

(defvar pocket-reader-mode-map
  (let ((map (make-sparse-keymap))
        (mappings '(
                    "RET" pocket-reader-open-url
                    "TAB" pocket-reader-pop-to-url
                    "a" pocket-reader-toggle-archived
                    "b" pocket-reader-open-in-external-browser
                    "c" pocket-reader-copy-url
                    "e" pocket-reader-excerpt
                    "E" pocket-reader-excerpt-all
                    "u" pocket-reader-toggle-archived
                    "*" pocket-reader-toggle-favorite
                    "f" pocket-reader-toggle-favorite
                    "s" pocket-reader-search
                    "m" pocket-reader-more
                    "l" pocket-reader-limit
                    "tt" pocket-reader-add-tags
                    "ta" pocket-reader-add-tags
                    "tr" pocket-reader-remove-tags
                    "ts" pocket-reader-set-tags
                    )))
    (cl-loop for (key fn) on mappings by #'cddr
             do (define-key map (kbd key) fn))
    map))

(defvar pocket-reader-items nil
  "Items to be shown.
This is stored in a var so we can fetch the items and calculate
settings for tabulated-list-mode based on it.  NOTE: This may
become out-of-sync with `tabulated-list-entries', so it should
not be used outside of functions that already use it.")

(defvar pocket-reader-offset 0
  "The current offset.")

(defvar pocket-reader-query nil
  "The current query string.")

(defconst pocket-reader-keys
  '(:item_id
    :status
    :favorite
    (:tags . pocket-lib--process-tags)
    :time_added
    :time_updated
    :time_read
    :given_title
    :resolved_title
    :excerpt
    :has_video
    :has_image
    :word_count
    :amp_url
    :resolved_url)
  "Keys to use in Pocket API responses, optionally with function to filter each one through.")

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

(defcustom pocket-reader-color-site t
  "Colorize site names uniquely."
  :type 'boolean)

(defcustom pocket-reader-color-title t
  "Colorize titles according to site."
  :type 'boolean)

(defcustom pocket-reader-show-count 50
  "Show this many items in the list."
  :type 'integer)

(defcustom pocket-reader-url-open-fn-map
  '((eww-browse-url "news.ycombinator.com"))
  "A list mapping URL-matching regular expressions to functions used to open the URL.
Regexps are anchored after the protocol (i.e. \"https://\" is not
matched against).

This is useful when certain sites should be opened in an external
browser.  The list is backward in the sense that the functions
are listed first, followed by the regexps, in this format: (FN
REGEXP REGEXP ...)."
  :type '(alist :key-type function
                :value-type (repeat string)))

(defcustom pocket-reader-finalize-hook
  '(pocket-reader--apply-faces
    pocket-reader--add-overlays)
  "Functions run after printing items into the buffer."
  :type 'hook
  :options '(pocket-reader--apply-faces
             pocket-reader--add-overlays))

;;;;;; Faces

(defface pocket-reader-unread `((default :weight bold)) "Face for unread items")
(defface pocket-reader-archived `((default :weight normal)) "Face for archived items")
(defface pocket-reader-favorite-star `((default :foreground "#b58900")) "Face for archived items")

;;;; Macros

(defmacro with-pocket-reader (&rest body)
  "Run BODY in pocket-reader buffer."
  `(with-current-buffer "*pocket-reader*"
     (let ((inhibit-read-only t))
       ,@body)))

(cl-defmacro pocket-reader--keywords-in-list (list &rest keywords)
  "If any KEYWORDS are in LIST, destructively remove them from LIST and return the last KEYWORD found in LIST."
  (declare (debug nil))
  `(car (last (cl-loop for keyword in ',keywords
                       when (member keyword ,list)
                       do (setq ,list (delete keyword ,list))
                       and collect (s-replace (rx ":") "" keyword)))))

(cl-defmacro pocket-reader--regexp-in-list (list regexp &optional (prefix ":"))
  "If REGEXP matches strings in LIST, destructively remove strings from LIST and return the last string without PREFIX."
  `(car (last (cl-loop for string in ,list
                       when (string-match ,regexp string)
                       do (setq ,list (delete string ,list))
                       and collect (replace-regexp-in-string (rx-to-string '(seq bos ,prefix)) "" string)))))

;;;; Mode

(define-derived-mode pocket-reader-mode tabulated-list-mode
  "Pocket Reader"
  :group 'pocket-reader

  ;; FIXME: Unfortunately I can't get (local 'symbol) to work with
  ;; `advice-add', and I can't get `add-function' to work either, so I
  ;; have to use `advice-add', test the buffer each time the advice is
  ;; called, and delete the advice manually when the buffer is killed.
  (advice-add 'tabulated-list--sort-by-column-name :after 'pocket-reader--finalize)
  (add-hook 'kill-buffer-hook (lambda ()
                                (advice-remove 'tabulated-list--sort-by-column-name 'pocket-reader--finalize))
            'append 'local)

  (setq tabulated-list-sort-key '("Added" . nil))
  (pocket-reader-search)
  ;; Invert initial sort order, putting most recent items on top
  (tabulated-list-sort 0))

;;;; Functions

;;;;; Commands

(defun pocket-reader ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*pocket-reader*"))
  (pocket-reader-mode))

(defun pocket-reader-search (&optional query)
  "Search Pocket items with QUERY."
  ;; This function is the main one used to get and display items.
  ;; When QUERY is nil, it simply shows the default list of unread items.
  (interactive (list (read-from-minibuffer "Query: ")))
  (custom-reevaluate-setting 'pocket-reader-show-count)
  (setq pocket-reader-offset 0
        pocket-reader-query query
        pocket-reader-items nil)
  (pocket-reader--add-items (pocket-reader--get-items query)))

(defun pocket-reader-more (count)
  "Fetch and show COUNT more items."
  (interactive "p")
  (let* ((count (if (= 1 count)
                    pocket-reader-show-count
                  count))
         (offset (incf pocket-reader-offset count)))
    (pocket-reader--add-items (pocket-reader--get-items pocket-reader-query))))

(defun pocket-reader-limit (query)
  "Limit display to items matching QUERY."
  (interactive (list (read-from-minibuffer "Query: ")))
  (if (s-present? query)
      (save-excursion
        (goto-char (point-min))
        (cl-loop while (not (eobp))
                 unless (re-search-forward query (line-end-position) t)
                 do (ov (line-beginning-position) (1+ (line-end-position)) 'display "")
                 do (forward-line 1)))
    ;; No query; show all entries
    (ov-clear 'display "")))

(defun pocket-reader-excerpt ()
  "Show excerpt for current item."
  (interactive)
  (let ((excerpt (pocket-reader--get-property :excerpt)))
    (unless (s-blank-str? excerpt)
      (let* ((start-col (1+ (pocket-reader--column-start "Title")))
             (prefix (s-repeat start-col " "))
             (width (- (window-text-width) start-col))
             (left-margin start-col)
             (string (concat prefix (s-trim (propertize (pocket-reader--wrap-string excerpt width)
                                                        'face 'default)) "\n")))
        (unless (cl-loop for ov in (ov-forwards)
                         when (equal string (ov-val ov 'before-string))
                         do (ov-reset ov)
                         and return t)
          (ov (1+ (line-end-position)) (1+ (line-end-position))
              'before-string string))))))

(defun pocket-reader--column-start (column)
  "Return text column that COLUMN starts at on each line."
  (let* ((column-num (cl-typecase column
                       (integer column)
                       (string (tabulated-list--column-number column))))
         (column-data (aref tabulated-list-format column-num))
         (start-col (1+ (cl-loop for i from 0 below column-num
                                 for col-data = (aref tabulated-list-format i)
                                 for col-width = (elt col-data 1)
                                 sum col-width)))
         (column-width (elt column-data 1))
         (end-pos (+ start-col column-width)))
    start-col))

(defun pocket-reader-excerpt-all ()
  "Show all excerpts."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; FIXME: This won't work if the first item has no excerpt
    (let ((first-excerpt (pocket-reader--get-property :excerpt)))
      (if (cl-loop for ov in (ov-forwards)
                   thereis (equal (ov-val ov 'before-string) first-excerpt))
          ;; Already shown; hide all
          (cl-loop for ov in (ov-forwards)
                   when (not (equal (ov-val ov 'before-string) "\n"))
                   do (ov-reset ov))
        ;; Show all
        (while (not (eobp))
          (pocket-reader-excerpt)
          (forward-line 1))))))

(defun pocket-reader--wrap-string (string length)
  "Wrap STRING to LENGTH."
  (if (> (length string) length)
      (s-trim (with-temp-buffer
                (insert string)
                (let ((fill-column length))
                  (fill-region-as-paragraph (point-min) (point-max))
                  (buffer-string))))
    string))

;;;;;; Tags

(defun pocket-reader-add-tags (new-tags)
  "Add tags to current item."
  (interactive (list (read-from-minibuffer "Tags: ")))
  (let ((item (pocket-reader--current-item))
        (new-tags (--> new-tags
                       (s-split " " it 'omit-nulls)
                       (s-join "," it)))
        (old-tags (pocket-reader--get-property :tags)))
    (when (and new-tags
               (pocket-lib--tags-action 'tags_add new-tags item))
      ;; Tags added successfully
      (pocket-reader--set-entry-property :tags (append (s-split "," new-tags)
                                                       old-tags))
      (pocket-reader--set-tags-column)
      ;; Fix face
      (pocket-reader--apply-faces-to-line))))

(defun pocket-reader-remove-tags (remove-tags)
  "Remove tags from current item."
  (interactive (list (completing-read "Tags: " (pocket-reader--get-property :tags))))
  (let* ((item (pocket-reader--current-item))
         (old-tags (pocket-reader--get-property :tags))
         (remove-tags (s-split " " remove-tags 'omit-nulls))
         (remove-tags-string (s-join "," remove-tags))
         (new-tags (or (seq-difference old-tags remove-tags)
                       '(" "))))
    (when (and remove-tags
               (pocket-lib--tags-action 'tags_remove remove-tags-string item))
      ;; Tags removed successfully
      (pocket-reader--set-entry-property :tags new-tags)
      (pocket-reader--set-tags-column)
      ;; Fix face
      (pocket-reader--apply-faces-to-line))))

(defun pocket-reader-set-tags (tags)
  "Set TAGS of current item."
  (interactive (list (read-from-minibuffer "Tags: ")))
  (with-pocket-reader
   (let* ((item (pocket-reader--current-item))
          (tags (s-split " " tags 'omit-nulls))
          (tags-string (s-join "," tags)))
     (when (pocket-lib--tags-action 'tags_replace tags-string item)
       ;; Tags replaced successfully
       (pocket-reader--set-entry-property :tags tags)
       (pocket-reader--set-tags-column)
       ;; Fix face
       (pocket-reader--apply-faces-to-line)))))

;;;;;; URL-opening

(defun pocket-reader-open-url (&optional &key fn)
  "Open URL of current item with default function."
  (interactive)
  (let* ((url (pocket-reader--get-property :resolved_url))
         (fn (or fn (pocket-reader--map-url-open-fn url))))
    (when (funcall fn url)
      ;; Item opened successfully
      (when pocket-reader-archive-on-open
        (with-pocket-reader
         (pocket-reader-toggle-archived))))))

(defun pocket-reader-pop-to-url ()
  "Open URL of current item with default pop-to function."
  (interactive)
  (pocket-reader-open-url :fn #'pocket-reader-pop-to-url-default-function))

(defun pocket-reader-open-in-external-browser ()
  (interactive)
  (let ((pocket-reader-open-url-default-function #'browse-url-default-browser))
    (call-interactively #'pocket-reader-open-url)))

(defun pocket-reader-copy-url ()
  "Copy URL of current item to kill-ring/clipboard."
  (interactive)
  (when-let ((url (pocket-reader--get-property :resolved_url)))
    (kill-new url)
    (message url)))

;;;;;; Other

(defun pocket-reader-toggle-favorite ()
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
                              t)
      (pocket-reader--apply-faces-to-line))))

(defun pocket-reader-toggle-archived ()
  "Toggle current item's archived/unread status."
  (interactive)
  (let* ((action (pcase (pocket-reader--get-property :status)
                   ;; Unread; archive
                   ("0" 'archive)
                   ;; Archived; readd
                   ("1" 'readd)))
         (face (cl-case action
                 ('archive 'pocket-reader-archived)
                 ('readd 'pocket-reader-unread)))
         (status (cl-case action
                   ('archive "1")
                   ('readd "0"))))
    (when (pocket-reader--action action)
      ;; Item successfully toggled
      (with-pocket-reader
       (pocket-reader--set-entry-property :status status)
       (pocket-reader--apply-faces-to-line)))))

;;;;; Helpers

(defun pocket-reader--add-items (items)
  "Add and display ITEMS."
  (setq pocket-reader-items (append pocket-reader-items items))
  (pocket-reader--set-tabulated-settings)
  (setq tabulated-list-entries pocket-reader-items)
  (tabulated-list-init-header)
  (tabulated-list-revert)
  (pocket-reader--finalize))

(defun pocket-reader--finalize (&rest ignore)
  "Finalize the buffer after adding or sorting items."
  ;; Make sure it's the pocket-reader buffer.
  (when (string= "*pocket-reader*" (buffer-name))
    (run-hooks 'pocket-reader-finalize-hook)))

(defun pocket-reader--get-items (&optional query)
  "Return Pocket items for QUERY.
QUERY is a string which may contain certain keywords:

:*, :favorite  Return only favorited items.
:archive      Return only archived items.
:unread        Return only unread items (default).
:all           Return all items.
:COUNT         Return at most COUNT (a number) items."
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
  (let* ((query (or query ""))
         (query-words (s-split " " query))
         (state (pocket-reader--keywords-in-list query-words ":archive" ":all" ":unread"))
         (favorite (when (pocket-reader--keywords-in-list query-words ":favorite" ":*") 1))
         (count (setq pocket-reader-show-count
                      (or (--when-let (pocket-reader--regexp-in-list query-words (rx bos ":" (1+ digit) eos))
                            (string-to-number it))
                          pocket-reader-show-count)))
         (query-string (s-join " " query-words))
         (items (cdr (cl-third (pocket-lib-get
                                 :detail-type "complete"
                                 :count count
                                 :offset pocket-reader-offset
                                 :search query-string
                                 :state state
                                 :favorite favorite))))
         (item-plists (--map (cl-loop with item = (kvalist->plist (cdr it))
                                      for key in pocket-reader-keys
                                      for fn = nil
                                      when (consp key)
                                      do (setq fn (cdr key)
                                               key (car key))
                                      for val = (if fn
                                                    (funcall fn (plist-get item key))
                                                  (plist-get item key))
                                      when val
                                      append (list key val))
                             items)))
    (cl-loop for it in item-plists
             for title = (pocket-reader--not-empty-string (apply #'propertize (or (plist-get it :resolved_title)
                                                                                  (plist-get it :given_title)
                                                                                  "[untitled]")
                                                                 (cl-loop for key in pocket-reader-keys
                                                                          when (consp key)
                                                                          do (setq key (car key))
                                                                          append (list key (plist-get it key)))))
             for tags = (pocket-reader--not-empty-string (s-join "," (plist-get it :tags)))
             collect (list (plist-get it :item_id)
                           (vector (pocket-reader--format-timestamp (string-to-number (plist-get it :time_added)))
                                   (pocket-reader--favorite-string (plist-get it :favorite))
                                   title
                                   (pocket-reader--url-domain (plist-get it :resolved_url))
                                   tags)))))

(defun pocket-reader--action (action &optional arg)
  "Execute ACTION on current item.
ACTION should be a string or symbol which is the name of an
action in the Pocket API."
  (with-pocket-reader
   (pocket-lib--action action (pocket-reader--current-item))))

(defun pocket-reader--set-tags-column ()
  "Set tags column for current entry."
  (tabulated-list-set-col 4 (s-join "," (pocket-reader--get-property :tags))))

(defun pocket-reader--set-tabulated-settings ()
  (let* ((site-width (cl-loop for item in pocket-reader-items
                              maximizing (length (elt (cadr item) 3))))
         (title-width (- (window-text-width) 11 2 site-width 10 1)))
    (setq tabulated-list-format (vector (list "Added" 10 t)
                                        (list "*" 1 nil) ; FIXME: Sort by star
                                        (list "Title" title-width t)
                                        (list "Site" site-width t)
                                        (list "Tags" 10 t)))))

(defun pocket-reader--map-url-open-fn (url)
  "Return function to use to open URL."
  (or (car (cl-rassoc url pocket-reader-url-open-fn-map
                      :test (lambda (url regexp)
                              (string-match (rx-to-string `(seq "http" (optional "s") "://"
                                                                (regexp ,(car regexp))
                                                                (or "/" eos)))
                                            url))))
      pocket-reader-open-url-default-function))

(defun pocket-reader--current-item ()
  "Return list containing cons of current item's ID, suitable for passing to pocket-lib."
  (let* ((id (string-to-number (tabulated-list-get-id)))
         (item (list (cons 'item_id id))))
    item))

(defun pocket-reader--get-property (property)
  "Return value of PROPERTY for current item."
  (get-text-property 0 property (elt (tabulated-list-get-entry) 2)))

(defun pocket-reader--set-entry-property (property value)
  "Set current item's PROPERTY to VALUE."
  ;; Properties are stored in the title column
  (with-pocket-reader
   (let ((title (elt (tabulated-list-get-entry) 2)))
     (put-text-property 0 (length title)
                        property value
                        title)
     (tabulated-list-set-col 2 title))))

(defun pocket-reader--not-empty-string (s)
  "If S is non-empty, return it; otherwise return \" \"."
  ;; No column may be actually empty, because `tabulated-list-set-col' doesn't work on
  ;; nil columns, because it uses `next-single-property-change' to find the place to
  ;; modify.  So we use an empty string instead of nil.
  (if (string-empty-p s)
      " "
    s))

(defun pocket-reader--url-domain (url)
  "Return domain for URL.
Common prefixes like www are removed."
  (replace-regexp-in-string (rx bos (and (or "www") ".")) ""
                            (url-host (url-generic-parse-url url))))

(defun pocket-reader--favorite-string (val)
  "If VAL is 1, return the star character as a string, otherwise the empty string."
  (pcase val
    ("0" "")
    ("1" "*")))

(defun pocket-reader--format-timestamp (timestamp)
  "Format TIMESTAMP."
  (format-time-string "%Y-%m-%d" timestamp))

(defun pocket-reader--add-overlays (&rest ignore)
  "Insert overlay spacers where the current sort column's values change.
For example, if sorted by date, a spacer will be inserted where the date changes."
  (let ((sort-column (seq-position tabulated-list-format tabulated-list-sort-key
                                   (lambda (seq elt)
                                     (string= (car seq) (car elt))))))
    (ov-clear)
    (save-excursion
      (goto-char (point-min))
      (cl-loop with prev-data = (elt (tabulated-list-get-entry) sort-column)
               while (not (eobp))
               do (forward-line 1)
               for current-data = (elt (tabulated-list-get-entry) sort-column)
               when (not (equal current-data prev-data))
               do (progn
                    (ov (line-beginning-position) (line-beginning-position) 'before-string "\n")
                    (setq prev-data current-data))))))

;;;;;; Faces

(defun pocket-reader--apply-faces ()
  ;; TODO: Maybe we should use a custom print function but this is simpler
  (with-pocket-reader
   (goto-char (point-min))
   (while (not (eobp))
     (pocket-reader--apply-faces-to-line)
     (forward-line 1))
   (goto-char (point-min))))

(defun pocket-reader--apply-faces-to-line ()
  "Apply faces to current line."
  (with-pocket-reader
   (add-text-properties (line-beginning-position) (line-end-position)
                        (list 'face (pcase (pocket-reader--get-property :status)
                                      ("0" 'pocket-reader-unread)
                                      ("1" 'pocket-reader-read)) ))
   (when (pocket-reader--get-property :favorite)
     (pocket-reader--set-column-face "*" 'pocket-reader-favorite-star))
   (when (or pocket-reader-color-site
             pocket-reader-color-title)
     (pocket-reader--set-site-face))))

(defun pocket-reader--set-site-face ()
  "Apply colored face to site column for current entry."
  (let* ((column (tabulated-list--column-number "Site"))
         (site (elt (tabulated-list-get-entry) column))
         (hash (rainbow-identifiers--hash-function site))
         (face (rainbow-identifiers-cie-l*a*b*-choose-face hash)))
    (when pocket-reader-color-site
      (pocket-reader--set-column-face "Site" face))
    (when pocket-reader-color-title
      (pocket-reader--set-column-face "Title" face))))

(defun pocket-reader--set-column-face (column face)
  "Apply FACE to COLUMN on current line.
COLUMN may be the column name or number."
  (let* ((column-num (cl-typecase column
                       (integer column)
                       (string (tabulated-list--column-number column))))
         (column-data (aref tabulated-list-format column-num))
         (start-pos (+ (line-beginning-position)
                       (1+ (cl-loop for i from 0 below column-num
                                    for col-data = (aref tabulated-list-format i)
                                    for col-width = (elt col-data 1)
                                    sum col-width))))
         (column-width (elt column-data 1))
         (end-pos (+ start-pos column-width)))
    (with-pocket-reader
     (add-face-text-property start-pos end-pos face t))))

;;;; Footer

(provide 'pocket-reader)

;;; pocket-reader.el ends here
