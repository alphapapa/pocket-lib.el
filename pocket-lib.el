;;; pocket-lib.el --- Library for accessing getpocket.com API  -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2017 Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net
;; Created: 2017-08-18
;; Version: 0.1-pre
;; Keywords: pocket
;; Package-Requires: ((emacs "25.1") (request "0.2") (dash "2.13.0") (kv "0.0.19"))
;; URL: https://github.com/alphapapa/pocket-lib.el

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
;; `pocket-lib-default-extra-headers' a few lines in the call to
;; `request', and the consumer-key is currently the same.


;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'json)
(require 'request)

(require 'dash)
(require 'kv)

;;;; Variables

(defvar pocket-lib--access-token-have-opened-browser nil)
(defvar pocket-lib--request-token nil)
(defvar pocket-lib--access-token nil)
(defconst pocket-lib-default-extra-headers '(("Host" . "getpocket.com")
                                             ("Content-Type" . "application/json; charset=UTF-8")
                                             ("X-Accept" . "application/json")))

;;;;; Customization

(defgroup pocket-lib nil
  "Pocket"
  :group 'external)

(defcustom pocket-lib-consumer-key "30410-da1b34ce81aec5843a2214f4"
  "API consumer key"
  :type 'string)

(defcustom pocket-lib-token-file (expand-file-name "~/.cache/emacs-pocket-lib-token.json")
  "Pocket API token stored in this file."
  :type 'file)

;;;; Functions

;;;;; Authorization

(cl-defun pocket-lib--authorize (&key force)
  "Get and save authorization token.
If token already exists, don't get a new one, unless FORCE is non-nil."
  (when (or (not pocket-lib--access-token) force)
    (unless force
      ;; Try to load from file
      (pocket-lib--load-access-token))
    (unless (and pocket-lib--access-token
                 (not force))
      ;; Get new token
      (if-let ((request-token (pocket-lib--request-token :force force))
               (access-token (pocket-lib--access-token request-token :force force)))
          (pocket-lib--save-access-token access-token)
        (error "Unable to authorize")))))

(defun pocket-lib--load-access-token ()
  "Load access token from `pocket-lib-token-file'."
  (when (file-readable-p pocket-lib-token-file)
    (setq pocket-lib--access-token (ignore-errors
                                     (json-read-file pocket-lib-token-file)))))

(defun pocket-lib--save-access-token (token)
  "Write TOKEN to `pocket-lib-auth-file' and set variable."
  (with-temp-file pocket-lib-token-file
    (insert (json-encode-alist token)))
  (setq pocket-lib--access-token token))

(cl-defun pocket-lib--request-token (&key force)
  "Return request token.
If no token exists, or if FORCE is non-nil, get a new token."
  (when (or (not pocket-lib--request-token) force)
    (let* ((response (pocket-lib--request 'oauth/request
                                          :no-auth t :sync t
                                          :data (list :redirect_uri "http://www.google.com")))
           (data (request-response-data response))
           (token (alist-get 'code data)))
      (unless token
        (error "Unable to get request token: %s" response))
      (setq pocket-lib--request-token token)))
  pocket-lib--request-token)

(cl-defun pocket-lib--access-token (request-token &key force)
  "Return access token retrieved with REQUEST-TOKEN.
If FORCE is non-nil, get a new token."
  (if (or (null pocket-lib--access-token)
          force)
      (progn
        (if (and pocket-lib--access-token-have-opened-browser
                 (not force))
            ;; Already authorized in browser; try to get token
            (let ((response (pocket-lib--request 'oauth/authorize
                                                 :data (list :code request-token)
                                                 :no-auth t :sync t)))
              (or (request-response-data response)
                  (error "Unable to get access token: %s" response)))
          ;; Not authorized yet, or forcing; browse to authorize
          ;; FIXME: Is this a nice way to do this?
          (let ((url (concat "https://getpocket.com/auth/authorize?request_token=" request-token)))
            ;; NOTE: Doing it in w3m doesn't seem to work.  It only
            ;;  seems to work in a regular browser, and then only when
            ;;  the user is logged out of Pocket when he accesses the
            ;;  auth URL.  (browse-url url)
            (kill-new url))
          (setq pocket-lib--access-token-have-opened-browser t)
          (error "Please go to the URL in the clipboard to authorize the token request, then try again")))))

(defun pocket-lib--reset-auth ()
  "Reset all auth variables."
  (setq pocket-lib--request-token nil
        pocket-lib--access-token nil
        pocket-lib--access-token-have-opened-browser nil)
  (with-temp-file pocket-lib-token-file
    nil))

;;;;; Methods

(cl-defun pocket-lib--request (endpoint &key data sync no-auth)
  "Return request response struct for an API request to \"https://getpocket/com/v3/ENDPOINT\".

ENDPOINT may be a string or symbol, e.g. `get'.  DATA should be a
plist of API parameters; keys with nil values are removed.  SYNC
is passed to `request''s `:sync' keyword.

The consumer key and access token are included automatically,
unless NO-AUTH is set, in which case the access token is left
out (facilitating authorization requests).

The response body is automatically parsed with `json-read'."
  (declare (indent defun))
  (unless (or pocket-lib--access-token no-auth)
    (pocket-lib--authorize))
  (let* ((endpoint (cl-typecase endpoint
                     (symbol (symbol-name endpoint))
                     (string endpoint)))
         (url (concat "https://getpocket.com/v3/" endpoint))
         (data (json-encode
                (pocket-lib--plist-non-nil
                 (kvplist-merge (list :consumer_key pocket-lib-consumer-key
                                      :access_token (alist-get 'access_token
                                                               pocket-lib--access-token))
                                data)))))
    (request url
             :type "POST"
             :headers pocket-lib-default-extra-headers
             :data data
             :sync sync
             :parser #'json-read
             :success (cl-function
                       (lambda (&key data &allow-other-keys)
                         data))
             :error (cl-function
                     (lambda (&key data error-thrown symbol-status response &allow-other-keys)
                       (error "Request error: URL:%s  DATA:%s  ERROR-THROWN:%s  SYMBOL-STATUS:%s  RESPONSE:%s"
                              url data error-thrown symbol-status response))))))

(cl-defun pocket-lib-get (&key (offset 0) (count 10) (detail-type "simple")
                               state favorite tag content-type sort
                               search domain since)
  "Return JSON response for a \"get\" API request.

Without any arguments, this simply returns the first 10
unarchived, unfavorited, untagged items in the user's list.

By default, OFFSET is 0, COUNT is 10, and DETAIL-TYPE is
\"simple\".  All other keys are unset by default.  Keys set to
nil will not be sent in the request.

See <https://getpocket.com/developer/docs/v3/retrieve>."
  (let ((offset (number-to-string offset))
        (count (number-to-string count))
        (data (list :offset offset :count count :detail-type detail-type
                    :state state :favorite favorite :tag tag
                    :content-type content-type :sort sort
                    :search search :domain domain :since since)))
    (request-response-data
     (pocket-lib--request 'get
                          :data data :sync t))))

(cl-defun pocket-lib--send (actions)
  "Return JSON response for a \"send\" API request containing ACTIONS.
ACTIONS should be a list of actions; this function will convert
it into a vector automatically.

See <https://getpocket.com/developer/docs/v3/modify>."
  (request-response-data
   (pocket-lib--request 'send
                        :data (list :actions (vconcat actions)) :sync t)))

;;;;; Actions

(defun pocket-lib--archive (&rest items)
  "Archive ITEMS."
  ;; MAYBE: Needs error handling...maybe.  It does give an error in
  ;; the minibuffer if the API command gives an error.
  (pocket-lib--send (--map (list :action "archive"
                                 :item_id (alist-get 'item_id it))
                           items)))

;;;;; Helpers

(defun pocket-lib--plist-non-nil (plist)
  "Return PLIST without key-value pairs whose value is nil."
  (cl-loop for (key value) on plist by #'cddr
           unless (null value)
           append (list key value)))

;;;; Footer

(provide 'pocket-lib)

;;; pocket-lib.el ends here
