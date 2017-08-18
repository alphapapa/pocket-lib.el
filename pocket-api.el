;;; pocket-api.el --- another pocket api -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2016 Free Software Foundation, Inc.

;; Author: DarkSun <lujun9972@gmail.com>
;; Created: 2016-05-23
;; Version: 0.1
;; Keywords: convenience, pocket
;; Package-Requires: ((emacs "24.4") (request "0.2"))
;; URL: https://github.com/lujun9972/pocket-api.el

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

;;; Source code
;;
;; pocket-api's code can be found here:
;;   http://github.com/lujun9972/pocket-api.el

;;; Commentary:

;; The usage is similar with [[https://github.com/pterygota/el-pocket][el-pocket]].

;; The first time using `pocket-api', you should execute =pocket-api-authorize= twice.

;; 1. The first time execute =pocket-api-authorize= you will be directed to the oauth/request page, where you can click on authorize. After authorizing, you may see an error page, but it don't matter.

;; 2. And then, the second time execute =pocket-api-authorize= you will get the access token, and it will be saved to =~/.el-pocket-auth.json=

;; After that, you don't need to do the authorizing job any more, just use =(el-pocket-load-auht)= to reload the access token.

;; Usng =M-x el-pocket-add= to add URLs

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'json)
(require 'request)

(require 'dash)
(require 'kv)

;;;; Variables

(defvar pocket-api--access-token-have-opened-browser nil)
(defvar pocket-api--request-token nil)
(defvar pocket-api--access-token nil)
(defconst pocket-api-default-extra-headers '(("Host" . "getpocket.com")
                                             ("Content-Type" . "application/json; charset=UTF-8")
                                             ("X-Accept" . "application/json")))

;;;;; Customization

(defgroup pocket-api nil
  "Pocket"
  :group 'external)

(defcustom pocket-api-consumer-key "30410-da1b34ce81aec5843a2214f4"
  "API consumer key"
  :type 'string)

(defcustom pocket-api-token-file (expand-file-name "~/.cache/emacs-pocket-api-token.json")
  "Pocket API token stored in this file."
  :type 'file)

;;;; Functions

;;;;; Authorization

(cl-defun pocket-api--authorize (&key force)
  "Get and save authorization token.
If token already exists, don't get a new one, unless FORCE is non-nil."
  (when (or (not pocket-api--access-token) force)
    (unless force
      ;; Try to load from file
      (pocket-api--load-access-token))
    (unless (and pocket-api--access-token
                 (not force))
      ;; Get new token
      (if-let ((request-token (pocket-api--request-token :force force))
               (access-token (pocket-api--access-token request-token :force force)))
          (pocket-api--save-access-token access-token)
        (error "Unable to authorize")))))

(defun pocket-api--load-access-token ()
  "Load access token from `pocket-api-token-file'."
  (when (file-readable-p pocket-api-token-file)
    (setq pocket-api--access-token (ignore-errors
                                     (json-read-file pocket-api-token-file)))))

(defun pocket-api--save-access-token (token)
  "Write TOKEN to `pocket-api-auth-file' and set variable."
  (with-temp-file pocket-api-token-file
    (insert (json-encode-alist token)))
  (setq pocket-api--access-token token))

(cl-defun pocket-api--request-token (&key force)
  "Return request token.
If no token exists, or if FORCE is non-nil, get a new token."
  (when (or (not pocket-api--request-token) force)
    (let* ((response (pocket-api--request 'oauth/request
                       :no-auth t :sync t
                       :data (list :redirect_uri "http://www.google.com")))
           (data (request-response-data response))
           (token (alist-get 'code data)))
      (unless token
        (error "Unable to get request token: %s" response))
      (setq pocket-api--request-token token)))
  pocket-api--request-token)

(cl-defun pocket-api--access-token (request-token &key force)
  "Return access token retrieved with REQUEST-TOKEN.
If FORCE is non-nil, get a new token."
  (if (or (null pocket-api--access-token)
          force)
      (progn
        (if (and pocket-api--access-token-have-opened-browser
                 (not force))
            ;; Already authorized in browser; try to get token
            (let ((response (pocket-api--request 'oauth/authorize
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
          (setq pocket-api--access-token-have-opened-browser t)
          (error "Please go to the URL in the clipboard to  authorize the token request, then try again")))))

(defun pocket-api--reset-auth ()
  "Reset all auth variables."
  (setq pocket-api--request-token nil
        pocket-api--access-token nil
        pocket-api--access-token-have-opened-browser nil)
  (with-temp-file pocket-api-token-file
    nil))

;;;;; Methods

(cl-defun pocket-api--request (endpoint &key data sync no-auth)
  "Return request response struct for an API request to \"https://getpocket/com/v3/ENDPOINT\".

ENDPOINT may be a string or symbol, e.g. `get'.  DATA should be a
plist of API parameters.  SYNC is passed to `request''s `:sync'
keyword.

The consumer key and access token are included automatically.

The response body is automatically parsed with `json-read'."
  (declare (indent defun))
  (unless (or pocket-api--access-token no-auth)
    (pocket-api--authorize))
  (let* ((endpoint (cl-typecase endpoint
                     (symbol (symbol-name endpoint))
                     (string endpoint)))
         (url (concat "https://getpocket.com/v3/" endpoint))
         (data (json-encode
                (pocket-api--plist-non-nil
                 (kvplist-merge (list :consumer_key pocket-api-consumer-key
                                      :access_token (alist-get 'access_token
                                                               pocket-api--access-token))
                                data)))))
    (request url
             :type "POST"
             :headers pocket-api-default-extra-headers
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

(cl-defun pocket-api--get (&key (offset 0) (count 10) (detail-type "simple")
                                state favorite tag content-type sort
                                search domain since)
  "Return JSON response for a \"get\" API request.

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
     (pocket-api--request 'get
       :data data :sync t))))

(cl-defun pocket-api--send (actions)
  "Return JSON response for a \"send\" API request containing ACTIONS.
ACTIONS should be a list of actions; this function will convert
it into a vector automatically.

See <https://getpocket.com/developer/docs/v3/modify>."
  (request-response-data
   (pocket-api--request 'send
     :data (list :actions (vconcat actions)) :sync t)))

;;;;; Actions

(defun pocket-api--archive (&rest items)
  "Archive ITEMS."
  ;; FIXME: Needs error handling...maybe.  It does give an error in
  ;; the minibuffer if the API command gives an error.
  (pocket-api--send (--map (list :action "archive"
                                 :item_id (alist-get 'item_id it))
                           items)))

;;;;; Helpers

(defun pocket-api--plist-non-nil (plist)
  "Return PLIST without key-value pairs whose value is nil."
  (cl-loop for (key value) on plist by #'cddr
           unless (null value)
           append (list key value)))

;;;; Footer

(provide 'ap/pocket-api)

;;; pocket-api.el ends here
