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
(require 'cl-lib)
(require 'json)
(require 'request)

(require 'kv)

;;various mouse-eared items
(defgroup pocket-api nil
  "Pocket"
  :prefix "pocket-api-"
  :group 'external)
(defvar pocket-api-oauth-request-url "https://getpocket.com/v3/oauth/request"
  "URL to use for OAuth request.")
(defvar pocket-api-oauth-authorize-url "https://getpocket.com/v3/oauth/authorize"
  "URL to use for OAuth authorization.")
(defvar pocket-api-request-token nil
  "Holds the request token")
(defvar pocket-api-access-token-and-username nil
  "Holds the current access token")
(defvar pocket-api-default-extra-headers '(("Host" . "getpocket.com")
                                           ("Content-Type" . "application/json; charset=UTF-8")
                                           ("X-Accept" . "application/json"))
  "Default extra headers")

;;no use hiding this I suppose
(defcustom pocket-api-consumer-key "30410-da1b34ce81aec5843a2214f4"
  "API consumer key"
  :group 'pocket-api
  :type 'string)

;;access-key and username stored here
(defcustom pocket-api-auth-file (expand-file-name "~/.pocket-api-auth.json")
  "JSON file to store the authorization."
  :group 'point-api
  :type 'file)

(defun pocket-api-load-auth (&optional auth-file)
  (let ((auth-file (or auth-file
                       pocket-api-auth-file)))
    (when (file-readable-p auth-file)
      (setq pocket-api-access-token-and-username (json-read-file auth-file)))))

(defun pocket-api-save-auth (token-and-username auth-file)
  (with-temp-file auth-file
    (insert (json-encode-alist token-and-username))))

;;;###autoload
(defun pocket-api-clear-auth ()
  (interactive)
  (setq pocket-api-request-token nil)
  (setq pocket-api-access-token-and-username nil))

(defun pocket-api-access-granted-p ()
  "Do we have access yet?"
  pocket-api-access-token-and-username)

;; the authorization dance:
;; TODO - make a nice interface for this
;; TODO - maybe use the oauth or oauth2 package instead?
;;;###autoload
(defun pocket-api-authorize ()
  (interactive)
  (unless (pocket-api-access-granted-p)
    (unless (pocket-api-load-auth)
      (if pocket-api-request-token
          (pocket-api-get-access-token)
        (pocket-api-get-request-token)))))

;; http post helper function
(cl-defun pocket-api--post (url post-data-json callback &key sync)
  "Post POST-DATA-ALIST to URL and then call the CALLBACK with data decoded as utf-8"
  (request url
           :type "POST"
           :headers pocket-api-default-extra-headers
           :data (json-encode post-data-json) ;若headers中设在了Content-Type，则:data必须为字符串，因为它表示发送给服务器的格式不一定是form表单的格式
           :sync sync
           :parser (lambda ()
                     (json-read-from-string (decode-coding-string (buffer-string) 'utf-8)))
           :success (cl-function
                     (lambda (&key data &allow-other-keys)
                       (funcall callback data)))))

;; once the request token is a-gotten,
;; and you've gone to the oauth/authorize page
;; and and done that, this will then get the
;; all-important access-token, huzzah!
(defun pocket-api-get-access-token ()
  "After authorizing, pocket-api-authorize again to call this and get an access-token."
  (pocket-api--post pocket-api-oauth-authorize-url
                   `(("consumer_key" . ,pocket-api-consumer-key)
                     ("code" . ,pocket-api-request-token))
                   (lambda (data)
                     (setq pocket-api-access-token-and-username data)
                     (pocket-api-save-auth pocket-api-access-token-and-username
                                           pocket-api-auth-file)
                     (display-message-or-buffer
                      "access a-gotten!"))))

;; we don't have a request token yet, so request
;; one, then send the user to oauth/authorize for
;; to authorize this shiz
(defun pocket-api-get-request-token ()
  "Request a request token, then direct the user to authorization URL"
  (pocket-api--post pocket-api-oauth-request-url
                   `(("consumer_key" . ,pocket-api-consumer-key)
                     ("redirect_uri" . "http://www.google.com" ))
                   (lambda (data)
                     (let* ((token (cdr (assoc 'code data)))
                            (url (concat "https://getpocket.com/auth/authorize?request_token=" token)))
                       (setq pocket-api-request-token token)
                       (kill-new url)
                       (display-message-or-buffer
                        (concat "authorize pocket-api at " url
                                " (copied to clipboard)\n"))
                       (browse-url url))
                     ;; (pocket-api-authorize)
                     )))

;;;###autoload
(cl-defun pocket-api-get (&key (offset 1) (count 10))
  "Gets things from your pocket."
  (unless (pocket-api-access-granted-p)
    (pocket-api-authorize))
  (let ((offset (number-to-string offset))
        (count (number-to-string count)))
    (request-response-data  (pocket-api--post  "https://getpocket.com/v3/get"
                                               `(("consumer_key" . ,pocket-api-consumer-key)
                                                 ("access_token" . ,(cdr (assoc 'access_token pocket-api-access-token-and-username)))
                                                 ("offset" . ,offset)
                                                 ("count" . ,count)
                                                 ("detailType" . "simple"))
                                               (lambda (data)
                                                 data)
                                               :sync t))))

;;;###autoload
(defun pocket-api-add (url-to-add)
  "Add URL-TO-ADD to your pocket."
  (interactive (list (read-string "pocket-api url: ")))
  (unless (pocket-api-access-granted-p)
    (pocket-api-authorize))
  (pocket-api--post  "https://getpocket.com/v3/add"
                     `(("consumer_key" . ,pocket-api-consumer-key)
                       ("access_token" . ,(cdr (assoc 'access_token pocket-api-access-token-and-username)))
                       ("url" . ,url-to-add))
                     (lambda (data)
                       data)))

(defun pocket-api-send-basic-action (action item_id)
  "Modify the item which specified by ITEM-ID.

ACTION only support basic actions which means add,archive,readd,favorite,unfavorite,delete"
  (unless (pocket-api-access-granted-p)
    (pocket-api-authorize))
  (let ((actions (vector `((action . ,action)
                           (item_id . ,item_id)))))
    (request-response-data (pocket-api--post "https://getpocket.com/v3/send"
                                             `(("consumer_key" . ,pocket-api-consumer-key)
                                               ("access_token" . ,(cdr (assoc 'access_token pocket-api-access-token-and-username)))
                                               ("actions" . ,actions))
                                             (lambda (data)
                                               data)))))

;;;###autoload
(defun pocket-api-archive (item_id)
  "Archive item which specified by ITEM_ID"
  (interactive (list (read-number "pocket-api item_id: ")))
  (pocket-api-send-basic-action 'archive item_id))

;;;###autoload
(defun pocket-api-readd (item_id)
  "Readd item which specified by ITEM_ID"
  (interactive (list (read-number "pocket-api item_id: ")))
  (pocket-api-send-basic-action 'readd item_id))

;;;###autoload
(defun pocket-api-favorite (item_id)
  "Favorite item which specified by ITEM_ID"
  (interactive (list (read-number "pocket-api item_id: ")))
  (pocket-api-send-basic-action 'favorite item_id))

;;;###autoload
(defun pocket-api-unfavorite (item_id)
  "Unfavorite item which specified by ITEM_ID"
  (interactive (list (read-number "pocket-api item_id: ")))
  (pocket-api-send-basic-action 'unfavorite item_id))

;;;###autoload
(defun pocket-api-delete (item_id)
  "Delete item which specified by ITEM_ID"
  (interactive (list (read-number "pocket-api item_id: ")))
  (pocket-api-send-basic-action 'delete item_id))


;; (dolist (action '("archive" "readd" "favorite" "unfavorite" "delete"))
;;   (let ((fn-symbol (intern (format "pocket-api-%s" action))))
;;     (fset fn-symbol (lambda (item_id)
;;                       (pocket-api-send-basic-action action item_id)))))

;; (pocket-api-get)
;; (pocket-api-send-basic-action 'readd 271799625)

(cl-defun pocket-api--request (endpoint &key data sync)
  "Return request response struct for an API request to \"https://getpocket/com/v3/ENDPOINT\".

ENDPOINT may be a string or symbol, e.g. `get'.  DATA should be a
plist of API parameters.  SYNC is passed to `request''s `:sync'
keyword.

The consumer key and access token are included automatically.

The response body is automatically parsed with `json-read'."
  (declare (indent defun))
  (unless (pocket-api-access-granted-p)
    (pocket-api-authorize))
  (let ((endpoint (cl-typecase endpoint
                    (symbol (symbol-name endpoint))
                    (string endpoint)))
        (data (pocket-api--plist-non-nil
               (kvplist-merge (list :consumer_key pocket-api-consumer-key
                                    :access_token (alist-get 'access_token
                                                             pocket-api-access-token-and-username))
                              data))))
    (request (concat "https://getpocket.com/v3/" endpoint)
             :type "POST"
             :headers pocket-api-default-extra-headers
             :data (json-encode data)
             :sync sync
             :parser #'json-read
             :success (cl-function
                       (lambda (&key data &allow-other-keys)
                         data)))))

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

;;;;; Helpers

(defun pocket-api--plist-non-nil (plist)
  "Return PLIST without key-value pairs whose value is nil."
  (cl-loop for (key value) on plist by #'cddr
           unless (null value)
           append (list key value)))

;;;; Footer

(provide 'pocket-api)

;;; pocket-api.el ends here
