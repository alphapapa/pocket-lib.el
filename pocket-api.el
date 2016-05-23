;;; pocket-api.el --- another pocket api -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2016 Free Software Foundation, Inc.

;; Author: DarkSun <lujun9972@gmail.com>
;; Created: 2016-05-23
;; Version: 0.1
;; Keywords: convenience, pocket
;; Package-Requires: ((emacs "24.4") (request "20160424.2032"))
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

;;various mouse-eared items
(defgroup pocket-api nil
  "Pocket"
  :prefix "pocket-api-"
  :group 'external)
(defcustom pocket-api-oauth-request-url "https://getpocket.com/v3/oauth/request"
  "URL to use for OAuth request.")
(defcustom pocket-api-oauth-authorize-url "https://getpocket.com/v3/oauth/authorize"
  "URL to use for OAuth authorization.")
(defvar pocket-api-request-token nil
  "Holds the request token")
(defvar pocket-api-access-token-and-username nil
  "Holds the current access token")
(defvar pocket-api-default-extra-headers '(("Host" . "getpocket.com")
                                           ("Content-Type" . "application/x-www-form-urlencoded; charset=UTF-8")
                                           ("X-Accept" . "application/json"))
  "Default extra headers")

;;no use hiding this I suppose
(defcustom pocket-api-consumer-key "30410-da1b34ce81aec5843a2214f4"
  "API consumer key")

;;access-key and username stored here
(defcustom pocket-api-auth-file (expand-file-name "~/.pocket-api-auth.json")
  "JSON file to store the authorization.")

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

;; the authorization dance:
;; TODO - make a nice interface for this
;; TODO - maybe use the oauth or oauth2 package instead?
;;;###autoload
(defun pocket-api-authorize ()
  (interactive)
  (unless pocket-api-access-token-and-username
    (unless (pocket-api-load-auth)
      (if pocket-api-request-token
          (pocket-api-get-access-token)
        (pocket-api-get-request-token)))))

;; http post helper function
(cl-defun pocket-api--post (url post-data-alist callback &key sync)
  "Post POST-DATA-ALIST to URL and then call the CALLBACK with data decoded as utf-8"
  (request url
           :type "POST"
           :headers pocket-api-default-extra-headers
           :data (request--urlencode-alist post-data-alist) ;若headers中设在了Content-Type，则:data必须为字符串，因为它表示发送给服务器的格式不一定是form表单的格式
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

(defun pocket-api-access-granted-p ()
  "Do we have access yet?"
  pocket-api-access-token-and-username)

;; skeleton function to test getting things from pocket
;; response is printed to *Messages*
;; TODO make this do useful things
(cl-defun pocket-api-get (&key (offset 1) (count 10))
  "Gets things from your pocket."
  (if (pocket-api-access-granted-p)
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
                                                   :sync t)))
    (pocket-api-authorize)))
;; (pocket-api-get)

;;oh my gosh
(defun pocket-api-add (url-to-add)
  "Add URL-TO-ADD to your pocket."
  (interactive
   (list
    (read-string "pocket-api url: ")))
  (if (pocket-api-access-granted-p)
      (pocket-api--post  "https://getpocket.com/v3/add"
                        `(("consumer_key" . ,pocket-api-consumer-key)
                          ("access_token" . ,(cdr (assoc 'access_token pocket-api-access-token-and-username)))
                          ("url" . ,url-to-add))
                        (lambda (data)
                          data))
    (pocket-api-authorize)))

(provide 'pocket-api)

;;; pocket-api.el ends here
