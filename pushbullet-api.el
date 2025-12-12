;;; pushbullet-api.el --- Pushbullet client for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Savio Sena <savio.sena@gmail.com>

;; Author: Savio Sena <savio.sena@gmail.com>
;; Version: 1.0.1
;; Package-Requires: ((emacs "29.1") (json "1.5") (request "0.3.3"))
;; Keywords: pushbullet, client, tool, internet
;; URL: https://github.com/sav/emacs-pushbullet

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;; This package provides the REST API components for the Emacs
;;; Pushbullet client.
;;;

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'json)
(require 'request)

(defgroup pushbullet-api nil
  "Client for the Pushbullet REST API."
  :group 'extensions)

(defcustom pushbullet-api-token nil
  "Your personal Pushbullet API access token.
This token is required for authentication with the Pushbullet API. You
can obtain your access token from the Pushbullet account settings page:
`https://www.pushbullet.com/#settings/account`."
  :type 'string
  :group 'pushbullet-api)

(defcustom pushbullet-api-limit 20
  "The maximum number of pushes to fetch in a single API request for
 pagination."
  :type 'integer
  :group 'pushbullet-api)

(defcustom pushbullet-api-debug nil
   "Enable verbose logging for Pushbullet API operations.
When non-nil, additional debug messages will be printed to the *Messages* buffer."
   :type 'boolean
   :group 'pushbullet-api)

(defvar pushbullet-api-url "https://api.pushbullet.com/v2"
   "The base URL for all Pushbullet API v2 endpoints.")

(defvar-local pushbullet-api-cursor nil
   "A buffer-local string used for pagination in Pushbullet API requests,
 indicating the point from which to fetch subsequent pushes.")

(defmacro pushbullet-api--log (fmt &rest args)
  "Log a debug message with FMT and ARGS when `pushbullet-debug' is
 enabled.
The message is prefixed with '[pushbullet]' for identification."
  `(when pushbullet-api-debug
     (message (concat "[pushbullet-api] " ,fmt) ,@args)))

(defun pushbullet-api--check-token ()
  "Ensures that the `pushbullet-api-token' is set, either directly or by
 retrieving it from `auth-source'.
If the token is not found, an error is signaled prompting the user to
set it."
  pushbullet-api-token
  (unless pushbullet-api-token
    (let ((auth-source-token (auth-source-pick-first-password :host "pushbullet.com")))
      (if auth-source-token
          (setq pushbullet-api-token auth-source-token)
        (error "Please set your Pushbullet token with M-x customize-variable RET pushbullet-api-token"))))
  pushbullet-api-token)

(defun pushbullet-api-request (method endpoint data callback &optional error-callback)
  "Makes an asynchronous HTTP request to the Pushbullet API.

METHOD is a string representing the HTTP method (e.g., 'GET', 'POST', 'DELETE').
ENDPOINT is a string specifying the API endpoint relative to `pushbullet-api-url`.
DATA is an optional alist of request data to be sent as JSON.
CALLBACK is a function to be called upon successful API response, receiving the parsed JSON data.
ERROR-CALLBACK is an optional function to be called if the API request encounters an error.

This function automatically includes the `pushbullet-api-token' for
authentication and handles JSON encoding/decoding."
  (pushbullet-api--check-token)
  (let ((url (concat pushbullet-api-url endpoint))
        (headers `(("Access-Token" . ,pushbullet-api-token)
                   ("Content-Type" . "application/json"))))
    (request url
      :type method
      :headers headers
      :data (when data (json-encode data))
      :parser 'json-read
      :success callback
      :error (or error-callback
                 (cl-function
                  (lambda (&key error-thrown &allow-other-keys)
                    (message "Pushbullet API error: %s" error-thrown)))))))

(defun pushbullet-api--fetch-url (&optional limit)
  "Constructs the API endpoint for fetching pushes, incorporating
 `CURSOR' for pagination.
If CURSOR is `nil', it fetches the initial set of pushes. Otherwise, it
fetches subsequent pushes using the provided CURSOR value and
`pushbullet-api-limit`.
Then the optional argument LIMIT is provided, fetch at most LIMIT items."
  (let* ((n (or limit pushbullet-api-limit)))
    (if pushbullet-api-cursor
        (format "/pushes?limit=%d&cursor=%s" n pushbullet-api-cursor)
      (format "/pushes?limit=%d" n))))

(defun pushbullet-api-active (push)
  "Returns true if PUSH has data and should be displayed. Returns `nil'
 otherwise."
  (let* ((active (alist-get 'active push))
         (title (alist-get 'title push))
         (url (alist-get 'url push))
         (body (alist-get 'body push)))
    (and (and active (not (eq active :json-false)))
         (or (not (string-empty-p title))
             (not (string-empty-p url))
             (not (string-empty-p body))))))

(defun pushbullet-api-fetch (callback &optional limit)
  "Fetches Pushbullet pushes from the API.
It uses `pushbullet-api-cursor' for pagination to fetch subsequent sets of
pushes. Upon successful retrieval, the fetched pushes are filtered,
`pushbullet-api-cursor' is updated, and CALLBACK is invoked with the
filtered pushes. When optional argument LIIMIT is provided, fetch at
most LIMIT items."
  (pushbullet-api-request
   "GET" (pushbullet-api--fetch-url limit) nil
   (cl-function
    (lambda (&key data &allow-other-keys)
      (let* ((pushes (alist-get 'pushes data))
             (cursor (alist-get 'cursor data)))
        (pushbullet-api--log "Received %S pushes" (length pushes))
        (setq pushbullet-cursor cursor)
        (funcall callback (cl-coerce pushes 'list)))))))

(defun pushbullet-api-delete (push)
  "Deletes the specified PUSH (an alist containing at least an 'iden
 field) from the Pushbullet server.
Upon successful deletion, a debug message is logged, and the Pushbullet
UI is implicitly refreshed by `pushbullet` being called."
  (let ((id (alist-get 'iden push)))
    (pushbullet-api-request
     "DELETE" (format "/pushes/%s" id) nil
     (cl-function
      (lambda (&key data &allow-other-keys)
        (pushbullet-api--log "Push deleted: %S" push))))))

(defun pushbullet-api-send (title body &optional url)
  (let ((push `((type . ,"note")
                (title . ,title)
                (body . ,body))))
    (when url (push '(url . url) push))
    (pushbullet-api--log "Pushing: %S" push)
    (pushbullet-api-request
     "POST" "/pushes" push
     (cl-function
      (lambda (&key data &allow-other-keys)
        (pushbullet-api--log "Pushed: %s" data))))))

(provide 'pushbullet-api)

;;; pushbullet-api.el ends here
