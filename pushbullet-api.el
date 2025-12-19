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
This token is required for authentication with the Pushbullet API.
You can obtain your access token from the Pushbullet account settings page:
`https://www.pushbullet.com/#settings/account`."
  :type 'string
  :group 'pushbullet-api)

(defcustom pushbullet-api-limit 20
  "The maximum number of pushes to fetch in a single API request for pagination."
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
  "A buffer-local string used for pagination in Pushbullet API requests, indicating the point from which to fetch subsequent pushes.")

(defmacro pushbullet-api--log (fmt &rest args)
  "Logs a debug message with FMT and ARGS when `pushbullet-api-debug' is enabled.
The message is prefixed with '[pushbullet-api]' for identification."
  `(when pushbullet-api-debug
     (message (concat "[pushbullet-api] " ,fmt) ,@args)))

(defun pushbullet-api--check-token ()
  "Ensures that `pushbullet-api-token' is set, either directly or by
retrieving it from `auth-source'. If the token is not found, return `nil'."
  (or (stringp pushbullet-api-token)
      (setq pushbullet-api-token (auth-source-pick-first-password :host "pushbullet.com"))))

(defun pushbullet-api-request (method endpoint data callback &optional error-callback)
  "Makes an asynchronous HTTP request to the Pushbullet API.

METHOD is a string representing the HTTP method (e.g., 'GET', 'POST', 'DELETE').
ENDPOINT is a string specifying the API endpoint relative to `pushbullet-api-url'.
DATA is an optional alist of request data to be sent as JSON.
CALLBACK is a function to be called upon a successful API response, receiving the parsed JSON data.
ERROR-CALLBACK is an optional function to be called if the API request encounters an error.

This function automatically includes `pushbullet-api-token' for authentication and handles JSON encoding/decoding."
  (unless (pushbullet-api--check-token)
    (error "Please set your Pushbullet token with M-x customize-variable RET pushbullet-api-token"))
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
  "Construct API endpoint string for fetching pushes with pagination.

Returns a string representing the Pushbullet API URL path.

If `pushbullet-api-cursor' is nil, fetches initial pushes.
Otherwise, uses cursor for pagination with `pushbullet-api-limit'.
Optional LIMIT overrides the default limit.

API documentation: https://docs.pushbullet.com/#list-pushes

The endpoint returns a JSON object with structure:
`((cursor . <string>)
  (pushes . [((iden . <string>)
              (type . <string>)
              (title . <string>)
              (body . <string>)
              (created . <number>)
              (modified . <number>)
              (active . <boolean>)) ...]))"
  (apply #'format
	 (let ((n (or limit pushbullet-api-limit)))
           (if pushbullet-api-cursor
               `("/pushes?limit=%d&cursor=%s" ,n ,pushbullet-api-cursor)
             `("/pushes?limit=%d" ,n)))))

(defun pushbullet-api-active (push)
  "Returns true if PUSH has data and should be displayed; otherwise, returns `nil`."
  (let ((active (alist-get 'active push)))
    (and (and active (not (eq active :json-false)))
         (or (not (string-empty-p (alist-get 'title push)))
             (not (string-empty-p (alist-get 'url push)))
             (not (string-empty-p (alist-get 'body push)))))))


(defun pushbullet-api-fetch (callback &optional limit)
  "Fetch Pushbullet pushes from the API and invoke CALLBACK with results.

Returns nil (asynchronous operation).

Uses `pushbullet-api-cursor' for pagination to fetch subsequent sets.
Upon successful retrieval, updates `pushbullet-api-cursor' and invokes
CALLBACK with a list of push alists.
Optional LIMIT overrides the default number of items to fetch.

API documentation: https://docs.pushbullet.com/#list-pushes

CALLBACK receives a list of push alists with structure:
`(((iden . <string>)
   (type . <string>)
   (title . <string>)
   (body . <string>)
   (created . <number>)
   (modified . <number>)
   (active . <boolean>))
  ...)"
  (pushbullet-api-request
   "GET" (pushbullet-api--fetch-url limit) nil
   (cl-function
    (lambda (&key data &allow-other-keys)
      (let ((pushes (alist-get 'pushes data))
             (cursor (alist-get 'cursor data)))
        (pushbullet-api--log "Received %S pushes" (length pushes))
        (setq pushbullet-cursor cursor)
        (funcall callback (cl-coerce pushes 'list)))))))

(defun pushbullet-api-delete (push)
  "Deletes the specified PUSH (an alist containing at least an 'iden field) from the Pushbullet server.
Upon successful deletion, a debug message is logged, and the Pushbullet UI is implicitly refreshed by `pushbullet` being called."
  (let ((id (alist-get 'iden push)))
    (pushbullet-api-request
     "DELETE" (format "/pushes/%s" id) nil
     (cl-function
      (lambda (&key data &allow-other-keys)
        (pushbullet-api--log "Push deleted: %S" push))))))

(defun pushbullet-api-send (title body &optional url)
  "Sends a push to the Pushbullet API.

TITLE is a string specifying the title of the push.
BODY is a string specifying the main content of the push.
URL is an optional string specifying a URL to be included, transforming
the push into a link."
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
