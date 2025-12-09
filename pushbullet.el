;;; pushbullet.el --- Pushbullet client for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Savio Sena <savio.sena@gmail.com>

;; Author: Savio Sena <savio.sena@gmail.com>
;; Version: 1.0.1
;; Package-Requires: ((emacs "29.1") (request "0.3.3") (all-the-icons "5.0.0"))
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

;;;; This package provides a comprehensive Emacs client for the Pushbullet service.
;;; It enables users to:
;;; - Send various types of pushes (notes, links) from Emacs.
;;; - Push selected text regions or clipboard contents directly to Pushbullet.
;;; - Browse, add, delete, and send Pushbullet pushes within a dedicated Emacs UI.
;;; - Export pushes to Org-mode format.

;;; Usage Examples:

;;;   `M-x pushbullet`           : Opens or switches to the main Pushbullet UI buffer.
;;;   `M-x pushbullet-send`      : Prompts for a title and text to send a new note push.
;;;   `M-x pushbullet-send-text` : Prompts for text to send a new note push, using a default title.
;;;   `M-x pushbullet-region`    : Sends the active region's content as a note push.
;;;   `M-x pushbullet-yank`      : Sends the latest kill-ring entry (clipboard content) as a note push.
;;;   `M-x pushbullet-export`    : Exports currently fetched pushes to an Org-mode buffer.

;;; Code:

(require 'request)
(require 'json)
(require 'auth-source)
(require 'pushbullet-ui)
(require 'cl-lib)

(defconst pushbullet-version "1.0.1"
   "The current version string of the Pushbullet Emacs package.")

(defgroup pushbullet nil
   "Client for the Pushbullet service, providing integration with Emacs."
   :version pushbullet-version
   :prefix "pushbullet-"
   :group 'applications)

(defcustom pushbullet-token nil
  "Your personal Pushbullet API access token.
This token is required for authentication with the Pushbullet API. You
can obtain your access token from the Pushbullet account settings page:
`https://www.pushbullet.com/#settings/account`."
  :type 'string
  :group 'pushbullet)

(defcustom pushbullet-limit 20
   "The maximum number of pushes to fetch in a single API request for
 pagination."
   :type 'integer
   :group 'pushbullet)

(defcustom pushbullet-default-title (format "GNU Emacs %s" emacs-version)
  "The default title string used for new pushes when no explicit title
 is provided.
It is formatted to include the current Emacs version."
  :type 'string
  :group 'pushbullet)

(defcustom pushbullet-debug nil
  "Enable verbose logging for Pushbullet operations.
When non-nil, additional debug messages will be printed to the
*Messages* buffer."
  :type 'boolean
  :group 'pushbullet
  :initialize 'custom-initialize-default)

(defvar pushbullet-api-url "https://api.pushbullet.com/v2"
   "The base URL for all Pushbullet API v2 endpoints.")

(defvar pushbullet-buffer "*Pushbullet*"
  "The name of the main buffer where the Pushbullet user interface is
 displayed.")

(defvar pushbullet-export-buffer "*Pushbullet Export*"
   "The name of the buffer used for exporting Pushbullet pushes to
 Org-mode format.")

(defvar-local pushbullet-cursor nil
   "A buffer-local string used for pagination in Pushbullet API requests,
 indicating the point from which to fetch subsequent pushes.")

(defmacro pushbullet--log (fmt &rest args)
  "Log a debug message with FMT and ARGS when `pushbullet-debug' is
 enabled.
The message is prefixed with '[pushbullet]' for identification."
  `(when pushbullet-debug
     (message (concat "[pushbullet] " ,fmt) ,@args)))

(defun pushbullet--check-token ()
  "Ensures that the `pushbullet-token' is set, either directly or by
 retrieving it from `auth-source'.
If the token is not found, an error is signaled prompting the user to
set it."
  pushbullet-token
  (unless pushbullet-token
    (let ((auth-source-token (auth-source-pick-first-password :host "pushbullet.com")))
      (if auth-source-token
          (setq pushbullet-token auth-source-token)
        (error "Please set your Pushbullet token with M-x customize-variable RET pushbullet-token"))))
  pushbullet-token)

(defun pushbullet--request (method endpoint data callback &optional error-callback)
  "Makes an asynchronous HTTP request to the Pushbullet API.

METHOD is a string representing the HTTP method (e.g., 'GET', 'POST', 'DELETE').
ENDPOINT is a string specifying the API endpoint relative to `pushbullet-api-url`.
DATA is an optional alist of request data to be sent as JSON.
CALLBACK is a function to be called upon successful API response, receiving the parsed JSON data.
ERROR-CALLBACK is an optional function to be called if the API request encounters an error.

This function automatically includes the `pushbullet-token' for
authentication and handles JSON encoding/decoding."
  (pushbullet--check-token)
  (let ((url (concat pushbullet-api-url endpoint))
        (headers `(("Access-Token" . ,pushbullet-token)
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

(defun pushbullet--next-endpoint (&optional limit)
  "Constructs the API endpoint for fetching pushes, incorporating
 `CURSOR' for pagination.
If CURSOR is `nil', it fetches the initial set of pushes. Otherwise, it
fetches subsequent pushes using the provided CURSOR value and
`pushbullet-limit`.
Then the optional argument LIMIT is provided, fetch at most LIMIT items."
  (let* ((n (or limit pushbullet-limit)))
    (if pushbullet-cursor
        (format "/pushes?limit=%d&cursor=%s" n pushbullet-cursor)
      (format "/pushes?limit=%d" n))))

(defun pushbullet-active (push)
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

(defun pushbullet-fetch (callback &optional limit)
  "Fetches Pushbullet pushes from the API.
It uses `pushbullet-cursor' for pagination to fetch subsequent sets of
pushes. Upon successful retrieval, the fetched pushes are filtered,
`pushbullet-cursor' is updated, and CALLBACK is invoked with the
filtered pushes. When optional argument LIIMIT is provided, fetch at
most LIMIT items."
  (pushbullet--request
   "GET" (pushbullet--next-endpoint limit) nil
   (cl-function
    (lambda (&key data &allow-other-keys)
      (let* ((pushes (alist-get 'pushes data))
             (cursor (alist-get 'cursor data)))
        (pushbullet--log "Received %S pushes" (length pushes))
        (setq pushbullet-cursor cursor)
        (funcall callback (cl-coerce pushes 'list)))))))

(defun pushbullet-delete (push)
  "Deletes the specified PUSH (an alist containing at least an 'iden
 field) from the Pushbullet server.
Upon successful deletion, a debug message is logged, and the Pushbullet
UI is implicitly refreshed by `pushbullet` being called."
  (let ((id (alist-get 'iden push)))
    (pushbullet--request
     "DELETE" (format "/pushes/%s" id) nil
     (cl-function
      (lambda (&key data &allow-other-keys)
        (pushbullet--log "Push deleted: %S" push))))))

;;;###autoload
(defun pushbullet-send (title body &optional url)
  "Sends a note push to Pushbullet with the given TITLE and BODY.
An optional URL can be provided to create a link push instead of a
simple note.

TITLE is a string specifying the title of the push.
BODY is a string specifying the main content of the push.
URL is an optional string specifying a URL to be included, transforming
the push into a link.

This function is interactive, prompting the user for TITLE and BODY if
called without arguments."
  (interactive "sTitle: \nsText: ")
  (let ((push `((type . ,"note")
                (title . ,title)
                (body . ,body))))
    (when url (push '(url . url) push))
    (pushbullet--log "Pushing: %S" push)
    (pushbullet--request
     "POST" "/pushes" push
     (cl-function
      (lambda (&key data &allow-other-keys)
        (pushbullet--log "Pushed: %s" data))))))

;;;###autoload
(defun pushbullet-send-text (text)
  "Sends a note push to Pushbullet with the provided TEXT.
The title for the push is automatically set using the
`pushbullet-default-title' variable.

TEXT is a string representing the content of the note push.

This function is interactive, prompting the user for the TEXT content if
called without arguments."
  (interactive "sText: ")
  (pushbullet-send pushbullet-default-title text))

;;;###autoload
(defun pushbullet-region (start end)
  "Sends the content of the currently active region to Pushbullet as a
 note push.
The text between START and END is used as the body, and the current
buffer's name is used as the title.

START and END are buffer positions defining the region.

This function is interactive and requires an active region to be
selected."
  (interactive "r")
  (unless (use-region-p)
    (error "No region selected"))
  (let ((text (buffer-substring-no-properties start end)))
    (pushbullet-send (buffer-name) text)))

;;;###autoload
(defun pushbullet-yank ()
  "Sends the latest entry from the Emacs kill-ring (clipboard) to
 Pushbullet as a note push.
The kill-ring content is used as the body, and the title is set to
`pushbullet-default-title'.

This function is interactive and will signal an error if the kill-ring is empty."
  (interactive)
  (let ((text (current-kill 0)))
    (unless text
      (error "Kill ring is empty"))
    (pushbullet-send pushbullet-default-title text)))

;;;###autoload
(defun pushbullet-export (&optional pushes)
  "Exports a list of PUSHES to a new Org-mode buffer named
 `pushbullet-export-buffer'.

Each active push is formatted as an Org-mode heading, including its
title, URL (if present), and body.

If PUSHES is `nil` or the function is called interactively, it exports
the currently displayed pushes from the UI (`pushbullet-ui--pushes`).

This function is interactive."
  (interactive)
  (let ((buffer (get-buffer-create pushbullet-export-buffer))
    	(pushes (or pushes pushbullet-ui--pushes)))
    (with-current-buffer buffer
      (remove-overlays)
      (erase-buffer)
      (insert "#+TITLE: Pushbullet Export\n\n")
      (mapc
       (lambda (push)
         (let ((active (alist-get 'active push))
               (title  (alist-get 'title push))
               (body   (alist-get 'body push))
               (url    (alist-get 'url push)))
           (when active
             (if title (insert (format "* %s\n" title))
               (insert "* "))
             (when url (insert (format "[[%s]]\n" url)))
             (if body (insert (format "%s\n" body))
               (insert "<empty>\n")))))
       pushes)
      (goto-char (point-min))
      (org-mode))
    (switch-to-buffer buffer)))

(defconst pushbullet-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-e") #'pushbullet-ui--export-all)
    (define-key map (kbd "C-c C-u") #'pushbullet-ui--load-more)
    (define-key map (kbd "C-c C-o") #'browse-url-at-point)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `pushbullet-mode', defining keybindings for interacting
 with the Pushbullet UI.

- `C-c C-e': Calls `pushbullet-ui--export-all` to export pushes to Org-mode.
- `C-c C-u': Calls `pushbullet-ui--load-more` to fetch more pushes.
- `C-c C-o': Calls `browse-url-at-point` to open a URL at the current cursor position.
- `q': Calls `quit-window` to close the Pushbullet UI buffer.")

(defvar pushbullet-api '((active . pushbullet-active)
                         (fetch . pushbullet-fetch)
                         (send . pushbullet-send)
                         (del . pushbullet-delete)
                         (export . pushbullet-export))

  "An alist of callback functions that map Pushbullet API operations to
 their corresponding backend functions.
This alist is passed to the Pushbullet UI to facilitate interaction with
the API.

- `active': Function to check if a push is active (`pushbullet-active').
- `fetch': Function to retrieve pushes (`pushbullet-fetch').
- `send': Function to create and send a new push (`pushbullet-send').
- `del': Function to remove a push (`pushbullet-delete').
- `export': Function to export the current pushes to Org-mode (`pushbullet-export').")

;;;###autoload
(defun pushbullet ()
  "Opens or switches to the main Pushbullet UI buffer (`pushbullet-buffer').
This function initializes the Pushbullet UI by setting up buffer-local
variables, configuring `pushbullet-mode', and fetching the latest pushes
from the Pushbullet API. If the buffer already exists, it is
re-initialized and updated to reflect the current state.

This function is interactive."
  (interactive)
  (when (null (get-buffer pushbullet-buffer))
    (let ((buffer (get-buffer-create pushbullet-buffer)) 
	  (title (format "Pushbullet %s" pushbullet-version)))
      (with-current-buffer buffer
        (kill-all-local-variables)
        (remove-overlays)
        (erase-buffer)
        (setq pushbullet-cursor nil)
        (pushbullet-ui title pushbullet-buffer pushbullet-api pushbullet-mode-map))))
  (switch-to-buffer-other-window (get-buffer pushbullet-buffer))
  nil)

(provide 'pushbullet)

;;; pushbullet.el ends here
