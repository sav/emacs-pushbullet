;;; pushbullet.el --- Pushbullet client for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Savio Sena <savio.sena@gmail.com>

;; Author: Savio Sena <savio.sena@gmail.com>
;; Version: 1.0.0
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

;; This package provides Pushbullet integration for Emacs, allowing you to:
;; - Send notes to Pushbullet
;; - Push text regions and clipboard contents
;; - Browse and manage your Pushbullet pushes in a dedicated UI buffer

;; Usage:

;;   M-x pushbullet            Open the Pushbullet buffer
;;   M-x pushbullet-send       Send a note with prompted title and text
;;   M-x pushbullet-send-text  Send a note with prompted text
;;   M-x pushbullet-region     Send the current region
;;   M-x pushbullet-yank       Send the current kill-ring contents

;;; Code:

(require 'request)
(require 'json)
(require 'button)
(require 'all-the-icons)
(require 'auth-source)

(defconst pushbullet-version "1.0.0"
  "Version of the Pushbullet client")

(defgroup pushbullet nil
  "Pushbullet client."
  :version pushbullet-version
  :prefix "pushbullet-"
  :group 'applications)

(defcustom pushbullet-token nil
  "Your Pushbullet API access token.
You can obtain this from https://www.pushbullet.com/#settings/account"
  :type 'string
  :group 'pushbullet)

(defcustom pushbullet-limit 20
  "Number of pushes to fetch per request."
  :type 'integer
  :group 'pushbullet)

(defcustom pushbullet-default-title (format "GNU Emacs %s" emacs-version)
  "Default title used when sending a push without an explicit title."
  :type 'string
  :group 'pushbullet)

(defcustom pushbullet-columns 80
  "Maximum number of columns used to wrap lines in Pushbullet message display."
  :type 'integer
  :group 'pushbullet
  :initialize 'custom-initialize-default)

(defcustom pushbullet-debug nil
  "Enable verbose logging for Pushbullet operations.
When non-nil, additional debug messages will be printed to the *Messages* buffer."
  :type 'boolean
  :group 'pushbullet
  :initialize 'custom-initialize-default)

(defvar pushbullet-api "https://api.pushbullet.com/v2"
  "Base URL for Pushbullet API.")

(defvar pushbullet-buffer "*Pushbullet*"
  "Name of the Pushbullet UI buffer.")

(defvar-local pushbullet-cursor nil
  "Cursor for pagination, returned by and used in the Pushbullet API.")

(defvar-local pushbullet-content-start-marker nil
  "Marker indicating the beginning of the content.")

(defvar-local pushbullet-prompt-start-marker nil
  "Marker indicating the beginning of the message prompt.")

(defvar-local pushbullet-prompt-end-marker nil
  "Marker indicating the end of the message prompt.")

(defmacro pushbullet--log (fmt &rest args)
  "Log a debug message with FMT and ARGS when `pushbullet-debug' is enabled.
The message is prefixed with '[pushbullet]' for identification."
  `(when pushbullet-debug
     (message (concat "[pushbullet] " ,fmt) ,@args)))

(defun pushbullet--check-token ()
  "Check if the Pushbullet token is configured."
  (unless pushbullet-token
    (let ((auth-source-token (auth-source-pick-first-password :host "pushbullet.com")))
      (if auth-source-token
          (setq pushbullet-token auth-source-token)
        (error "Please set your Pushbullet token with M-x customize-variable RET pushbullet-token"))))
  pushbullet-token)

(defun pushbullet--request (method endpoint data callback &optional error-callback)
  "Make a request to the Pushbullet API.
METHOD is the HTTP method (GET, POST, etc.).
ENDPOINT is the API endpoint.
DATA is the request data.
CALLBACK is called on success.
ERROR-CALLBACK is called on error."
  (pushbullet--check-token)
  (let ((url (concat pushbullet-api endpoint))
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

(defun pushbullet--format-push (item)
  "Format a Pushbullet push ITEM for display in the UI buffer.
Returns a formatted string with timestamp, sender info, title, and body."
  (let* ((created (seconds-to-time (alist-get 'created item)))
         (datetime (propertize (format " ( %s %s  %s %s ) %s"
                                       (all-the-icons-faicon "calendar")
                                       (format-time-string "%Y-%b-%d" created)
                                       (all-the-icons-faicon "clock-o")
                                       (format-time-string "%H:%M" created)
                                       (make-string 4 ?━))
                               'face 'shadow))
         (separator1 (propertize (make-string (- pushbullet-columns (length datetime) 3) ?━)
                                 'face 'shadow))
         (separator2 (propertize (make-string pushbullet-columns ?━) 'face 'shadow))
         (sender-name (alist-get 'sender_name item))
         (sender-email (alist-get 'sender_email item))
         (sender (format "%s %s %s\n"
                         (propertize "   From:" 'face 'shadow)
                         (all-the-icons-faicon "user")
                         (propertize (format "%s <%s>" sender-name sender-email)
                                     'face '(info-emphasis variable-pitch))))
         (title (alist-get 'title item))
         (subject (if title
                      (format "%s %s %s\n"
                              (propertize "Subject:" 'face 'shadow)
                              (all-the-icons-faicon "pencil")
                              (propertize title 'face '(variable-pitch bold info-header-node)))
                    ""))
         (body (or (alist-get 'body item) "… (nil) …"))
         (text (propertize body 'face '(fixed-pitch info-fixed-pitch))))
    (format "%s%s\n%s%s%s\n%s\n\n" separator1 datetime sender subject separator2 text)))

(defun pushbullet--insert-button (label help value action)
  (let ((start (point)))
    (insert label)
    (make-text-button
     start (point)
     'value value
     'action `(lambda (btn)
                (when (yes-or-no-p "Confirm action?")
                  (,action (button-get btn 'value))))
     'follow-link t
     'help-echo help)))

(defun pushbullet--delete-push (id)
  (pushbullet--request
   "DELETE" (format "/pushes/%s" id) nil
   (cl-function
    (lambda (&key data &allow-other-keys)
      (pushbullet--log "Push deleted: %s" id)
      (pushbullet)))))

(defun pushbullet--display-push (item)
  "Display a single Pushbullet push ITEM in the current buffer.
Only displays the push if it is active and has a non-empty body."
  (let ((active (alist-get 'active item))
        (body (alist-get 'body item)))
    (when (and active body (> (length body) 0))
      (insert (pushbullet--format-push item))
      (pushbullet--insert-button
       "[delete]"
       "Delete push"
       (alist-get 'iden item)
       'pushbullet--delete-push)
      (insert "\n\n"))))

(defun pushbullet--display-pushes (data)
  "Display multiple Pushbullet pushes from DATA in the UI buffer.
DATA should contain 'pushes' (list of push items) and 'cursor' (pagination cursor).
Updates the buffer-local cursor for pagination and logs debug information."
  (let* ((inhibit-read-only t)
         (pushes (alist-get 'pushes data))
         (cursor (alist-get 'cursor data)))
    (pushbullet--log "Requested %S items, received %S" pushbullet-limit (length pushes))
    (setq pushbullet-cursor cursor)
    (with-current-buffer (get-buffer-create pushbullet-buffer)
      (mapc (lambda (item)
              (goto-char (point-max))
              (pushbullet--display-push item))
            pushes))))

(defun pushbullet--format-banner ()
  "Format the banner header for the Pushbullet UI buffer.
Returns a formatted string with the package name, version, and decorative separator."
  (let ((banner (format "Pushbullet %s" pushbullet-version)))
    (format "%s\n%s\n\n" banner (make-string (length banner) ?━))))

(defun pushbullet--delete-first-occurence (pattern)
  "Delete the region corresponding to the first match of PATTERN in the current buffer.
PATTERN should be a regexp string."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward pattern nil t)
      (delete-region (match-beginning 0) (match-end 0)))))

;;;###autoload
(defun pushbullet-update ()
  "Fetch and display Pushbullet pushes in the current buffer.
Uses pagination cursor if available to fetch additional pushes.
This function is called automatically when opening the Pushbullet buffer."
  (interactive)
  (let ((endpoint (if pushbullet-cursor
                      (format "/pushes?limit=%d&cursor=%s" pushbullet-limit pushbullet-cursor)
                    (format "/pushes?limit=%d" pushbullet-limit))))
    (pushbullet--request
     "GET" endpoint nil
     (cl-function
      (lambda (&key data &allow-other-keys)
        (pushbullet--display-pushes data)))))
  t)

;;;###autoload
(defun pushbullet-send (title body)
  "Send a note to Pushbullet with TITLE and TEXT."
  (interactive "sTitle: \nsText: ")
  (let ((data `((type . "note")
                (title . ,title)
                (body . ,body))))
    (pushbullet--request
     "POST" "/pushes" data
     (cl-function
      (lambda (&key data &allow-other-keys)
        (pushbullet--log "Note pushed successfully: %s" title))))))

;;;###autoload
(defun pushbullet-send-text (text)
  "Send a note to Pushbullet with TEXT, using `pushbullet-default-title` as title."
  (interactive "sText: ")
  (pushbullet-send pushbullet-default-title text))

;;;###autoload
(defun pushbullet-region (start end)
  "Push the selected region to Pushbullet.
START and END define the region boundaries.
The push title is set to the current buffer's name."
  (interactive "r")
  (unless (use-region-p)
    (error "No region selected"))
  (let ((text (buffer-substring-no-properties start end)))
    (pushbullet-send (buffer-name) text)))

;;;###autoload
(defun pushbullet-yank ()
  "Push the current kill-ring (clipboard) contents to Pushbullet."
  (interactive)
  (let ((text (current-kill 0)))
    (unless text
      (error "Kill ring is empty"))
    (pushbullet-send pushbullet-default-title text)))

(defvar pushbullet-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'pushbullet-send)
    (define-key map (kbd "C-c C-u") 'pushbullet-update)
    (define-key map (kbd "C-c C-o") 'browse-url-at-point)
    (define-key map (kbd "q") 'quit-window)
    map)
  "Keymap for `pushbullet-mode'.")

(define-derived-mode pushbullet-mode fundamental-mode "Pushbullet"
  "Major mode for Pushbullet UI buffer."
  (kill-all-local-variables)
  (use-local-map pushbullet-mode-map)
  (setq mode-name "pushbullet")
  (setq major-mode 'pushbullet-mode)
  (goto-address-mode 1)
  (setq buffer-read-only t))

;;;###autoload
(defun pushbullet ()
  "Open the Pushbullet UI buffer."
  (interactive)
  (let ((buffer (get-buffer-create pushbullet-buffer)))
    (with-current-buffer buffer
      (pushbullet-mode)
      (let ((inhibit-read-only t)
            (loading-message "Loading pushes...\n\n"))
        (erase-buffer)
        (insert (propertize (pushbullet--format-banner) 'face 'font-lock-function-name-face))
        (setq pushbullet-content-start-marker (point-max-marker))
        (insert (propertize loading-message 'face 'font-lock-comment-face))
        (pushbullet-update)
        (pushbullet--delete-first-occurence loading-message)))
    (switch-to-buffer buffer))
  t)

(provide 'pushbullet)

;;; pushbullet.el ends here
