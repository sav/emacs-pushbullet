;;; pushbullet-ui.el --- Pushbullet UI -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Savio Sena <savio.sena@gmail.com>

;; Author: Savio Sena <savio.sena@gmail.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1") (all-the-icons "5.0.0"))
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
;;; This package provides the user interface components for the Emacs Pushbullet
;;; client. It includes functions for rendering pushes, handling user
;;; interactions, and managing the display of Pushbullet data within Emacs.
;;;

;;; Code:

(require 'cl-lib)
(require 'button)
(require 'widget)
(require 'wid-edit)
(require 'all-the-icons)

(defgroup pushbullet-ui nil
   "User interface for the Pushbullet client."
   :group 'extensions)

(defcustom pushbullet-ui-columns 70
   "Maximum number of columns for wrapping lines in the Pushbullet UI buffer."
   :type 'integer
   :group 'pushbullet-ui)

(defcustom pushbullet-ui-left-alignment 8
   "The size of the left alignment padding in the Pushbullet UI."
   :type 'integer
   :group 'pushbullet-ui)

(defcustom pushbullet-ui-textfield-width
   (truncate
       (* (- pushbullet-ui-columns pushbullet-ui-left-alignment) 0.90))
   "The calculated width for editable text fields within the Pushbullet UI."
   :type 'integer
   :group 'pushbullet-ui)

(defcustom pushbullet-ui-debug nil
   "Enable verbose logging for Pushbullet UI operations.
When non-nil, additional debug messages will be printed to the *Messages* buffer."
   :type 'boolean
   :group 'pushbullet-ui)

(defcustom pushbullet-ui-show-send-form t
   "Whether to display the send form in the Pushbullet UI."
   :type 'boolean
   :group 'pushbullet-ui)

(defvar pushbullet-ui--buffer nil
   "The buffer currently used for rendering the Pushbullet UI. This is a
 buffer-local variable.")

(defvar pushbullet-ui--title nil
   "The title string displayed at the top of the Pushbullet UI buffer.
 This is a buffer-local variable.")

(defvar pushbullet-ui--pushes nil
   "A buffer-local list of Pushbullet pushes currently displayed in the
 UI, where each push is an alist.")

(defvar pushbullet-ui--mode-map nil
  "The keymap active within the Pushbullet UI buffer.")

(defvar pushbullet-ui--api nil
  "A buffer-local alist of callback functions for Pushbullet API
 interactions. See `pushbullet-api'.")

(defmacro pushbullet-ui--log (fmt &rest args)
  "Logs a debug message with FMT and ARGS if `pushbullet-ui-debug' is
 enabled.
The message is prefixed with '[pushbullet-ui]' for easy identification
in the `*Messages*' buffer."
  `(when pushbullet-ui-debug
     (message (concat "[pushbullet-ui] " ,fmt) ,@args)))

(defun pushbullet-ui--align-right (max str)
   "Inserts spaces to right-align STR within a field of MAX width in the
 current buffer."
   (let ((len (length str)))
       (when (>= max len)
         (widget-insert (make-string (- max (length str)) ?\s)))))

(defun pushbullet-ui--insert-aligned (str)
   "Inserts a newline and then the string STR, right-aligned by
 `pushbullet-ui-left-alignment'."
   (widget-insert "\n")
   (pushbullet-ui--align-right pushbullet-ui-left-alignment str)
   (widget-insert str))

(defun pushbullet-ui--list-filter (pushes)
  "Filters a list of PUSHES, returning only those that are active and
 have at least a title, URL, or body."
  (let ((is-active (alist-get 'active pushbullet-ui--api)))
    (seq-filter (lambda (push) (funcall is-active push)) pushes)))

(defun pushbullet-ui--list-remove (list push)
  "Removes PUSH from LIST where elements in LIST match PUSH
 based on the `'iden' key-value pairs."
  (let ((iden (alist-get 'iden push)))
    (seq-remove (lambda (item) (equal (alist-get 'iden item) iden)) list)))

(defun pushbullet-ui--send (title body url)
  (let ((send (alist-get 'send pushbullet-ui--api)))
    (funcall send title body url)
    (pushbullet-ui--log "Pushed: (%S, %S, %S)" title body url)
    (pushbullet-ui--load-more 1)))

(defun pushbullet-ui--load-more (&optional limit)
  "Fetches additional pushes from the Pushbullet server using the `fetch'
 callback from `pushbullet-ui--api', and then re-renders the UI."
  (let ((fetch (alist-get 'fetch pushbullet-ui--api)))
    (funcall fetch
             #'(lambda (pushes)
                 (setq pushbullet-ui--pushes
                       (pushbullet-ui--list-filter
                        (append pushbullet-ui--pushes pushes)))
                 (pushbullet-ui--log "Loaded more %S pushes. Total: %S"
                                     (length pushes) (length pushbullet-ui--pushes))
                 (pushbullet-ui--render)
                 ;; Move cursor back to its original position when called from
                 ;; "Load More" button.
                 (when (not limit)
                   (goto-char (point-max))
                   (search-backward "Load More")))
             limit))
  nil)

(defun pushbullet-ui--export-all ()
   "Exports all currently loaded pushes to an Org-mode buffer using the
 `export' callback from `pushbullet-ui--api'."
   (let* ((export (alist-get 'export pushbullet-ui--api))) 
     (funcall export pushbullet-ui--pushes)))

(defun pushbullet-ui--delete-all (&rest args)
  "Deletes all pushes currently displayed in the UI from the Pushbullet
 server using the `delete' callback from `pushbullet-ui--api', then
 re-renders the UI."
  (let* ((del (alist-get 'del pushbullet-ui--api)))
    (dolist (push pushbullet-ui--pushes)
      (funcall del push)))
  (setq pushbullet-ui--pushes nil)
  (pushbullet-ui--render))

(defun pushbullet-ui--delete-row (push)
  "Deletes a single PUSH from the `pushbullet-ui--pushes' list, invokes
 the `delete' callback from `pushbullet-ui--api', and then re-renders
 the UI."
  (setq pushbullet-ui--pushes
        (pushbullet-ui--list-remove pushbullet-ui--pushes push))
  (let* ((del (alist-get 'del pushbullet-ui--api)))
    (funcall del push))
  (pushbullet-ui--log "Row deleted")
  (pushbullet-ui--render))

(defun pushbullet-ui--render-top (title)
  "Renders the top section of the Pushbullet UI, displaying the
 provided TITLE as a banner."
  (let ((len (length title)))
    (widget-insert
     (propertize
      (concat "══ " title " " (make-string (- pushbullet-ui-columns 4 len) ?═) "\n")
      'face 'bold))))

(defun pushbullet-ui--render-bottom ()
  "Renders the bottom section of the Pushbullet UI, including action
 buttons such as 'Load More', 'Export', 'Delete All', and 'Close'."
  (widget-insert "\n" (make-string pushbullet-ui-columns ?═) "\n")
  (pushbullet-ui--align-right
   pushbullet-ui-columns
   "   [Load More] [Export] [Delete All] [Close]")
  (widget-create 'push-button
                 :notify (lambda (&rest _) (pushbullet-ui--load-more))
                 "Load More")
  (widget-insert " ")
  (widget-create 'push-button
                 :notify (lambda (&rest _) (pushbullet-ui--export-all))
                 "Export")
  (widget-insert " ")
  (widget-create 'push-button
                 :notify (lambda (&rest _) (pushbullet-ui--delete-all))
                 "Delete All")
  (widget-insert " ")
  (widget-create 'push-button
                 :notify (lambda (&rest _) (kill-buffer))
                 "Close")
  (widget-insert "\n"))

(defun pushbullet-ui--render-push (push)
  "Renders a single PUSH (an alist) as a set of editable widgets in the UI.
This includes displaying its creation datetime, editable fields for
title, URL, and body, and 'Delete' buttons."
  (let* ((created (seconds-to-time (alist-get 'created push)))
         (datetime (propertize
                    (format "%s %s  %s %s"
                            (all-the-icons-faicon "calendar")
                            (format-time-string "%Y-%b-%d" created)
                            (all-the-icons-faicon "clock-o")
                            (format-time-string "%H:%M" created))
                    'face 'shadow))
         (_ (widget-insert (format "\n ── %s " datetime)))
         (_ (widget-insert (make-string
                            (- pushbullet-ui-columns
                               (+ 7 (length datetime))) ?─) "\n"))
         (_ (pushbullet-ui--insert-aligned "Title: "))
         (title-w (widget-create 'editable-field
                                 :size pushbullet-ui-textfield-width
                                 :format "%v"
                                 :value (or (alist-get 'title push) "")))
         (_ (pushbullet-ui--insert-aligned "URL: "))
         (url-w (widget-create 'editable-field
                               :size pushbullet-ui-textfield-width
                               :format "%v"
                               :value (or (alist-get 'url push) "")))
         (_ (pushbullet-ui--insert-aligned "Body: "))
         (body-w (widget-create 'text
                                :size pushbullet-ui-textfield-width
                                :format "%v"
                                :value (or (alist-get 'body push) ""))))
    (widget-insert "\n\n")
    (pushbullet-ui--align-right pushbullet-ui-columns "[Delete]")
    (widget-create 'push-button
                   :notify
                   (lambda (&rest _)
                     (pushbullet-ui--delete-row push))
                   "Delete")
    (widget-insert "\n")))

(defun pushbullet-ui--render-pushes ()
  "Render the list of pushes in the UI (`pushbullet-ai--pushes')."
  (dolist (push pushbullet-ui--pushes)
    (let* ((is-active (alist-get 'active pushbullet-ui--api)))
      (when (funcall is-active push)
        (pushbullet-ui--render-push push)))))

(defun pushbullet-ui--render-form ()
  "Renders the 'New Push' form, allowing users to input a title, URL,
 and body for a new Pushbullet push.
Includes a 'Push' button to submit the form via the `send' callback from
`pushbullet-ui--api'."
  (widget-insert
   (propertize
    (concat "\n\n\n══ New Push "
            (make-string (- pushbullet-ui-columns 12) ?═) " \n")
    'face 'bold))
  (pushbullet-ui--insert-aligned "Title: ")
  (let* ((new-title (widget-create
                     'editable-field
                     :size pushbullet-ui-textfield-width
                     :value ""))
         (_ (pushbullet-ui--insert-aligned "URL: "))
         (new-url (widget-create
                   'editable-field
                   :size pushbullet-ui-textfield-width
                   :value ""))
         (_ (pushbullet-ui--insert-aligned "Body: "))
         (new-body (widget-create
                    'editable-field
                    :size pushbullet-ui-textfield-width
                    :value "")))

    (widget-insert "\n\n")
    (widget-insert (make-string pushbullet-ui-columns ?═) "\n")
    (pushbullet-ui--align-right pushbullet-ui-columns " Push ")
    (widget-create 'push-button
                   :notify (lambda (&rest _)
                             (pushbullet-ui--send
                              (widget-value new-title)
                              (widget-value new-body)
                              (widget-value new-url)))
                   "Push")
    (widget-insert "\n")))

(defun pushbullet-ui--render ()
  "Renders the complete Pushbullet UI in the buffer specified by
 `pushbullet-ui--buffer'.
This involves rendering the top banner, iterating through
`pushbullet-ui--pushes' to display each push, rendering the bottom
action buttons, and optionally rendering the 'New Push' form if
`pushbullet-ui-show-send-form' is non-nil."
  (with-current-buffer pushbullet-ui--buffer
    (let* ((inhibit-read-only t)
           (inhibit-modification-hooks t))
      (remove-overlays)
      (erase-buffer)
      (goto-address-mode 1)
      (pushbullet-ui--render-top pushbullet-ui--title)
      ;; cleanup inactive pushes
      (setq pushbullet-ui--pushes
            (pushbullet-ui--list-filter pushbullet-ui--pushes))
      (pushbullet-ui--render-pushes)
      (pushbullet-ui--render-bottom)
      (when pushbullet-ui-show-send-form
        (pushbullet-ui--render-form))
      (widget-setup)
      (use-local-map
       (make-composed-keymap
        pushbullet-ui--mode-map widget-keymap)))))

(defun pushbullet-ui (title buffer api mode-map)
  "Initializes and displays the Pushbullet UI in the specified BUFFER.

This function sets up buffer-local variables, including the UI TITLE,
the API CALLBACKS alist for interaction, and the MODE-MAP for
keybindings. It then triggers the initial fetch of pushes and renders
the complete UI.

BUFFER is the buffer to create or use.
TITLE is a string to be displayed as the main title of the UI.
API is an alist of callback functions for Pushbullet API operations.
MODE-MAP is the keymap to use in the UI buffer."
  (setq pushbullet-ui--title title
	pushbullet-ui--buffer buffer
	pushbullet-ui--api api
	pushbullet-ui--pushes nil
	pushbullet-ui--mode-map mode-map)
  (pushbullet-ui--load-more)
  nil)

(provide 'pushbullet-ui)

;;; pushbullet-ui.el ends here
