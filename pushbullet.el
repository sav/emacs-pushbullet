;;; pushbullet.el --- Pushbullet client for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Savio Sena <savio.sena@gmail.com>

;; Author: Savio Sena <savio.sena@gmail.com>
;; Version: 1.0.1
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

;;;; This package provides a comprehensive Emacs client for the Pushbullet service.
;;; It enables users to:
;;; - Send various types of pushes (notes, links) from Emacs.
;;; - Push selected text regions or clipboard contents directly to Pushbullet.
;;; - Browse, add,  delete, and send Pushbullet pushes within a dedicated Emacs UI.
;;; - Export pushes to Org-mode format.

;;; Usage Examples:

;;;   `M-x pushbullet`           : Opens or switches to the main Pushbullet UI buffer.
;;;   `M-x pushbullet-send`      : Prompts for a title and text to send a new note push.
;;;   `M-x pushbullet-send-text` : Prompts for text to send a new note push, using a default title.
;;;   `M-x pushbullet-region`    : Sends the active region's content as a note push.
;;;   `M-x pushbullet-yank`      : Sends the latest kill-ring entry (clipboard content) as a note push.
;;;   `M-x pushbullet-export`    : Exports currently fetched pushes to an Org-mode buffer.

;;; Code:

(require 'all-the-icons)
(require 'button)
(require 'cl-lib)
(require 'wid-edit)
(require 'widget)
(require 'pushbullet-api)

(defconst pushbullet-version "1.0.1"
  "The current version string of the Pushbullet Emacs package.")

(defgroup pushbullet nil
   "Client for the Pushbullet service, providing integration with Emacs."
   :version pushbullet-version
   :prefix "pushbullet-"
   :group 'applications)

(defcustom pushbullet-debug nil
  "Enable verbose logging for Pushbullet operations.
When non-nil, additional debug messages will be printed to the
*Messages* buffer."
  :type 'boolean
  :group 'pushbullet
  :initialize 'custom-initialize-default)

(defvar pushbullet-buffer-name "*Pushbullet*"
  "The name of the main buffer where the Pushbullet user interface is
 displayed.")

(defvar pushbullet-export-buffer-name "*Pushbullet Export*"
   "The name of the buffer used for exporting Pushbullet pushes to
 Org-mode format.")

(defcustom pushbullet-default-title (format "GNU Emacs %s" emacs-version)
  "The default title string used for new pushes when no explicit title
 is provided.
It is formatted to include the current Emacs version."
  :type 'string
  :group 'pushbullet)

(defcustom pushbullet-columns 70
  "Maximum number of columns for wrapping lines in the Pushbullet UI buffer."
  :type 'integer
  :group 'pushbullet)

(defcustom pushbullet-left-alignment 8
   "The size of the left alignment padding in the Pushbullet UI."
   :type 'integer
   :group 'pushbullet)

(defcustom pushbullet-textfield-width
   (truncate
       (* (- pushbullet-columns pushbullet-left-alignment) 0.90))
   "The calculated width for editable text fields within the Pushbullet UI."
   :type 'integer
   :group 'pushbullet)

(defcustom pushbullet-show-send-form t
  "Whether to display the send form in the Pushbullet UI."
  :type 'boolean
  :group 'pushbullet)

(defvar pushbullet--buffer nil
   "The buffer currently used for rendering the Pushbullet UI. This is a
 buffer-local variable.")

(defvar pushbullet--title nil
   "The title string displayed at the top of the Pushbullet UI buffer.
 This is a buffer-local variable.")

(defvar pushbullet--pushes nil
   "A buffer-local list of Pushbullet pushes currently displayed in the
 UI, where each push is an alist.")

(defmacro pushbullet--log (fmt &rest args)
  "Logs a debug message with FMT and ARGS if `pushbullet-debug' is
 enabled.
The message is prefixed with '[pushbullet]' for easy identification
in the `*Messages*' buffer."
  `(when pushbullet-debug
     (message (concat "[pushbullet] " ,fmt) ,@args)))

(defun pushbullet--align-right (max str)
   "Inserts spaces to right-align STR within a field of MAX width in the
 current buffer."
   (let ((len (length str)))
       (when (>= max len)
         (widget-insert (make-string (- max (length str)) ?\s)))))

(defun pushbullet--insert-aligned (str)
   "Inserts a newline and then the string STR, right-aligned by
 `pushbullet-left-alignment'."
   (widget-insert "\n")
   (pushbullet--align-right pushbullet-left-alignment str)
   (widget-insert str))

(defun pushbullet--list-filter (pushes)
  "Filters a list of PUSHES, returning only those that are active and
 have at least a title, URL, or body."
  (seq-filter (lambda (push) (pushbullet-api-active push)) pushes))

(defun pushbullet--list-remove (pushes push)
  "Removes PUSH from LIST where elements in LIST match PUSH
 based on the `'iden' key-value pairs."
  (let ((iden (alist-get 'iden push)))
    (seq-remove (lambda (item) (equal (alist-get 'iden item) iden)) pushes)))

(defun pushbullet--send (title body url)
  (pushbullet-api-send title body url)
  (pushbullet--log "Pushed: (%S, %S, %S)" title body url)
  (pushbullet--load-more 1))

(defun pushbullet--load-more (&optional limit)
  "Fetches additional pushes from the Pushbullet server using the `fetch'
 callback from `pushbullet--api', and then re-renders the UI."
  (let ((fetch (alist-get 'fetch pushbullet--api)))
    (pushbullet-api-fetch
     #'(lambda (pushes)
         (setq pushbullet--pushes
               (pushbullet--list-filter
                (append pushbullet--pushes pushes)))
         (pushbullet--log "Loaded more %S pushes. Total: %S"
                          (length pushes) (length pushbullet--pushes))
         (pushbullet--render)
         ;; Move cursor back to its original position when called from
         ;; "Load More" button.
         (when (not limit)
           (goto-char (point-max))
           (search-backward "Load More")))
     limit))
  nil)

(defun pushbullet--export-all ()
  "Exports all currently loaded pushes to an Org-mode buffer using the
 `export' callback from `pushbullet--api'."
  (pushbullet-export pushbullet--pushes))

(defun pushbullet--delete-all (&rest args)
  "Deletes all pushes currently displayed in the UI from the Pushbullet
 server using the `delete' callback from `pushbullet--api', then
 re-renders the UI."
  (dolist (push pushbullet--pushes)
    (pushbullet-api-delete push))
  (setq pushbullet--pushes nil)
  (pushbullet--render))

(defun pushbullet--delete-row (push)
  "Deletes a single PUSH from the `pushbullet--pushes' list, invokes
 the `delete' callback from `pushbullet--api', and then re-renders
 the UI."
  (setq pushbullet--pushes
        (pushbullet--list-remove pushbullet--pushes push))
  (pushbullet-api-delete push)
  (pushbullet--log "Row deleted")
  (pushbullet--render))

(defun pushbullet--render-top (title)
  "Renders the top section of the Pushbullet UI, displaying the
 provided TITLE as a banner."
  (let ((len (length title)))
    (widget-insert
     (propertize
      (concat
       "══ " title " "
       (make-string (- pushbullet-columns 4 len) ?═)
       "\n")
      'face 'bold))))

(defun pushbullet--render-push (push)
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
                            (- pushbullet-columns
                               (+ 7 (length datetime))) ?─) "\n"))
         (_ (pushbullet--insert-aligned "Title: "))
         (title-w (widget-create 'editable-field
                                 :size pushbullet-textfield-width
                                 :format "%v"
                                 :value (or (alist-get 'title push) "")))
         (_ (pushbullet--insert-aligned "URL: "))
         (url-w (widget-create 'editable-field
                               :size pushbullet-textfield-width
                               :format "%v"
                               :value (or
                                       (alist-get 'url push)
                                       (alist-get 'image_url push)
                                       (alist-get 'file_url push)
                                       "")))
         (_ (pushbullet--insert-aligned "Text: "))
         (body-w (widget-create 'text
                                :size pushbullet-textfield-width
                                :format "%v"
                                :value (or (alist-get 'body push) ""))))
    (widget-insert "\n\n")
    (pushbullet--align-right pushbullet-columns "[Delete]")
    (widget-create 'push-button
                   :notify
                   (lambda (&rest _)
                     (pushbullet--delete-row push))
                   "Delete")
    (widget-insert "\n")))

(defun pushbullet--render-pushes ()
  "Render the list of pushes in the UI (`pushbullet-ai--pushes')."
  (dolist (push pushbullet--pushes)
    (when (pushbullet-api-active push)
      (pushbullet--render-push push))))

(defun pushbullet--render-form ()
  "Renders the 'New Push' form, allowing users to input a title, URL,
 and body for a new Pushbullet push.
Includes a 'Push' button to submit the form via the `send' callback from
`pushbullet--api'."
  (widget-insert
   (propertize
    (concat "\n\n\n══ New Push "
            (make-string (- pushbullet-columns 12) ?═) " \n")
    'face 'bold))
  (pushbullet--insert-aligned "Title: ")
  (let* ((new-title (widget-create
                     'editable-field
                     :size pushbullet-textfield-width
                     :value ""))
         (_ (pushbullet--insert-aligned "URL: "))
         (new-url (widget-create
                   'editable-field
                   :size pushbullet-textfield-width
                   :value ""))
         (_ (pushbullet--insert-aligned "Text: "))
         (new-body (widget-create
                    'editable-field
                    :size pushbullet-textfield-width
                    :value "")))

    (widget-insert "\n\n")
    (widget-insert (make-string pushbullet-columns ?═) "\n")
    (pushbullet--align-right pushbullet-columns " Push ")
    (widget-create 'push-button
                   :notify (lambda (&rest _)
                             (pushbullet--send
                              (widget-value new-title)
                              (widget-value new-body)
                              (widget-value new-url)))
                   "Push")
    (widget-insert "\n")))

(defun pushbullet--render-bottom ()
  "Renders the bottom section of the Pushbullet UI, including action
 buttons such as 'Load More', 'Export', 'Delete All', and 'Close'."
  (widget-insert "\n" (make-string pushbullet-columns ?═) "\n")
  (pushbullet--align-right
   pushbullet-columns
   "[Load More] [Export] [Delete All] [Close]")
  (widget-create 'push-button
                 :notify (lambda (&rest _) (pushbullet--load-more))
                 "Load More")
  (widget-insert " ")
  (widget-create 'push-button
                 :notify (lambda (&rest _) (pushbullet--export-all))
                 "Export")
  (widget-insert " ")
  (widget-create 'push-button
                 :notify (lambda (&rest _) (pushbullet--delete-all))
                 "Delete All")
  (widget-insert " ")
  (widget-create 'push-button
                 :notify (lambda (&rest _) (kill-buffer))
                 "Close")
  (widget-insert "\n"))

(defun pushbullet--render ()
  "Renders the complete Pushbullet UI in the buffer specified by
 `pushbullet--buffer'.
This involves rendering the top banner, iterating through
`pushbullet--pushes' to display each push, rendering the bottom
action buttons, and optionally rendering the 'New Push' form if
`pushbullet-show-send-form' is non-nil."
  (with-current-buffer pushbullet--buffer
    (let* ((inhibit-read-only t)
           (inhibit-modification-hooks t))
      (remove-overlays)
      (erase-buffer)
      (goto-address-mode 1)
      (pushbullet--render-top pushbullet--title)
      ;; cleanup inactive pushes
      (setq pushbullet--pushes
            (pushbullet--list-filter pushbullet--pushes))
      (pushbullet--render-pushes)
      (pushbullet--render-bottom)
      (when pushbullet-show-send-form
        (pushbullet--render-form))
      (widget-setup)
      (use-local-map
       (make-composed-keymap
        pushbullet-mode-map widget-keymap)))))

;;;###autoload
(defun pushbullet-export (&optional pushes)
  "Exports a list of PUSHES to a new Org-mode buffer named
 `pushbullet-export-buffer-name'.

Each active push is formatted as an Org-mode heading, including its
title, URL (if present), and body.

If PUSHES is `nil` or the function is called interactively, it exports
the currently displayed pushes from the UI (`pushbullet-ui--pushes`).

This function is interactive."
  (interactive)
  (let ((buffer (get-buffer-create pushbullet-export-buffer-name))
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
  (pushbullet-api-send title body url))

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

(defconst pushbullet-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-e") #'pushbullet--export-all)
    (define-key map (kbd "C-c C-u") #'pushbullet--load-more)
    (define-key map (kbd "C-c C-o") #'browse-url-at-point)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `pushbullet-mode', defining keybindings for interacting
 with the Pushbullet UI.

- `C-c C-e': Calls `pushbullet--export-all` to export pushes to Org-mode.
- `C-c C-u': Calls `pushbullet--load-more` to fetch more pushes.
- `C-c C-o': Calls `browse-url-at-point` to open a URL at the current cursor position.
- `q': Calls `quit-window` to close the Pushbullet UI buffer.")

;;;###autoload
(defun pushbullet ()
  "Opens or switches to the main Pushbullet UI buffer (`pushbullet-buffer-name').
This function initializes the Pushbullet UI by setting up buffer-local
variables, configuring `pushbullet-mode', and fetching the latest pushes
from the Pushbullet API. If the buffer already exists, it is
re-initialized and updated to reflect the current state.

This function is interactive."
  (interactive)
  (when (null (get-buffer pushbullet-buffer-name))
    (let ((buffer (get-buffer-create pushbullet-buffer-name)))
      (with-current-buffer buffer
        (kill-all-local-variables)
        (remove-overlays)
        (erase-buffer)
        (setq
         pushbullet--title (format "Pushbullet %s" pushbullet-version)
         pushbullet--buffer buffer
         pushbullet--pushes nil
         pushbullet-api-cursor nil))))
  (pushbullet--load-more)
  (switch-to-buffer-other-window (get-buffer pushbullet-buffer-name))
  nil)

(provide 'pushbullet)

;;; pushbullet.el ends here
