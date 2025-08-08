;;; lobsters-ui.el --- A Lobsters client for Emacs -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;;; Commentary:

;; User interface functions for the Lobsters client.

;;; Code:

(require 'lobsters-variables)
(require 'lobsters-feed)
(require 'widget)
(require 'wid-edit)
(require 'eww)
(require 'cl-lib)

;; UI Variables
(defconst lobsters--char-separator ?-)

(defun lobsters--insert-formatted-text (text &optional size font-color background-color)
  "Insert TEXT with optional formatting SIZE, FONT-COLOR, and BACKGROUND-COLOR."
  (let ((start (point)))
    (insert text)
    (let ((end (point))
          (props (list)))
      (when size
        (push `(:height ,size) props))
      (when font-color
        (push `(:foreground ,font-color) props))
      (when background-color
        (push `(:background ,background-color) props))
      (when props
        (put-text-property start end 'face (apply #'append props))))))

(defun lobsters--insert-logo ()
  "Insert the Lobsters logo/header."
  (lobsters--insert-formatted-text "🦞 " 1.5 "#d2691e")
  (lobsters--insert-formatted-text "Lobsters" 1.3 "#d2691e")
  (lobsters--insert-formatted-text " for Emacs\n\n"))

(defun lobsters--string-separator ()
  "Return a string with the separator character."
  (make-string lobsters--max-width lobsters--char-separator))

(defun lobsters--insert-separator ()
  "Insert a horizontal separator line."
  (lobsters--insert-formatted-text "\n")
  (lobsters--insert-formatted-text (lobsters--string-separator) nil "#666666")
  (lobsters--insert-formatted-text "\n"))

(defun lobsters--format-tags (tags)
  "Format TAGS list for display."
  (if tags
      (concat "[" (mapconcat 'identity tags ", ") "]")
    ""))

(defun lobsters--format-relative-time (timestamp)
  "Format TIMESTAMP as relative time."
  (let* ((time (date-to-time timestamp))
         (diff (float-time (time-subtract (current-time) time)))
         (days (floor (/ diff 86400)))
         (hours (floor (/ (mod diff 86400) 3600)))
         (minutes (floor (/ (mod diff 3600) 60))))
    (cond
     ((> days 0) (format "%d day%s ago" days (if (= days 1) "" "s")))
     ((> hours 0) (format "%d hour%s ago" hours (if (= hours 1) "" "s")))
     ((> minutes 0) (format "%d minute%s ago" minutes (if (= minutes 1) "" "s")))
     (t "just now"))))

(defun lobsters--story-component (story)
  "Insert a story component for STORY."
  (let* ((title (cdr (assoc 'title story)))
         (url (cdr (assoc 'url story)))
         (score (cdr (assoc 'score story)))
         (comment-count (cdr (assoc 'comment-count story)))
         (submitter (cdr (assoc 'submitter story)))
         (tags (cdr (assoc 'tags story)))
         (created-at (cdr (assoc 'created-at story)))
         (comments-url (cdr (assoc 'comments-url story)))
         (description (cdr (assoc 'description story))))

    ;; Title (make it a clickable link)
    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (if url
                                 (eww-browse-url url)
                               (message "No URL available for this story")))
                   :help-echo (if url (format "Open: %s" url) "No URL")
                   :format "%[%v%]"
                   title)

    (lobsters--insert-formatted-text "\n")

    ;; Score and comments info
    (lobsters--insert-formatted-text "  ")
    (lobsters--insert-formatted-text (format "↑%d" score) nil "#ff6600")
    (lobsters--insert-formatted-text " | ")
    (lobsters--insert-formatted-text (format "%d comment%s"
                                             comment-count
                                             (if (= comment-count 1) "" "s")) nil "#666666")
    (lobsters--insert-formatted-text " | by ")
    (lobsters--insert-formatted-text submitter nil "#0066cc")
    (lobsters--insert-formatted-text " | ")
    (lobsters--insert-formatted-text (lobsters--format-relative-time created-at) nil "#666666")

    ;; Tags
    (when tags
      (lobsters--insert-formatted-text "\n  ")
      (lobsters--insert-formatted-text (lobsters--format-tags tags) nil "#008000"))

    ;; Description (if available)
    (when (and description (not (string-empty-p description)))
      (lobsters--insert-formatted-text "\n  ")
      (lobsters--insert-formatted-text description nil "#333333"))

    ;; Comments link
    (lobsters--insert-formatted-text "\n  ")
    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (eww-browse-url comments-url))
                   :help-echo (format "View comments: %s" comments-url)
                   " 💬 Comments ")

    ;; URL link (if different from comments)
    (when url
      (lobsters--insert-formatted-text " ")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (eww-browse-url url))
                     :help-echo (format "Open link: %s" url)
                     " 🔗 Link "))

    (lobsters--insert-formatted-text "\n")
    (lobsters--insert-separator)))

(defun lobsters--insert-header (feed-type)
  "Insert the header for FEED-TYPE."
  (lobsters--insert-logo)

  ;; Navigation buttons
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (lobsters--fetch-stories-async lobsters--hottest-endpoint 'hottest))
                 :help-echo "View hottest stories"
                 " 🔥 Hottest ")

  (lobsters--insert-formatted-text " ")

  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (lobsters--fetch-stories-async lobsters--newest-endpoint 'newest))
                 :help-echo "View newest stories"
                 " 🆕 Newest ")

  (lobsters--insert-formatted-text " ")

  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (lobsters--refresh-current-feed))
                 :help-echo "Refresh current feed"
                 " ↻ Refresh ")

  ;; Current feed indicator
  (lobsters--insert-formatted-text (format "\n\nShowing: %s stories\n"
                                           (if (eq feed-type 'hottest) "Hottest" "Newest"))
                                   nil "#d2691e")

  ;; Keyboard shortcuts help
  (lobsters--insert-formatted-text "Keyboard: (n) Next story | (p) Previous story | (r) Refresh | (q) Quit\n")

  (lobsters--insert-separator))

(defun lobsters--insert-stories ()
  "Insert all stories."
  (let ((stories (lobsters--get-all-stories)))
    (if stories
        (dolist (story stories)
          (lobsters--story-component story))
      (lobsters--insert-formatted-text "No stories available.\n" nil "#ff0000"))))

(defun lobsters--goto-next-story ()
  "Go to the next story."
  (interactive)
  (let ((separator-regex (concat "^" (regexp-quote (lobsters--string-separator)) "$")))
    (if (search-forward-regexp separator-regex nil t)
        (forward-line 1)
      (message "No more stories"))))

(defun lobsters--goto-previous-story ()
  "Go to the previous story."
  (interactive)
  (let ((separator-regex (concat "^" (regexp-quote (lobsters--string-separator)) "$")))
    (search-backward-regexp separator-regex nil t)
    (unless (search-backward-regexp separator-regex nil t)
      (goto-char (point-min)))
    (forward-line 1)))

(defun lobsters--quit ()
  "Quit the Lobsters buffer."
  (interactive)
  (let ((buffer-name (if (eq lobsters--current-feed-type 'hottest)
                         lobsters--hottest-buffer-name
                       lobsters--newest-buffer-name)))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))))

(defun lobsters--display-stories (feed-type)
  "Display stories for FEED-TYPE."
  (let ((buffer-name (if (eq feed-type 'hottest)
                         lobsters--hottest-buffer-name
                       lobsters--newest-buffer-name)))
    (switch-to-buffer buffer-name)
    (kill-all-local-variables)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)

    ;; Insert content
    (lobsters--insert-header feed-type)
    (lobsters--insert-stories)

    ;; Set up the buffer
    (use-local-map widget-keymap)
    (display-line-numbers-mode 0)

    ;; Keyboard shortcuts
    (local-set-key (kbd "n") 'lobsters--goto-next-story)
    (local-set-key (kbd "p") 'lobsters--goto-previous-story)
    (local-set-key (kbd "r") 'lobsters--refresh-current-feed)
    (local-set-key (kbd "q") 'lobsters--quit)
    (local-set-key (kbd "g") 'lobsters--refresh-current-feed)

    ;; Enable minor mode and finish setup
    (lobsters-mode 1)
    (widget-setup)
    (goto-char (point-min))
    (widget-forward 1)
    (read-only-mode 1)))

(provide 'lobsters-ui)
;;; lobsters-ui.el ends here
