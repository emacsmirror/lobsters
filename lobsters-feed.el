;;; lobsters-feed.el --- A Lobsters client for Emacs -*- lexical-binding: t -*- -*- coding: utf-8 -*-

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

;; Functions for fetching and processing Lobsters stories.

;;; Code:

(require 'lobsters-variables)
(require 'request)
(require 'json)
(require 'seq)

;; Hooks
(defvar lobsters-after-fetch-stories-hook nil
  "Hook run after stories are successfully fetched.")

(defun lobsters--fetch-stories-async (endpoint feed-type)
  "Fetch stories from ENDPOINT and display them. FEED-TYPE should be 'hottest or 'newest."
  (setq lobsters--loading t)
  (setq lobsters--current-feed-type feed-type)
  (message "Fetching Lobsters stories...")

  (request endpoint
    :timeout 10
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (setq lobsters--loading nil)
                (setq lobsters--stories (lobsters--process-stories data))
                (message "Lobsters stories loaded!")
                (lobsters--display-stories feed-type)
                (run-hooks 'lobsters-after-fetch-stories-hook)))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (setq lobsters--loading nil)
              (message "Error fetching Lobsters stories: %S" error-thrown)))))

(defun lobsters--process-stories (raw-stories)
  "Process RAW-STORIES from the API into a more usable format."
  (mapcar (lambda (story)
            (list
             (cons 'short-id (cdr (assoc 'short_id story)))
             (cons 'title (cdr (assoc 'title story)))
             (cons 'url (cdr (assoc 'url story)))
             (cons 'score (cdr (assoc 'score story)))
             (cons 'comment-count (cdr (assoc 'comment_count story)))
             (cons 'submitter (cdr (assoc 'submitter_user story)))
             (cons 'tags (cdr (assoc 'tags story)))
             (cons 'created-at (cdr (assoc 'created_at story)))
             (cons 'comments-url (cdr (assoc 'comments_url story)))
             (cons 'short-id-url (cdr (assoc 'short_id_url story)))
             (cons 'description (or (cdr (assoc 'description_plain story)) ""))))
          raw-stories))

(defun lobsters--get-all-stories ()
  "Get all stories."
  lobsters--stories)

(defun lobsters--refresh-current-feed ()
  "Refresh the current feed."
  (interactive)
  (when lobsters--current-feed-type
    (let ((endpoint (if (eq lobsters--current-feed-type 'hottest)
                        lobsters--hottest-endpoint
                      lobsters--newest-endpoint)))
      (lobsters--fetch-stories-async endpoint lobsters--current-feed-type))))

(provide 'lobsters-feed)
;;; lobsters-feed.el ends here
