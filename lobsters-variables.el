;;; lobsters-variables.el --- A Lobsters client for Emacs -*- lexical-binding: t -*- -*- coding: utf-8 -*-

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

;; Configuration variables for the Lobsters Emacs client.

;;; Code:

(defcustom lobsters-stories-per-page nil
  "Number of stories to display per page. Set to nil to show all stories."
  :type '(choice (const :tag "Show all" nil)
                 (integer :tag "Stories per page"))
  :group 'lobsters)

(defcustom lobsters-auto-refresh-interval nil
  "Automatic refresh interval in seconds. Set to nil to disable."
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds"))
  :group 'lobsters)

;; API endpoints
(defconst lobsters--hottest-endpoint "https://lobste.rs/hottest.json")
(defconst lobsters--newest-endpoint "https://lobste.rs/newest.json")

;; Buffer names
(defconst lobsters--hottest-buffer-name "*Lobsters - Hottest*")
(defconst lobsters--newest-buffer-name "*Lobsters - Newest*")

;; Variables for state management
(defvar lobsters--stories nil
  "List of currently loaded stories.")

(defvar lobsters--current-feed-type nil
  "Current feed type being displayed ('hottest or 'newest).")

(defvar lobsters--loading nil
  "Whether we're currently loading stories.")

(provide 'lobsters-variables)
;;; lobsters-variables.el ends here
