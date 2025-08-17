;;; lobsters.el --- A Lobsters client -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 1.0
;; URL: https://github.com/tanrax/lobsters.el
;; Package-Requires: ((emacs "25.1") (request "0.2.0") (visual-fill-column "2.4"))

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A simple Lobsters client for Emacs that allows browsing hottest and newest
;; stories from https://lobste.rs/
;;
;; Usage:
;; View hottest stories: `M-x lobsters-hottest`
;; View newest stories: `M-x lobsters-newest`

;;; Code:

;; Autoload
(defconst lobsters--root-dir (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path lobsters--root-dir)
(require 'lobsters-variables)
(require 'lobsters-feed)
(require 'lobsters-ui)
(require 'visual-fill-column)
(require 'cl-lib)

(defconst lobsters--max-width 80)
(defgroup lobsters nil
  "A Lobsters client for Emacs."
  :group 'lobsters)

(define-derived-mode lobsters-view-mode special-mode "Lobsters"
  "Major mode for viewing lobsters stories."
  (setq visual-fill-column-center-text t)
  (setq visual-fill-column-width lobsters--max-width)
  (visual-fill-column-mode 1))

(defun lobsters-hottest ()
  "View the hottest stories from Lobsters."
  (interactive)
  (lobsters-feed--fetch-stories-async lobsters-variables--hottest-endpoint 'hottest))

(defun lobsters-newest ()
  "View the newest stories from Lobsters."
  (interactive)
  (lobsters-feed--fetch-stories-async lobsters-variables--newest-endpoint 'newest))

(provide 'lobsters)
;;; lobsters.el ends here
