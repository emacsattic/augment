;;; augment.el --- Display metadata about code

;; Copyright (C) 2007 Phil Hagelberg

;; Author: Phil Hagelberg <technomancy@gmail.com>
;; Created: 16 Oct 2007
;; Version: 0.1
;; Keywords: augment testing metadata

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs; see the file COPYING, or type `C-h C-c'. If not,
;; write to the Free Software Foundation at this address:

;;   Free Software Foundation
;;   51 Franklin Street, Fifth Floor
;;   Boston, MA 02110-1301
;;   USA

;;; Commentary:

;; augment.el is a frontend for augment, a system for gathering and
;; displaying metadata about code. augment.el is a frontend for
;; augment, meaning that it's for displaying data that's been gathered
;; and for initiating new augmentations.

;; Tests are present in spec/augment-frontend.el

;;; Todo:

;;; Bugs:

;;; Code:

(require 'cl)
(require 'json) ;; See hober's http://edward.oconnor.cx/2006/03/json.el

(defvar augment-file-extensions '(".rb")
  "List of file extensions that augment activates for.")

(defstruct layer begin end color message)

(defun augment-layer-from-json (json-string)
  (let* ((json (json-read-from-string json-string))
	 (range (cdr (assoc 'range json))))
    (setq l json)
    (make-layer :begin (string-to-number (first (split-string range "\\.")))
		:end (string-to-number (cadddr (split-string range "\\.")))
		:color (cdr (assoc 'color json))
		:message (cdr (assoc 'message json)))))
  
(define-minor-mode augment-mode
  "Major mode for showing code metadata.
\\{test-unit-mode-map}"

  :lighter "-augment"
  :keymap (setq augment-mode-map (make-sparse-keymap))

  (define-key augment-mode-map
    "\C-c\C-s" 'augment-initiate)
  (define-key augment-mode-map
    "\C-c\C-i" 'augment-show-info)
  (define-key augment-mode-map
    "\C-c\C-k" 'augment-clear)
  (define-key augment-mode-map
    "\C-x`" 'augment-jump-to-next-message)
  
  ;; a buffer where we wait for a complete line from the augment process
  (set (make-local-variable 'augment-incomplete-line) "")
  (augment-watch))

(defun augment-watch ()
  (set-process-filter (start-process "augment" nil
				     "augment-out" "--watch" (buffer-file-name))
		      'augment-filter))

(defun augment-filter (process output)
  ;; SO bloody annoying; why can't processes do this by default?!
  ;; At least we can use the Power of Recursion (tm)!
  (with-current-buffer test-unit-currently-running-buffer
    (if (string-match "\n" output)
	(progn
	  ;; Send everything up to the first newline to the real filter
	  (funcall test-filter-function
		   process (concat test-unit-incomplete-line (car (split-string output "\n"))))
	  ;; Recurse on the rest
	  (test-unit-filter process (substring output (+ 1 (string-match "\n" output)))))
      ;; Save the remainder to a buffer
      (setq test-unit-incomplete-line (concat test-unit-incomplete-line output)))))

(provide 'augment)
;;; augment.el ends here