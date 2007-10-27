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
;; displaying metadata about code. It's a minor mode for displaying
;; data that's been gathered and for initiating new augmentations.

;; Tests are present in spec/emacs-frontend-test.el

;;; Todo:

;; * Support overlapping layers

;;; Code:

(require 'cl)
(require 'json) ;; See hober's http://edward.oconnor.cx/2006/03/json.el

(defstruct layer begin end color message backend)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun augment-layer-from-plist (plist)
  "Make a layers struct from a plist."
  (make-layer :begin (string-to-number (first (split-string (getf plist :range) "\\.")))
	      :end (string-to-number (cadddr (split-string (getf plist :range) "\\.")))
	      :color (getf plist :color)
	      :message (getf plist :message)))

(defun augment-render-layer (layer)
  "Create an overlay for a layer." ;; needs to be reimplemented for xemacs
  (overlay-put (make-overlay (layer-begin layer) (layer-end layer))
	       'face (layer-face layer)))

(defun layer-face (layer)
  ;; could do some kind of transformation here for color themes.
  (cons 'background-color (layer-color layer)))

(defun augment-file-path (file)
  (concat
   (file-name-directory file) ".augment/"
   (file-name-nondirectory file)))

(defun augment-message-at-point (&optional point)
  (interactive)
  ;; find the first layer that the point is between begin and end
  (layer-message (find (or point (point)) layers :test
		       (lambda (p l) (and (> p (layer-begin l))
				     (< p (layer-end l)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-minor-mode augment-mode
  "Major mode for showing code metadata.

\\{augment-mode-map}"

  :keymap (setq augment-mode-map (make-sparse-keymap))

  (define-key augment-mode-map (kbd "C-c C-s") 'augment-initiate)
  (define-key augment-mode-map (kbd "C-c C-k") 'augment-clear)
  (define-key augment-mode-map (kbd "C-c C-i") 'augment-message-at-point)
  
  (make-local-variable 'layers)

  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook 'augment-initiate)

  (augment-initiate (buffer-file-name)))

(defun augment-initiate (&optional file)
  (interactive)
  (setq layers nil)
  (augment-clear)
  (augment-start-process)
  (process-send-string "augment" (concat (or file (buffer-file-name)) "\n")))

(defun augment-start-process ()
  (unless (get-process "augment") ;; only one should be running at a time
    (set-process-filter
     (start-process "augment" "*augment-out*"
		    "augment" "--interactive")
     'augment-filter)))

(defun augment-filter (process output)
  (if (string-match "^Error augmenting \\(.*\\)\\." output)
      (error "Error augmenting %s." (match-string 1 output))
    ;; layers need to be cached in local var for messages
    (let* ((json-object-type 'plist)
	   (json-array-type 'list)
	   (all-layers (json-read-from-string output)))
      (while all-layers
	;; gotta remove the colon from the plisted filename
	(let ((filename (substring (symbol-name (pop all-layers)) 1 nil))
	      (layer-plists (pop all-layers)))
	  (with-current-buffer (file-name-nondirectory filename)
	    (setq layers (mapcar #'augment-layer-from-plist layer-plists))
	    (augment-buffer layers)))))))

(defun augment-buffer (layers)
  (dolist (layer layers)
    (augment-render-layer layer)))

(defun augment-clear ()
  (interactive)
  (remove-overlays))

(defun augment-reset ()
  (interactive)
  (kill-process "augment")
  (augment-clear))

(provide 'augment)
;;; augment.el ends here