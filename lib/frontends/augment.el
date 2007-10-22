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

;; * Write augment-minor-mode
;; * Watch for changes in the layer file

;;; Bugs:

;; * Doesn't deal with overlapping layers. (won't fix for a while)

;;; Code:

(require 'cl)
(require 'json) ;; See hober's http://edward.oconnor.cx/2006/03/json.el

(defvar augmented-buffers ()
  "List of all buffers currently being augmented.")

(defvar augment-incomplete-buffer ""
  "A buffer where we wait for a complete set of layers from the augment process.")

(defconst augment-filter-file-regex "End layers for \\([^\n]+\\)\n"
  "The delimiter to let us know know when a file is done being output.")

(defstruct layer begin end color message)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun augment-layer-from-plist (plist)
  "Make a layers struct from a plist."
  (make-layer :begin (string-to-number (first (split-string (getf plist :range) "\\.")))
	      :end (string-to-number (cadddr (split-string (getf plist :range) "\\.")))
	      :color (getf plist :color)
	      :message (getf plist :message)))

(defun augment-layers-from-file (filename)
  (let ((json-object-type 'plist))
    (mapcar #'augment-layer-from-plist
	    (json-read-file filename))))

(defun augment-layers-from-string (string)
  (let ((json-object-type 'plist))
    (mapcar #'augment-layer-from-plist
	    (json-read-from-string string))))

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
  ;; find the first layer that the point is between begin and end
  (layer-message (find point layers :test
		       (lambda (p l) (and (> p (layer-begin l))
				     (< p (layer-end l)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-minor-mode augment-mode
  "Major mode for showing code metadata.

\\{augment-mode-map}"

  :keymap (setq augment-mode-map (make-sparse-keymap))

  (define-key augment-mode-map (kbd "C-c C-s") 'augment-initiate)
  (define-key augment-mode-map (kbd "C-c C-k") 'augment-clear)
  
  (make-local-variable 'layers)

  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook 'augment-initiate)

  (add-to-list 'augmented-buffers (buffer-file-name))
  (augment-initiate (buffer-file-name)))

(defun augment-initiate (&optional file)
  (interactive)
  (setq layers nil)
  (augment-start-process)
  (process-send-string "augment" (concat (or file (buffer-file-name)) "\n")))

(defun augment-start-process ()
  (unless (get-process "augment") ;; only one should be running at a time
    (set-process-filter
     (start-process "augment" "*augment-out*" ;; FIXME: don't hardcode path
				       "/home/phil/projects/augment/bin/augment" "background")
     'augment-filter)))

(defun augment-filter (process output)
  (if (not (string-match augment-filter-file-regex output))
      ;; Haven't yet received all the file's layersn
      (if (string-match "^Error augmenting \\(.*\\)" output)
	  (progn (setq augment-incomplete-buffer "")
		 (error "Error augmenting %s." (match-string 1 output)))
	;; Push it on to the incomplete buffer
	(setq augment-incomplete-buffer (concat augment-incomplete-buffer output)))
    ;; Send it to the real filter
    (setq augment-incomplete-buffer "")
    (augment-filter-buffer process
			   (concat augment-incomplete-buffer output)
			   (file-name-nondirectory (match-string 1 output)))))

(defun augment-filter-buffer (process output &optional buffer)
  (with-current-buffer (substring buffer 0 -1)
    (setq out output)
    (setq layers (augment-layers-from-string (augment-strip-foot output)))
    (augment-buffer layers)))

(defun augment-strip-foot (output)
  (apply #'concat (subseq (split-string output "\n") 0 -2)))
  
(defun augment-buffer (layers)
  (augment-clear)
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