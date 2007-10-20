;;; augment.el --- Display metadata about code

;; Copyright (C) 2007 Phil Hagelberg

;; Author: Phil Hagelberg <technomancy@gmail.com>
;; Created: 19 Oct 2007
;; Version: 0.1
;; Keywords: augment testing metadata

;; This file is part of the Augment system:
;; http://augment.rubyforge.org

(require 'elunit) ;; See http://www.emacswiki.org/cgi-bin/wiki/ElUnit
(require 'augment)

(elunit-clear-suites)
(defsuite augment-suite nil)

(deftest layer-from-json augment-suite
  "The layer struct should populated from JSON."
  (let ((layer (augment-layer-from-plist (list :message "message" :color "color"
					       :range "221...226"))))
    (assert-equal 221 (layer-begin layer))
    (assert-equal 226 (layer-end layer))
    (assert-equal "color" (layer-color layer))
    (assert-equal "message" (layer-message layer))))

(deftest layer-file-load augment-suite
  "Ensure that a whole layer file loads properly."
  (let ((layers (augment-layers-from-file "fixtures/layers.json")))
    (assert-equal 3 (length layers))
    (assert-equal "cons" (layer-message (pop layers)))
    (assert-equal "car" (layer-message (pop layers)))
    (assert-equal "cdr" (layer-message (pop layers)))))

;; (deftest render-layers augment-suite
;;   "Rendering layers should create overlays in a buffer."
;;   (with-output-to-temp-buffer "*augment-test*"
;;     ;; Fill the buffer with some garbage
;;     (dotimes (i 5) (princ "hello world.\n"))
;;     (augment-render-layers (augment-layers-from-file "fixtures/layers.json"))
;;     (assert-overlay nil)
;;     (assert-overlay nil)
;;     (assert-overlay nil)))

;; (deftest layer-message augment-suite
;;   "Finding message at point should get the message of the layer the point is in."
;;   (with-output-to-temp-buffer "*augment-test*"
;;     ;; Fill the buffer with some garbage
;;     (dotimes (i 5) (princ "hello world.\n"))
;;     (augment-render-layers (augment-layers-from-file "fixtures/layers.json"))
;;     (goto-char 30)
;;     (assert-equal "cons" (augment-message-at-point))))

;; (deftest watching augment-suite
;;   (let ((original-file "fixtures/drinks/lib/drink.rb")
;; 	(layer-file "fixtures/drinks/lib/.augment/drink.rb"))
;;     (shell-command (concat "../bin/augment color " original-file))))

(elunit "augment-suite")