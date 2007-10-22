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

(deftest layer-from-plist augment-suite
  "The layer struct should populated from a plist."
  (let ((layer (augment-layer-from-plist (list :message "message" :color "color"
					       :range "221...226"))))
    (assert-equal 221 (layer-begin layer))
    (assert-equal 226 (layer-end layer))
    (assert-equal "color" (layer-color layer))
    (assert-equal "message" (layer-message layer))))

(deftest layer-from-string augment-suite
  "The layer struct should populated from a JSON string."
  (let ((layer (first (augment-layers-from-string
		       "[{\"message\":\"message\", \"color\":\"color\", \"range\":\"1...10\"}]"))))
		       
    (assert-equal 1 (layer-begin layer))
    (assert-equal 10 (layer-end layer))
    (assert-equal "color" (layer-color layer))
    (assert-equal "message" (layer-message layer))))

(deftest layer-file-load augment-suite
  "Ensure that a whole layer file loads properly."
  (let ((layers (augment-layers-from-file "fixtures/layers.json")))
    (assert-equal 3 (length layers))
    (assert-equal "cons" (layer-message (pop layers)))
    (assert-equal "car" (layer-message (pop layers)))
    (assert-equal "cdr" (layer-message (pop layers)))))

(deftest render-layers augment-suite
  "Rendering layers should create overlays in a buffer."
  (with-output-to-temp-buffer "*augment-test*"
    ;; Fill the buffer with some garbage
    (dotimes (i 5) (princ "hello world.\n"))
    (augment-render-layer (augment-layer-from-plist (list :message "hello"
							  :color "red"
							  :range "0...10")))
    (assert-overlay 1)
    (assert-overlay 9))
  (kill-buffer "*augment-test*"))

(deftest layer-message augment-suite
  "Finding message at point should get the message of the layer the point is in."
  (let ((layers (augment-layers-from-file "fixtures/layers.json")))
    (assert-equal "cons" (augment-message-at-point 5))
    (assert-equal "car" (augment-message-at-point 12))
    (assert-equal "cdr" (augment-message-at-point 22))))

(elunit "augment-suite")