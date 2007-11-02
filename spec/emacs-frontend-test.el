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
(require 'flymake)

;; in case it hasn't been properly installed
(add-to-list 'exec-path (expand-file-name "../bin"))

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

(deftest augment-file-path augment-suite
  (assert-equal "/foo/bar/.augment/baz.rb" (augment-file-path "/foo/bar/baz.rb")))
  
(deftest render-layers augment-suite
  "Rendering layers should create overlays in a buffer."
  (with-test-buffer
    ;; Fill the buffer with some garbage
    (dotimes (i 5) (insert "hello world.\n"))
    (augment-render-layer (augment-layer-from-plist (list :message "hello"
							  :color "red"
							  :range "0...10")))
    (assert-overlay 1)
    (assert-overlay 9)))

(deftest layer-message augment-suite
  "Finding message at point should get the message of the layer the point is in."
  (let* ((json-object-type 'plist)
	 (json-array-type 'list)
	 (layers (mapcar #'augment-layer-from-plist
			 (json-read-file "fixtures/layers.json"))))
    (assert-equal "cons" (augment-show-message 5))
    (assert-equal "car" (augment-show-message 16))
    (assert-equal "cdr" (augment-show-message 29))))

(deftest augment-filter augment-suite
  (with-test-buffer
    (make-local-variable 'layers)
    (dotimes (i 3) (insert "hello world\n"))
    (augment-filter nil (flymake-read-file-to-string "fixtures/augment-output.txt"))
    (assert-overlay 2)))

(elunit "augment-suite")