;; notmuch-test.el --- ERT based tests for notmuch.el.
;;
;; Copyright © David Edmondson
;;
;; This file is part of Notmuch.
;;
;; Notmuch is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Notmuch is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Notmuch.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Authors: David Edmondson <dme@dme.org>

(require 'ert)

;;

(defun notmuch-test-file-as-string (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

;; This tends not to work well except in batch mode.
(defun notmuch-test-expected-result (name)
  "Get the expected result of a test as a string."
  (notmuch-test-file-as-string
   (concat (getenv "EXPECTED") "/" name)))

(defun notmuch-test-buffer-result (fn)
  (funcall fn)
  (buffer-substring-no-properties (point-min) (point-max)))

;;

(require 'notmuch-maildir-fcc)

(ert-deftest notmuch-fcc-trivial ()
  "One Fcc folder."
  (should (string= "sent"
		   (notmuch-fcc-determine-folder "sent" "foo@bar.baz"))))

(ert-deftest notmuch-fcc-simple ()
  "Fcc folder matching single address."
  (should (string= "sent"
		   (notmuch-fcc-determine-folder
		    '(("foo@bar.baz" . "sent"))
		    "foo@bar.baz"))))

(ert-deftest notmuch-fcc-none ()
  "No matching Fcc folder."
  (should (eq nil
	      (notmuch-fcc-determine-folder
	       '(("foo@bar.baz" . "sent"))
	       "not@bar.baz"))))

(ert-deftest notmuch-fcc-multiple ()
  "Choose the correct folder from two options."
  (let ((fcc-config '(("foo@bar.baz" . "foosent")
		      ("bar@bar.baz" . "barsent"))))

    (should (string= "foosent"
		     (notmuch-fcc-determine-folder
		      fcc-config "foo@bar.baz")))

    (should (string= "barsent"
		     (notmuch-fcc-determine-folder
		      fcc-config "bar@bar.baz")))))

(ert-deftest notmuch-fcc-regexp ()
  "Allow a regexp in the `From' string."
  (let ((fcc-config '(("foo@bar.baz" . "foosent")
		      (".*" . "barsent"))))

    (should (string= "foosent"
		     (notmuch-fcc-determine-folder
		      fcc-config "foo@bar.baz")))
		     
    (should (string= "barsent"
		     (notmuch-fcc-determine-folder
		      fcc-config "bar@bar.baz")))))

(ert-deftest notmuch-fcc-oldstyle ()
  "Raise an error for an old style configuration."

  ;; No match.
  (should-error (notmuch-fcc-determine-folder
		 '("sent" ("not@bar.baz" . "foosent"))
		 "foo@bar.baz"))
  ;; One match.
  (should-error (notmuch-fcc-determine-folder
		 '("sent" ("not@bar.baz" . "foosent"))
		 "not@bar.baz")))
  

;;

(require 'notmuch-hello)

(ert-deftest notmuch-hello ()
  "Check that `notmuch-hello' outputs correct data.

Presumes that the email corpus is already present."

  (should (string= (notmuch-test-buffer-result 'notmuch-hello)
		   (notmuch-test-expected-result "notmuch-hello"))))

(ert-deftest notmuch-hello-reflect ()
  (should (equal '(1 4 7 10 2 5 8 11 3 6 9 12)
		 (notmuch-hello-reflect '(1 2 3 4 5 6 7 8 9 10 11 12) 4)))

  (should (equal '(1 4 7 10 2 5 8 nil 3 6 9 nil)
		 (notmuch-hello-reflect '(1 2 3 4 5 6 7 8 9 10) 4))))

;;

(require 'notmuch-show)

(ert-deftest notmuch-show-strip-re ()
  (mapc '(lambda (test)
	   (should (string= "fish"
			    (notmuch-show-strip-re test))))
	'("fish" "re: fish" "Re: fish" "RE: fish"))

  (mapc '(lambda (test)
	   (should (string= "some fish"
			    (notmuch-show-strip-re test))))
	'("some fish" "re: some fish" "some re: fish"))

  (mapc '(lambda (test)
	   (should-not (string= "some fish"
				(notmuch-show-strip-re test))))
	'(" some fish" "re: some fish " "somere: fish" "some fish re:")))

;; 

(defun notmuch-test-batch ()
  "Run the notmuch ERT tests in batch mode."

  ;; Avoid the 10 column default of `emacs --batch'.
  (set-frame-width (window-frame (get-buffer-window)) 80)

  (ert-run-tests-batch-and-exit))
