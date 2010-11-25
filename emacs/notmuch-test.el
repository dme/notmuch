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

(defun notmuch-test ()
  "Run the notmuch ERT tests."
  (interactive)

  (ert t))

(defun notmuch-test-batch ()
  "Run the notmuch ERT tests in batch mode."
  (ert-run-tests-batch-and-exit))
