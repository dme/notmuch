;; notmuch-test.el --- testing the emacs interface
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

(defvar notmuch-test-tests '(notmuch-test-show-advance-simple
			     notmuch-test-show-advance-twice
			     notmuch-test-show-advance-rewind
			     notmuch-test-show-advance-twice-rewind
			     notmuch-test-show-advance-not-eobp
			     notmuch-test-show-advance-eobp)
  "A list of tests.")

(defun notmuch-test-all ()
  "Wrapper for ease of test development."
  (let ((result (get-buffer-create "*notmuch-test*")))
    (set-buffer result)
    (erase-buffer)
    (dolist (test notmuch-test-tests)
      (insert (format "%40S: %S\n"
		      test
		      (save-excursion
			(funcall test)))))
    (switch-to-buffer result)))

;;

(defun notmuch-test-show-advance-simple ()
  ;; Find a particular thread.
  (notmuch-show "id:20091117190054.GU3165@dottiness.seas.harvard.edu")
  ;; Ensure that all messages are open.
  (notmuch-show-open-or-close-all)
  (redisplay t)
  ;; Advance.
  (notmuch-show-advance)
  (redisplay t)
  ;; The first message in the thread is longer than a screen, so we
  ;; should just have scrolled normally.
  (if (= (line-number-at-pos) 19)
      t
    (cons (line-number-at-pos) 19)))

(defun notmuch-test-show-advance-twice ()
  ;; Find a particular thread.
  (notmuch-show "id:20091117190054.GU3165@dottiness.seas.harvard.edu")
  ;; Ensure that all messages are open.
  (notmuch-show-open-or-close-all)
  (redisplay t)
  ;; Advance twice.
  (notmuch-show-advance)
  (redisplay t)
  (notmuch-show-advance)
  (redisplay t)
  ;; The first message in the thread is shorter than two screens, so
  ;; we should be at the start of the second message.
  (if (= (line-number-at-pos) 41)
      t
    (cons (line-number-at-pos) 41)))

(defun notmuch-test-show-advance-rewind ()
  ;; Find a particular thread.
  (notmuch-show "id:20091117190054.GU3165@dottiness.seas.harvard.edu")
  ;; Ensure that all messages are open.
  (notmuch-show-open-or-close-all)
  (redisplay t)
  ;; Advance.
  (notmuch-show-advance)
  (redisplay t)
  ;; Rewind.
  (notmuch-show-rewind)
  (redisplay t)
  ;; Should be back at the start.
  (if (= (line-number-at-pos) 1)
      t
    (cons (line-number-at-pos) 1)))

(defun notmuch-test-show-advance-twice-rewind ()
  ;; Find a particular thread.
  (notmuch-show "id:20091117190054.GU3165@dottiness.seas.harvard.edu")
  ;; Ensure that all messages are open.
  (notmuch-show-open-or-close-all)
  (redisplay t)
  ;; Advance twice.
  (notmuch-show-advance)
  (redisplay t)
  (notmuch-show-advance)
  (redisplay t)
  ;; Rewind.
  (notmuch-show-rewind)
  (redisplay t)
  ;; The first message in the thread is shorter than two screens, so
  ;; we should be in the middle of it.
  (if (= (line-number-at-pos) 17)
      t
    (cons (line-number-at-pos) 17)))

(defun notmuch-test-show-advance-not-eobp ()
  ;; Find a particular thread.
  (notmuch-show "id:20091117190054.GU3165@dottiness.seas.harvard.edu")
  ;; From here `advance' should never cause us to leave the buffer.
  (not (notmuch-show-advance)))

(defun notmuch-test-show-advance-eobp ()
  ;; Find a particular thread.
  (notmuch-show "id:20091117190054.GU3165@dottiness.seas.harvard.edu")
  (goto-char (point-max))
  ;; From here `advance' should always cause us to leave the buffer.
  (notmuch-show-advance))

;;

(provide 'notmuch-test)
