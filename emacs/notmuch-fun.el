;; notmuch-fun.el --- Fun addons for notmuch.el
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

;;

(require 'gravatar)
(require 'mail-parse)

(defun notmuch-fun-insert-gravatar (image buffer position)
  (when (and (buffer-live-p buffer)
	     (not (eq image 'error)))
    (let ((inhibit-read-only t))
      (goto-char position)
      (insert "\n")
      (insert (make-string (notmuch-show-get-depth) ? ))
      (insert-image image))))

(defun notmuch-fun-add-gravatar ()
  (let ((parsed-from (mail-header-parse-address (notmuch-show-get-from))))
    (when (car parsed-from)
      (gravatar-retrieve (car parsed-from) 'notmuch-fun-insert-gravatar
			 (list (current-buffer)
			       (save-excursion
				 (end-of-line)
				 (point)))))))

(defun notmuch-fun-add-gravatars ()
  (save-excursion
    (goto-char (point-min))

    (loop do (notmuch-fun-add-gravatar)
	  while (notmuch-show-goto-message-next))))

;;

(provide 'notmuch-fun)
