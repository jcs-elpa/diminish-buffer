;;; diminish-buffer.el --- Diminished buffers from being shown in buffer menu.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shen, Jen-Chieh
;; Created date 2019-08-31 00:02:54

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Diminished buffers from being shown in buffer menu.
;; Keyword: diminish hide buffer menu
;; Version: 0.0.3
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/jcs090218/diminish-buffer

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Diminished buffers from being shown in buffer menu.
;;

;;; Code:


(defgroup diminish-buffer nil
  "Diminished buffers from being shown in buffer menu."
  :prefix "diminish-buffer-"
  :group 'convenience
  :link '(url-link :tag "Repository" "https://github.com/jcs090218/diminish-buffer"))


(defcustom diminish-buffer-list '("*helm")
  "List of buffer that you want to hide in the buffer."
  :type 'list
  :group 'diminish-buffer)


(defun diminish-buffer--is-contain-list-string-regexp (in-list in-str)
  "Check if a string IN-STR contain in any string in the string list IN-LIST."
  (cl-some #'(lambda (lb-sub-str) (string-match-p lb-sub-str in-str)) in-list))

;;;###autoload
(defun diminish-buffer-clean ()
  "Do the diminish action for `buffer-menu'."
  (interactive)
  (when (string= (buffer-name) "*Buffer List*")
    (save-excursion
      (while (< (line-number-at-pos) (line-number-at-pos (point-max)))
        (let ((buf-name (elt (tabulated-list-get-entry) 3)))
          (if (and (stringp buf-name)
                   (diminish-buffer--is-contain-list-string-regexp diminish-buffer-list buf-name))
              (tabulated-list-delete-entry)
            (forward-line 1)))))))

(defun diminish-buffer--refresh-buffer-menu ()
  "Refresh buffer menu at time when enabled/disabled."
  (save-window-excursion
    (let ((inhibit-message t)
          (message-log-max nil))
      (buffer-menu))
    (bury-buffer)))

(defun diminish-buffer--buffer-menu--advice-after (&rest _)
  "Advice after execute `buffer-menu' command."
  (diminish-buffer-clean))

(defun diminish-buffer--tabulated-list-revert--advice-after (&rest _)
  "Advice run after execute `tabulated-list-revert' command."
  (diminish-buffer-clean))


(defun diminish-buffer--enable ()
  "Enable `diminish-buffer'."
  (advice-add 'buffer-menu :after #'diminish-buffer--buffer-menu--advice-after)
  (advice-add 'tabulated-list-revert :after #'diminish-buffer--tabulated-list-revert--advice-after)
  (diminish-buffer--refresh-buffer-menu))

(defun diminish-buffer--disable ()
  "Disable `diminish-buffer'."
  (advice-remove 'buffer-menu #'diminish-buffer--buffer-menu--advice-after)
  (advice-remove 'tabulated-list-revert #'diminish-buffer--tabulated-list-revert--advice-after)
  (diminish-buffer--refresh-buffer-menu))

;;;###autoload
(define-minor-mode diminish-buffer-mode
  "Minor mode 'diminish-buffer-mode'."
  :global t
  :require 'diminish-buffer
  :group 'diminish-buffer
  (if diminish-buffer-mode
      (diminish-buffer--enable)
    (diminish-buffer--disable)))


(provide 'diminish-buffer)
;;; diminish-buffer.el ends here
