;;; mu4e-thread-folding -- -*- lexical-binding: t -*-

;; Copyright (C) 2020 Nicolas P. Rougier
;;
;; Author: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; Homepage: https://github.com/rougier/mu4e-thread-folding
;; Keywords: mu4e
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'mu4e)
(require 'color)

(defvar mu4e-headers-thread-base-color "#673AB7")

(defface mu4e-headers-thread-parent-face nil
  "Face for mu4e thread parent in headers view")
(set-face-attribute 'mu4e-headers-thread-parent-face nil
                    :extend t
                    :background (color-lighten-name
                                 mu4e-headers-thread-base-color 45))

(defface mu4e-headers-thread-child-face nil
  "Face for mu4e thread child in headers view")
(set-face-attribute 'mu4e-headers-thread-child-face nil
                    :extend t
                    :background (color-lighten-name
                                 mu4e-headers-thread-base-color  50))

(defun mu4e-headers-get-thread-id (msg)
  "Retrieve the thread-id of a msg.

This uses the mu4e private API and this might break in future releases.
Furthermore, the thread id does not seem to be very consistent."
  (mu4e~headers-get-thread-info msg 'thread-id))

(defun mu4e-headers-mark-threads ()
  "Mark line in headers view with various information contained in overlays."
  
  (interactive)
  (with-current-buffer "*mu4e-headers*"
    (remove-overlays (point-min) (point-max))
    (let ((parent-thread-id)
          (parent-thread-overlay)
          (parent-thread-prefix-overlay))
      (mu4e-headers-for-each
       (lambda (msg)
         (let ((thread-id (mu4e-headers-get-thread-id msg))
               (unread (member 'unread (mu4e-message-field msg :flags)))
               (child-thread-overlay (make-overlay
                                      (line-beginning-position)
                                      (+ 1 (line-end-position)))))
           ;; We mark the parent thread if and only if there'a child
           (if (string= parent-thread-id thread-id)
               (progn
                 (overlay-put child-thread-overlay 'face 'mu4e-headers-thread-child-face)
                 (overlay-put child-thread-overlay 'priority -60)
                 (overlay-put child-thread-overlay 'unread unread)
                 (overlay-put child-thread-overlay 'thread-child t)
                 (overlay-put child-thread-overlay 'thread-id thread-id)

                 (overlay-put parent-thread-overlay 'face 'mu4e-headers-thread-parent-face)
                 (overlay-put parent-thread-overlay 'priority -60)
                 (overlay-put parent-thread-overlay 'thread-parent t)
                 (overlay-put parent-thread-overlay 'thread-id thread-id)))
             ;; Set the new parent (this relies on default message order in header's view)
             (setq parent-thread-id thread-id
                 parent-thread-overlay (make-overlay
                                        (line-beginning-position)
                                        (+ 1 (line-end-position)))))))))))


(defun mu4e-headers-get-overlay (prop)
  "Get overlay at point having the PROP property"
  (let ((overlays (overlays-at (point)))
        found)
    (while (and overlays (not found))
      (let ((overlay (car overlays)))
        (if (overlay-get overlay prop)
            (setq found overlay)))
      (setq overlays (cdr overlays)))
    found))

(defun mu4e-headers-overlay-set-visibility (value &optional thread-id)
  "Set the invisible property for all thread children or only the ones matching thread-id.
Unread message are not folded."
  (interactive)
  (with-current-buffer "*mu4e-headers*"
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((overlay (mu4e-headers-get-overlay 'thread-child)))
          (when overlay
            (let ((id (overlay-get overlay 'thread-id))
                  (unread (overlay-get overlay 'unread)))
              (if (and (not unread) (or (not thread-id)
                                        (string= id thread-id)))
                  (overlay-put overlay 'invisible value)))))
        (forward-line 1)))))

(defun mu4e-headers-fold-all ()
  "Fold all threads"
  (interactive)
  (mu4e-headers-overlay-set-visibility t))

(defun mu4e-headers-unfold-all ()
  "Unfold all threads"
  (interactive)
  (mu4e-headers-overlay-set-visibility nil))
  
(defun mu4e-headers-fold-at-point ()
  "Fold current thread at point"
  (interactive)
  (with-current-buffer "*mu4e-headers*"
    (let ((overlay (mu4e-headers-get-overlay 'thread-id)))
      (mu4e-headers-overlay-set-visibility t (overlay-get overlay 'thread-id)))))

(defun mu4e-headers-unfold-at-point ()
  "Unfold current thread at point"
  (interactive)
  (with-current-buffer "*mu4e-headers*"
    (let ((overlay (mu4e-headers-get-overlay 'thread-id)))
      (mu4e-headers-overlay-set-visibility nil (overlay-get overlay 'thread-id)))))

;; Install hooks and  keybindings
(add-hook 'mu4e-index-updated-hook #'mu4e-headers-mark-threads)
(add-hook 'mu4e-headers-found-hook #'mu4e-headers-mark-threads)
(define-key mu4e-headers-mode-map (kbd "<left>") 'mu4e-headers-fold-at-point)
(define-key mu4e-headers-mode-map (kbd "<S-left>") 'mu4e-headers-fold-all)
(define-key mu4e-headers-mode-map (kbd "<right>") 'mu4e-headers-unfold-at-point)
(define-key mu4e-headers-mode-map (kbd "<S-right>") 'mu4e-headers-unfold-all)

(provide 'mu4e-thread-folding)
