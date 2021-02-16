;;; mu4e-thread-folding -- -*- lexical-binding: t -*-

;; Copyright (C) 2021 Nicolas P. Rougier
;;
;; Author: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; Homepage: https://github.com/rougier/mu4e-thread-folding
;; Keywords: mu4e
;; Version: 0.2
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
(require 'svg-icon)

(defgroup mu4e-thread-folding '()
  "Group for mu4e thread folding options")


(defface mu4e-thread-folding-root-unfolded-face nil
  "Face for the root node thread when it is unfolded."
  :group 'mu4e-thread-folding)

(defface mu4e-thread-folding-root-folded-face nil
  "Face for the root node of a thread when it is folded."
  :group 'mu4e-thread-folding)

(defface mu4e-thread-folding-child-face nil
  "Face for a thread when it is unfolded (child node)"
  :group 'mu4e-thread-folding)


(defcustom mu4e-thread-folding-root-unfolded-prefix-string
  "▼ "
  ;; "▿ "
  ;; (svg-icon "material" "chevron-down"  nano-color-salient "#DCE6F9")
  "Prefix for the root node thread when it is unfolded."
  :type 'string
  :group 'mu4e-thread-folding)

(defcustom mu4e-thread-folding-root-folded-prefix-string
  "► "
  ;; "▸ "
  ;; (svg-icon "material" "chevron-right" nano-color-salient)
  "Prefix for the root node (when folded)"
  :type 'string
  :group 'mu4e-thread-folding)

(defcustom mu4e-thread-folding-child-prefix-string  "  "
  ;; (svg-icon "material" "chevron-right"  nano-color-salient)
  "Prefix for a child node."
  :type 'string
  :group 'mu4e-thread-folding)

(defcustom mu4e-thread-folding-root-prefix-position '(9 . 11)
  "Prefix position (columns) of a root node. 9 correspond to the first displayed columns."
  :type '(cons integer integer)
  :group 'mu4e-thread-folding)

(defcustom mu4e-thread-folding-child-prefix-position '(9 . 11)
  "Prefix position (columns) of a child node. 9 correspond to the first displayed columns."
  :type '(cons integer integer)
  :group 'mu4e-thread-folding)


(set-face-attribute 'mu4e-thread-folding-root-unfolded-face nil
                    :extend t
                    :overline nil
                    :underline nil ;; (if (display-graphic-p) "white" nil)
                    :foreground nil
                    :background "#DCE6F9")

(set-face-attribute 'mu4e-thread-folding-root-folded-face nil
                    :inherit 'default
                    :overline nil
                    :underline nil
                    :foreground nil
                    :background nil)

(set-face-attribute 'mu4e-thread-folding-child-face nil
                    :extend t
                    :underline nil ;; (if (display-graphic-p) "white" nil)
                    :foreground nil
                    :background "#EEF3FC")




(defun mu4e-headers-get-thread-id (msg)
  "Retrieve the thread-id of a msg.

This uses the mu4e private API and this might break in future releases.
Furthermore, the thread id does not seem to be very consistent."
  (mu4e~headers-get-thread-info msg 'thread-id))

(defun mu4e-headers-mark-threads ()
  "Mark line in headers view with various information contained in overlays."
  
  (interactive)
  (if (get-buffer "*mu4e-headers*")
      (with-current-buffer "*mu4e-headers*"

        ;; Remove all overlays
        (remove-overlays (point-min) (point-max))
        
        (let ((parent-thread-id)
              (unread-child nil)
              (parent-thread-overlay)
              (parent-thread-prefix-overlay))
          ;; Iterate over each header
          (mu4e-headers-for-each
           (lambda (msg)
             (let ((thread-id (mu4e-headers-get-thread-id msg))
                   (unread (member 'unread (mu4e-message-field msg :flags)))
                   ;; Overlay for child (prefix)
                   (child-thread-prefix-overlay
                    (make-overlay
                     (+ (car mu4e-thread-folding-child-prefix-position) (line-beginning-position))
                     (+ (cdr mu4e-thread-folding-child-prefix-position) (line-beginning-position))))
                   ;; Overlay for child (whole line)
                   (child-thread-overlay
                    (make-overlay
                     (+ 0 (line-beginning-position))
                     (+ 1 (line-end-position)))))
               
               ;; We mark the parent thread if and only if there'a child               
               (if (string= parent-thread-id thread-id)
                   (progn

                     ;; unread-child indicates that there's at least one unread child
                     (setq unread-child (or unread-child unread))
                     
                     ;; Child
                     (overlay-put child-thread-overlay 'face 'mu4e-thread-folding-child-face)
                     (if (not unread) (overlay-put child-thread-overlay 'invisible t))
                     ;; (overlay-put child-thread-overlay 'invisible t)
                     (overlay-put child-thread-overlay 'priority     -60)
                     (overlay-put child-thread-overlay 'unread       unread)
                     (overlay-put child-thread-overlay 'thread-child t)
                     (overlay-put child-thread-overlay 'thread-id    thread-id)
                     (overlay-put child-thread-prefix-overlay
                                  'display  mu4e-thread-folding-child-prefix-string)
                                          
                     ;; Root
                     (if unread-child
                         (overlay-put parent-thread-overlay 'face 'mu4e-thread-folding-root-unfolded-face)
                      (overlay-put parent-thread-overlay 'face 'mu4e-thread-folding-root-folded-face))
                     (overlay-put parent-thread-overlay 'priority -60)
                     (overlay-put parent-thread-overlay 'thread-parent t)
                     (overlay-put parent-thread-overlay 'thread-id thread-id)
                     (overlay-put parent-thread-prefix-overlay
                                  'display mu4e-thread-folding-root-folded-prefix-string))
                 
                 ;; Set the new parent (this relies on default message order in header's view)
                 (progn
                   (setq parent-thread-id thread-id
                         unread-child nil
                         parent-thread-overlay (make-overlay
                                                (+ 0 (line-beginning-position))
                                                (+ 1 (line-end-position)))
                         parent-thread-prefix-overlay
                         (make-overlay
                          (+ (car mu4e-thread-folding-root-prefix-position) (line-beginning-position))
                          (+ (cdr mu4e-thread-folding-root-prefix-position) (line-beginning-position)))))))))))))


(defun mu4e-headers-get-overlay (prop &optional index)
  "Get overlay at point having the PROP property"
  (let* ((index (or index 0))
        (overlays (overlays-at (+ (point) index)))
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
  (if (get-buffer "*mu4e-headers*")
      (with-current-buffer "*mu4e-headers*"
        (save-excursion
          (goto-char (point-min))

;;          (let ((overlay-root  nil)
;;                (overlay-child nil)
;;                (unread-child  nil)))

          (while (not (eobp))
            
;;            (setq overlay-child (mu4e-headers-get-overlay 'thread-child)
;;                  overlay-root  (mu4e-headers-get-overlay 'thread-parent))
            
            ;; Is current line a child
            (let ((overlay (mu4e-headers-get-overlay 'thread-child)))
              (when overlay
                (let ((id (overlay-get overlay 'thread-id))
                      (unread (overlay-get overlay 'unread)))
                  (if (and (not unread) (or (not thread-id)
                                            (string= id thread-id)))
                      (overlay-put overlay 'invisible value)))))

            ;; Is current line a root
            (let ((overlay (mu4e-headers-get-overlay 'thread-parent)))
              (when overlay
                (let ((id (overlay-get overlay 'thread-id))
                      (overlay-prefix (mu4e-headers-get-overlay
                                       'display (car mu4e-thread-folding-root-prefix-position))))
                  (when (and overlay (or (not thread-id) (string= id thread-id)))
                    (overlay-put overlay-prefix 'display (if value
                                                             mu4e-thread-folding-root-folded-prefix-string
                                                           mu4e-thread-folding-root-unfolded-prefix-string))
                    (if value
                        (overlay-put overlay 'face 'mu4e-thread-folding-root-folded-face)
                      (overlay-put overlay 'face 'mu4e-thread-folding-root-unfolded-face))))))
            (forward-line 1))))))

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
  (if (get-buffer "*mu4e-headers*")
      (with-current-buffer "*mu4e-headers*"
        (let ((overlay (mu4e-headers-get-overlay 'thread-id)))
          (mu4e-headers-overlay-set-visibility t (overlay-get overlay 'thread-id))))))

(defun mu4e-headers-unfold-at-point ()
  "Unfold current thread at point"
  (interactive)
  (if (get-buffer "*mu4e-headers*")
      (with-current-buffer "*mu4e-headers*"
        (let ((overlay (mu4e-headers-get-overlay 'thread-id)))
          (mu4e-headers-overlay-set-visibility nil (overlay-get overlay 'thread-id))))))

;; Install hooks and  keybindings
(add-hook 'mu4e-index-updated-hook #'mu4e-headers-mark-threads)
(add-hook 'mu4e-headers-found-hook #'mu4e-headers-mark-threads)
(define-key mu4e-headers-mode-map (kbd "<left>") 'mu4e-headers-fold-at-point)
(define-key mu4e-headers-mode-map (kbd "<S-left>") 'mu4e-headers-fold-all)
(define-key mu4e-headers-mode-map (kbd "<right>") 'mu4e-headers-unfold-at-point)
(define-key mu4e-headers-mode-map (kbd "<S-right>") 'mu4e-headers-unfold-all)

(provide 'mu4e-thread-folding)
