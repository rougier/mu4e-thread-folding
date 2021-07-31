;;; mu4e-thread-folding.el --- Thread folding support for mu4e -*- lexical-binding: t -*-

;; Copyright (C) 2021 Nicolas P. Rougier
;;
;; Author: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; Homepage: https://github.com/rougier/mu4e-thread-folding
;; Keywords: mail
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

;; mu4e-thread-folding.el is a small library to enable threads folding in
;; mu4e. This works by using overlays with an invisible property and
;; setting hooks at the right place. It is possible to configure colors
;; to better highlight a thread and also to have a prefix string
;; indicating if a thread is folded or not. Note that when a thread is
;; folded, any unread child remains visible.

;; Usage:

;; The prefix string is displayed over the header line and it is thus
;; recommended to have an empty field at the start of an header line.
;; Have a look at ~mu4e-headers-fields~.
;;
;; (require 'mu4e-thread-folding)
;; (add-to-list 'mu4e-header-info-custom
;;              '(:empty . (:name "Empty"
;;                          :shortname ""
;;                          :function (lambda (msg) "  "))))
;; (setq mu4e-headers-fields '((:empty         .    2)
;;                             (:human-date    .   12)
;;                             (:flags         .    6)
;;                             (:mailing-list  .   10)
;;                             (:from          .   22)
;;                             (:subject       .   nil)))


;;; Code:
(require 'mu4e)

(defvar mu4e-thread-folding-mode nil)
(defvar mu4e-headers--folded-items nil)

(defgroup mu4e-thread-folding '()
  "Group for mu4e thread folding options"
  :group 'mu4e)

(defface mu4e-thread-folding-root-unfolded-face
  `((t :inherit 'default))
  "Face for the root node thread when it is unfolded."
  :group 'mu4e-thread-folding)

(defface mu4e-thread-folding-root-folded-face
  `((t :inherit 'default))
  "Face for the root node of a thread when it is folded."
  :group 'mu4e-thread-folding)

(defface mu4e-thread-folding-child-face
  `((t :inherit 'default))
  "Face for a thread when it is unfolded (child node)"
  :group 'mu4e-thread-folding)

(defface mu4e-thread-folding-root-prefix-face
  `((t :inherit default))
  "Face for the root node thread when it is unfolded."
  :group 'mu4e-thread-folding)

(defcustom mu4e-thread-folding-default-view 'folded
  "Initial folding status ('folded or 'unfolded)."
  :type 'string
  :group 'mu4e-thread-folding)

(defcustom mu4e-thread-folding-root-unfolded-prefix-string
  "[%2d] ▾"
  "Prefix for the root node thread when it is unfolded."
  :type 'string
  :group 'mu4e-thread-folding)

(defcustom mu4e-thread-folding-root-folded-prefix-string
  "[%2d] ▸"
  "Prefix for the root node (when folded)"
  :type 'string
  :group 'mu4e-thread-folding)

(defcustom mu4e-thread-folding-child-prefix-string
  " "
  "Prefix for a child node."
  :type 'string
  :group 'mu4e-thread-folding)

(defvar mu4e-thread-folding-all-folded nil
  "Record whether last fold-all state was folded.")

(defun mu4e-headers-get-thread-id (msg)
  "Retrieve the thread-id of a msg.
This uses the mu4e private API and this might break in future releases."
  (mu4e~headers-get-thread-info msg 'thread-id))


(defun mu4e-headers-mark-threads (&optional no-reset)
  "Mark line in headers view with various information contained in overlays."
  (when (and (get-buffer "*mu4e-headers*") mu4e-headers-show-threads)
    (with-current-buffer "*mu4e-headers*"
      (setq-local line-move-visual t
                  line-move-ignore-invisible t)
      ;; turn on minor mode for key bindings
      (unless mu4e-thread-folding-mode (mu4e-thread-folding-mode 1))
      ;; Remove all overlays
      (cl-loop with names = '(thread-child thread-root root-prefix)
               for ov being the overlays
               when (cl-loop for name in names
                             thereis (overlay-get ov name))
               do (delete-overlay ov))
      (unless no-reset (setq mu4e-headers--folded-items nil))
      (setq-local left-margin-width 1)
      (if (get-buffer-window "*mu4e-headers*")
          (set-window-margins (get-buffer-window "*mu4e-headers*")
                              (max (length mu4e-thread-folding-root-folded-prefix-string)
                                   (length mu4e-thread-folding-root-unfolded-prefix-string))))
      (let ((overlay-priority     -60)
            (folded               (string= mu4e-thread-folding-default-view 'folded))
            (child-face           'mu4e-thread-folding-child-face)
            (children-number      1)
            (root-id              nil)
            (root-overlay         nil)
            (root-unread-child    nil)
            docid-overlay
            (root-folded-face     'mu4e-thread-folding-root-folded-face)
            (root-unfolded-face   'mu4e-thread-folding-root-unfolded-face)
            (root-folded-prefix   mu4e-thread-folding-root-folded-prefix-string)
            (root-unfolded-prefix mu4e-thread-folding-root-unfolded-prefix-string))
        ;; store initial folded state
        (setq mu4e-thread-folding-all-folded folded)
        (setq-local buffer-invisibility-spec '(docid t))
        ;; Iterate over each header
        (mu4e-headers-for-each
         (lambda (msg)
           (let* ((docid (mu4e-message-field msg :docid))
                  (docid-pos (cons (mu4e~headers-goto-docid docid)
                                   (mu4e~headers-goto-docid docid t)))
                  (id     (mu4e-headers-get-thread-id msg))
                  (flagged (member 'flagged (mu4e-message-field msg :flags)))
                  (unread (member 'unread (mu4e-message-field msg :flags)))
                  (child-overlay (make-overlay
                                  (line-beginning-position)
                                  (+ 1 (line-end-position)))))
;;             (setq folded (or (and (member id mu4e-headers--folded-items) t)
;;                              mu4e-thread-folding-all-folded))
             (setq folded (member id mu4e-headers--folded-items))
             ;; We mark the root thread if and only if there's child
             (if (string= root-id id)
                 (progn
                   (setq children-number (+ children-number 1))
                   
                   ;; unread-child indicates that there's at least one unread child
                   (setq root-unread-child (or root-unread-child unread))
                   ;; Child
                   (when (and (not unread) (not flagged))
                     (overlay-put child-overlay 'face child-face))
                   (overlay-put child-overlay 'invisible (and folded (not unread)))
                   (overlay-put child-overlay 'priority overlay-priority)
                   (overlay-put child-overlay 'unread unread)
                   (overlay-put child-overlay 'thread-child t)
                   (overlay-put child-overlay 'thread-id id)
                   ;; Root
                   (overlay-put
                    root-overlay 'face (if (or root-unread-child (not folded))
                                           root-unfolded-face
                                         root-folded-face))
                   (overlay-put root-overlay 'thread-root t)
                   (overlay-put root-overlay 'thread-id id)
                   (overlay-put root-overlay 'folded folded)
                   (overlay-put root-overlay 'priority overlay-priority)
                   (overlay-put root-overlay 'invisible 'root)
                   (overlay-put root-overlay 'prefix-docid docid-overlay)
                   (overlay-put
                    docid-overlay 'before-string
                    (propertize
                     " " 'display
                     `((margin left-margin)
                       ,(propertize
                         (if (or root-unread-child (not folded))
                             (format root-unfolded-prefix children-number)
                           (format root-folded-prefix children-number))
                         'face 'mu4e-thread-folding-root-prefix-face))))
                   (overlay-put docid-overlay 'invisible 'docid)
                   (overlay-put docid-overlay 'priority 1)
                   (overlay-put docid-overlay 'root-prefix t))
               ;; Else, set the new root (this relies on default message order in header's view)
               (progn
                 (if (> children-number 1)
                     (overlay-put root-overlay 'children-number children-number))
                 (setq root-id id
                     root-unread-child nil
                     children-number 1
                     root-overlay (make-overlay
                                   (line-beginning-position)
                                   (1+ (line-end-position)))
                     docid-overlay (make-overlay
                                    (car docid-pos)
                                    (cdr docid-pos))))))))))))

(defun mu4e-headers-mark-threads-no-reset ()
  "Same as `mu4e-headers-mark-threads' but don't reset `mu4e-headers--folded-items'."
  (mu4e-headers-mark-threads 'no-reset))

(defun mu4e-headers-overlay-set-visibility (value &optional thread-id)
  "Set the invisible property for all thread children or only the ones matching thread-id.
Unread message are not folded."
  (when (and (get-buffer "*mu4e-headers*") mu4e-headers-show-threads)
    (with-current-buffer "*mu4e-headers*"
      (unless thread-id
        (setq mu4e-thread-folding-all-folded value))
      (save-excursion
        (goto-char (point-min))
        (let ((root-overlay  nil)
              (child-overlay nil)
              (root-folded-face 'mu4e-thread-folding-root-folded-face)
              (root-unfolded-face 'mu4e-thread-folding-root-unfolded-face)
              (root-folded-prefix mu4e-thread-folding-root-folded-prefix-string)
              (root-unfolded-prefix mu4e-thread-folding-root-unfolded-prefix-string))
          
          (mu4e-headers-for-each
           (lambda (_msg)
             (let (local-child-overlay local-root-overlay)
               (cl-loop for ov in (overlays-in (point-at-bol) (point-at-eol))
                        when (overlay-get ov 'thread-child)
                        do (setq local-child-overlay ov)
                        when (overlay-get ov 'thread-root)
                        do (setq local-root-overlay ov))
               ;; Child
               (when local-child-overlay
                 (let ((id     (overlay-get local-child-overlay 'thread-id))
                       (unread (overlay-get local-child-overlay 'unread)))
                   (setq child-overlay local-child-overlay)
                   (when (or (not thread-id) (string= id thread-id))
                     (if (and root-overlay unread)
                         (overlay-put root-overlay 'face root-unfolded-face)
                       (overlay-put child-overlay 'invisible value)))))
               ;; Root
               (when local-root-overlay
                 (let ((children-number (or (overlay-get local-root-overlay 'children-number) 1))
                       (id (overlay-get local-root-overlay 'thread-id)))
                   (setq root-overlay local-root-overlay)
                   (when (or (not thread-id) (string= id thread-id))
                     (if (and (overlay-get root-overlay 'folded) (null value))
                         (setq mu4e-headers--folded-items
                               (delete id mu4e-headers--folded-items))
                       (push id mu4e-headers--folded-items))
                     (overlay-put root-overlay 'folded value)
                     (overlay-put
                      (overlay-get root-overlay 'prefix-docid) 'before-string
                      (propertize
                       " " 'display
                       `((margin left-margin)
                         ,(propertize
                           (if value
                               (format root-folded-prefix children-number)
                             (format root-unfolded-prefix children-number))
                           'face 'mu4e-thread-folding-root-prefix-face))))
                     (overlay-put
                      root-overlay 'face (if value
                                             root-folded-face
                                           root-unfolded-face)))))
               ;; Not a root, not a child, we reset the root overlay
               (when (and (not local-child-overlay) (not local-root-overlay))
                 (setq root-overlay nil))))))))))

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

(defun mu4e-headers-toggle-at-point ()
  "Toggle visibility of the thread at point"
  (interactive)
  (when (get-buffer "*mu4e-headers*")
    (with-current-buffer "*mu4e-headers*"
      (catch 'break
        (while (and (not (mu4e-headers--toggle-internal))
                    (not (bobp)))
          (forward-line -1))))))

(defun mu4e-headers--toggle-internal ()
  "Toggle visibility of the thread at point"
  (let (child-overlay root-overlay)
    (cl-loop for ov in (overlays-in (point-at-bol) (point-at-eol))
             when (overlay-get ov 'thread-child)
             return (setq child-overlay ov)
             when (overlay-get ov 'thread-root)
             return (setq root-overlay ov))
    (cond (root-overlay
           (let ((id     (overlay-get root-overlay 'thread-id))
                 (folded (overlay-get root-overlay 'folded)))
             (mu4e-headers-overlay-set-visibility (not folded) id)
             (throw 'break t)))
          ((not child-overlay)
           (throw 'break t)))))

(defun mu4e-headers-toggle-fold-all ()
  "Toggle between all threads unfolded and all threads folded."
  (interactive)
  (mu4e-headers-overlay-set-visibility
   (not mu4e-thread-folding-all-folded)))

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

(defvar mu4e-thread-folding-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mu4e-headers-mode-map)
    (define-key map (kbd "TAB") 'mu4e-headers-toggle-at-point)
    (define-key map (kbd "<backtab>") 'mu4e-headers-toggle-fold-all)
    map))

;; Install hooks
(defun mu4e-thread-folding-load ()
  "Install hooks."
  (add-hook 'mu4e-index-updated-hook #'mu4e-headers-mark-threads)
  (add-hook 'mu4e-headers-found-hook #'mu4e-headers-mark-threads)
  (add-hook 'mu4e-view-mode-hook 'mu4e-headers-mark-threads-no-reset))

;;;###autoload
(define-minor-mode mu4e-thread-folding-mode
  "Minor mode for folding threads in mu4e-headers view."
  :group 'mu4e-thread-folding
  :lighter " Threads"
  (if mu4e-thread-folding-mode
      (mu4e-thread-folding-load)
    (remove-hook 'mu4e-index-updated-hook #'mu4e-headers-mark-threads)
    (remove-hook 'mu4e-headers-found-hook #'mu4e-headers-mark-threads)
    (remove-hook 'mu4e-view-mode-hook 'mu4e-headers-mark-threads-no-reset)))


(provide 'mu4e-thread-folding)
;;; mu4e-thread-folding.el ends here
