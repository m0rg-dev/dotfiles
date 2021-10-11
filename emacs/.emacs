(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(terraform-mode go-mode nasm-mode markdown-mode yafolding org-plus-contrib slack dockerfile-mode docker-compose-mode yaml-mode powerline))
 '(powerline-display-hud nil)
 '(powerline-gui-use-vcs-glyph t)
 '(scroll-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil))


;(mac-auto-operator-composition-mode)
(powerline-default-theme)
(winner-mode)

(setq-default indent-tabs-mode nil)
(setq fill-column 130)
(setq auto-fill-mode t)
(c-set-offset 'case-label t)

(setq cperl-invalid-face nil)

(defun kill-unused-buffers ()
  "Kill all buffers not currently visible in a window somewhere."
  (interactive)
  (dolist (buf (buffer-list))
    (unless (get-buffer-window buf 'visible) (kill-buffer buf))))
(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)

;; ====================
;; insert date and time

(defvar current-date-time-format "%a %Y-%m-%d"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%H:%M"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
       (interactive)
       (insert "* ")
;       (insert (let () (comment-start)))
       (insert (format-time-string current-date-time-format (current-time)))
       (insert "\n")
       )

(defun insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
       (interactive)
       (insert (format-time-string current-time-format (current-time)))
       (insert " ")
       )

(global-set-key "\C-cn" 'insert-current-date-time)
(global-set-key "\C-cm" 'insert-current-time)

(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(put 'downcase-region 'disabled nil)
(setq mac-option-key-is-meta t)
(setq mac-command-key-is-meta nil)
(setq mac-command-modifier nil)
(setq mac-option-modifier 'meta)
