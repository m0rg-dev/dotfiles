(tool-bar-mode -1)
(menu-bar-mode -1)

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
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(compilation-message-face (quote default))
 '(cperl-invalid-face (quote default) t)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(fci-rule-color "#073642")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(mac-option-modifier (quote (:ordinary meta :function alt :mouse alt)))
 '(magit-diff-use-overlays nil)
 '(markdown-fontify-code-blocks-natively t)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (markdown-mode yafolding org-plus-contrib slack dockerfile-mode docker-compose-mode yaml-mode solarized-theme powerline)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(powerline-display-hud nil)
 '(powerline-gui-use-vcs-glyph t)
 '(scroll-bar-mode nil)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tramp-shell-prompt-pattern
   "\\(?:^\\|\\)[^]#$%>
]*#?[]#$%>] *\\(\\[[0-9;]*[a-zA-Z] *\\)*")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c9485ddd1797")
     (60 . "#bf7e73b30bcb")
     (80 . "#b58900")
     (100 . "#a5a58ee30000")
     (120 . "#9d9d91910000")
     (140 . "#9595943e0000")
     (160 . "#8d8d96eb0000")
     (180 . "#859900")
     (200 . "#67119c4632dd")
     (220 . "#57d79d9d4c4c")
     (240 . "#489d9ef365ba")
     (260 . "#3963a04a7f29")
     (280 . "#2aa198")
     (300 . "#288e98cbafe2")
     (320 . "#27c19460bb87")
     (340 . "#26f38ff5c72c")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#002b36" :foreground "#93a1a1" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width semi-condensed :foundry "PfEd" :family "Monoid"))))
 '(bold ((t (:weight bold))))
 '(cperl-array-face ((t (:foreground "#268bd2" :slant italic))))
 '(cperl-hash-face ((t (:foreground "#dc322f" :slant italic))))
 '(cperl-nonoverridable-face ((t (:inherit font-lock-keyword-face))))
 '(cursor ((t (:background "#fdf6e3"))))
 '(error ((t (:foreground "#dc322f" :weight bold))))
 '(escape-glyph ((t (:foreground "#2aa198"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(font-lock-builtin-face ((t (:foreground "#268bd2" :slant normal :weight semi-bold))))
 '(font-lock-comment-face ((t (:foreground "#dc322f"))))
 '(font-lock-constant-face ((t (:foreground "#c33682"))))
 '(font-lock-function-name-face ((t (:foreground "#6c71c4" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "#2aa198" :weight normal))))
 '(font-lock-string-face ((t (:background "#073642" :foreground "#cb4b16"))))
 '(font-lock-type-face ((t (:foreground "#859900"))))
 '(font-lock-variable-name-face ((t (:foreground "#b58900"))))
 '(fringe ((t (:background "#073642"))))
 '(isearch ((t (:background "#d33682" :foreground "#eee8d5"))))
 '(link ((t (:foreground "#2aa198" :underline t))))
 '(link-visited ((t (:inherit link :foreground "#6c71c4"))))
 '(minibuffer-prompt ((t (:foreground "#2aa198"))))
 '(mode-line ((t (:background "#073642" :foreground "#93a1a1"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey30" :foreground "grey80" :weight light))))
 '(org-code ((t (:foreground "#eee8d5"))))
 '(org-date ((t (:foreground "#2aa198" :underline t))))
 '(org-level-1 ((t (:inherit outline-1))))
 '(org-level-2 ((t (:inherit outline-2))))
 '(org-level-3 ((t (:inherit outline-3))))
 '(powerline-active0 ((t (:background "#586e75" :foreground "#073642"))))
 '(powerline-active1 ((t (:inherit powerline-active0 :background "#002b36" :foreground "#93a1a1"))))
 '(powerline-active2 ((t (:inherit mode-line :background "#073642" :foreground "#93a1a1"))))
 '(powerline-inactive0 ((t (:background "#073642" :foreground "#839496"))))
 '(powerline-inactive1 ((t (:inherit powerline-inactive0 :background "#002b36"))))
 '(powerline-inactive2 ((t (:inherit powerline-inactive0))))
 '(smerge-refined-added ((t (:inherit smerge-refined-change :background "#227722"))))
 '(term-color-black ((t (:background "#fdf6e3" :foreground "#fdf6e3"))))
 '(term-color-blue ((t (:background "#268bd2" :foreground "#268bd2"))))
 '(term-color-cyan ((t (:background "#2aa198" :foreground "#2aa198"))))
 '(term-color-green ((t (:background "#859900" :foreground "#859900"))))
 '(term-color-magenta ((t (:background "#d33682" :foreground "#d33682"))))
 '(term-color-red ((t (:background "#dc322f" :foreground "#dc322f"))))
 '(term-color-white ((t (:background "#073642" :foreground "#073642"))))
 '(term-color-yellow ((t (:background "#b58900" :foreground "#b58900")))))



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
