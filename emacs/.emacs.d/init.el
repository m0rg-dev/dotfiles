;;; init --- Morgan's base Emacs configuration
;;;
;;; Commentary:
;;;
;;;
;;; Code:

;; Pull in any local customizations.
(let ((local-settings "~/.emacs.local.el"))
  (when (file-exists-p local-settings)
    (load-file local-settings)))

(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

(menu-bar-mode -1)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (auto-package-update-maybe))

(use-package terraform-mode)
(use-package go-mode)
(use-package nasm-mode)
(use-package markdown-mode)
(use-package dockerfile-mode)
(use-package docker-compose-mode)
(use-package yaml-mode)
(use-package vterm)
(use-package flycheck :config (global-flycheck-mode))
(use-package company :config (global-company-mode))
(use-package yasnippet :config (yas-global-mode 1))
(use-package lsp-mode :config (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))
(use-package lsp-ui)
(use-package rust-mode)
(use-package projectile :config (projectile-mode) (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
(use-package helm :config (helm-mode))
(use-package diminish
  :config
  (diminish 'yas-minor-mode)
  (diminish 'company-mode)
  (diminish 'helm-mode)
  (diminish 'eldoc-mode)
  )
(use-package magit)
(use-package treemacs)
(use-package treemacs-projectile)
(use-package lsp-treemacs :config (lsp-treemacs-sync-mode 1))

(winner-mode)
(column-number-mode)

(require 'lsp-mode)
(add-hook 'rust-mode-hook #'lsp)
(add-hook 'go-mode-hook #'lsp)

(require 'display-line-numbers)

(defcustom display-line-numbers-exempt-modes
  '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode)
  "Major modes on which to disable line numbers."
  :group 'display-line-numbers
  :type 'list
  :version "green")

(defun display-line-numbers--turn-on ()
  "Turn on line numbers except for certain major modes.
Exempt major modes are defined in `display-line-numbers-exempt-modes'."
  (unless (or (minibufferp)
              (member major-mode display-line-numbers-exempt-modes))
    (display-line-numbers-mode)))

(global-display-line-numbers-mode)

;; bind mouse scroll
(defun mouse-scroll-down () "Scroll down faster." (interactive) (scroll-down 2))
(defun mouse-scroll-up () "Scroll up faster." (interactive) (scroll-up 2))

(unless (display-graphic-p)
  (global-set-key (kbd "<mouse-4>") #'mouse-scroll-down)
  (global-set-key (kbd "<mouse-5>") #'mouse-scroll-up))

(eval-after-load 'lsp '(define-key lsp-mode-map (kbd "d") #'lsp-ui-doc-show))

;; bind C-z to undo
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-set-key (kbd "C-z") #'undo)

;;; Face customization.

(progn (setq frame-background-mode 'light)
 (mapc 'frame-set-background-mode (frame-list)))

(progn (setq frame-background-mode 'dark)
 (mapc 'frame-set-background-mode (frame-list)))

(defvar tty-specific-colors)
(setq tty-specific-colors (if (display-graphic-p) '((:base03  . "#002b36")
						    (:base02  . "#073642")
						    (:base01  . "#586e75")
						    (:base00  . "#657b83")
						    (:base0   . "#839496")
						    (:base1   . "#93a1a1")
						    (:base2   . "#eee8d5")
						    (:base3   . "#fdf6e3")
						    (:yellow  . "#b58900")
						    (:orange  . "#cb4b16")
						    (:red     . "#dc322f")
						    (:magenta . "#d33682")
						    (:violet  . "#6c71c4")
						    (:blue    . "#268bd2")
						    (:cyan    . "#2aa198")
						    (:green   . "#859900"))
			    '((:base03  . "black")
			      (:base02  . "brightblack")
			      (:base01  . "brightgreen")
			      (:base00  . "brightyellow")
			      (:base0   . "brightblue")
			      (:base1   . "brightcyan")
			      (:base2   . "white")
			      (:base3   . "brightwhite")
			      (:yellow  . "yellow")
			      (:orange  . "brightred")
			      (:red     . "red")
			      (:magenta . "magenta")
			      (:violet  . "brightmagenta")
			      (:blue    . "blue")
			      (:cyan    . "cyan")
			      (:green   . "green"))))

(defvar named-colors)
(setq named-colors (append tty-specific-colors '()))

(defun color (sym) "Get SYM out of named-colors (shorthand)." (alist-get sym named-colors))

(face-spec-set 'default `(
			  (((background light)) (:foreground ,(color :base00) :background ,(color :base3) :family "Iosevka" :height 160))
			  (((background dark)) (:foreground ,(color :base0) :background ,(color :base03) :family "Iosevka" :height 160))
			  ))

(defun face (name &rest args) "Set face NAME's default face spec to ARGS across all terminal types."
       (face-spec-set name `((t ,args)) 'face-defface-spec))

(defun dynamic-face (name light-only dark-only &rest both) "Set face NAME's default face spec to BOTH plus LIGHT-ONLY on light frames, and BOTH plus DARK-ONLY on dark frames."
       (face-spec-set name `(
			     (((background light)) ,(append light-only both))
			      (((background dark)) ,(append dark-only both)))
			   'face-defface-spec))

(face 	      'error :foreground (color :red) :slant 'italic)
(face 	      'font-lock-builtin-face :foreground (color :violet) :slant 'italic)
(dynamic-face 'font-lock-comment-face (list :foreground (color :base1)) (list :foreground (color :base01)) :slant 'italic)
(face 	      'font-lock-constant-face :foreground (color :cyan))
(face 	      'font-lock-function-name-face :foreground (color :blue) :weight 'bold)
(face 	      'font-lock-keyword-face :foreground (color :orange))
(face 	      'font-lock-type-face :foreground (color :green))
(face 	      'font-lock-string-face :foreground (color :cyan))
(face 	      'font-lock-variable-name-face :foreground (color :yellow))
(face 	      'header-line :underline t)
(dynamic-face 'line-number (list :foreground (color :base1)) (list :foreground (color :base01)) :weight 'light)
(face         'line-number-current-line :inherit 'line-number :weight 'bold)
(face         'lsp-face-semhl-interface nil)
(face         'minibuffer-prompt :foreground (color :blue) :weight 'bold)
(face         'mode-line :foreground (color :base2) :background (color :violet))
(dynamic-face 'mode-line-inactive (list :background (color :base2)) (list :background (color :base02)) :foreground (color :violet) :weight 'light)
(face         'rust-string-interpolation :inherit 'font-lock-builtin-face)
(dynamic-face 'shadow (list :foreground (color :base1)) (list :foreground (color :base01)) nil)
(face         'success :foreground (color :green) :slant 'italic)
(dynamic-face 'tty-menu-enabled-face (list :background (color :base2) :foreground (color :base00)) (list :background (color :base02) :foreground (color :base0)) :weight 'bold)
(dynamic-face 'tty-menu-disabled-face (list :background (color :base2) :foreground (color :base01)) (list :background (color :base02) :foreground (color :base1)) :weight 'bold)
(face         'tty-menu-selected-face :foreground (color :orange) :inherit 'tty-menu-enabled-face)
(face         'warning :foreground "DarkOrange" :slant 'italic)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-mode-text "")
 '(display-line-numbers-width-start t)
 '(inhibit-startup-screen t)
 '(lsp-keymap-prefix "C-c l")
 '(lsp-lens-enable nil)
 '(lsp-rust-analyzer-cargo-watch-command "clippy")
 '(lsp-rust-analyzer-proc-macro-enable t)
 '(lsp-rust-analyzer-server-display-inlay-hints t)
 '(lsp-semantic-tokens-enable t)
 '(lsp-ui-sideline-enable t)
 '(package-selected-packages
   '(multi-vterm treemacs-projectile lsp-treemacs treemacs magit diminish projectile helm lsp-ui yasnippet rust-mode lsp-mode company flycheck dockerfile-mode yaml-mode use-package powerline auto-package-update))
 '(powerline-display-hud nil)
 '(powerline-gui-use-vcs-glyph t)
 '(scroll-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vterm-buffer-name-string "vterm %s")
 '(xterm-mouse-mode t))

;;; DO NOT USE!
;;; TODO auto-check and make sure this stays empty
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
