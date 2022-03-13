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
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package terraform-mode)
(use-package powerline
  :config (powerline-default-theme))
(use-package go-mode)
(use-package nasm-mode)
(use-package markdown-mode)
(use-package dockerfile-mode)
(use-package docker-compose-mode)
(use-package yaml-mode)
(use-package vterm)
(use-package flycheck
  :init (global-flycheck-mode))
(use-package company
  :init (global-company-mode))
(use-package yasnippet :config (yas-global-mode 1))
(use-package lsp-mode
  :config (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))
(use-package lsp-ui)
(use-package rust-mode)
(use-package projectile :init (projectile-mode) (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
(use-package helm :init (helm-mode))
(use-package diminish
  :config
  (diminish 'yas-minor-mode)
  (diminish 'company-mode)
  (diminish 'helm-mode)
  )
(use-package magit)

(winner-mode)

(require 'display-line-numbers)

(require 'lsp-mode)
(add-hook 'rust-mode-hook #'lsp)

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
(defun mouse-scroll-down () (interactive) (scroll-down 2))
(defun mouse-scroll-up () (interactive) (scroll-up 2))

(unless (display-graphic-p)
  (global-set-key (kbd "<mouse-4>") 'mouse-scroll-down)
  (global-set-key (kbd "<mouse-5>") 'mouse-scroll-up))

(global-set-key (kbd "C-c l d") 'lsp-ui-doc-show)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(display-line-numbers-width-start t)
 '(inhibit-startup-screen t)
 '(lsp-keymap-prefix "C-c l")
 '(lsp-lens-enable nil)
 '(lsp-rust-analyzer-cargo-watch-command "clippy")
 '(lsp-rust-analyzer-proc-macro-enable t)
 '(lsp-ui-sideline-enable t)
 '(package-selected-packages
   '(magit diminish projectile helm lsp-ui yasnippet rust-mode lsp-mode company flycheck dockerfile-mode yaml-mode use-package powerline auto-package-update))
 '(powerline-display-hud nil)
 '(powerline-gui-use-vcs-glyph t)
 '(scroll-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vterm-buffer-name-string "vterm %s")
 '(xterm-mouse-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-variable-tag ((t (:foreground "blue" :weight bold))))
 '(error ((t (:foreground "Red1" :slant italic))))
 '(font-lock-builtin-face ((t (:foreground "brightmagenta" :slant italic))))
 '(font-lock-comment-face ((t (:foreground "red" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "cyan"))))
 '(font-lock-function-name-face ((t (:foreground "blue"))))
 '(font-lock-keyword-face ((t (:foreground "brightred"))))
 '(font-lock-string-face ((t (:foreground "magenta"))))
 '(font-lock-type-face ((t (:foreground "green"))))
 '(font-lock-variable-name-face ((t (:foreground "yellow"))))
 '(header-line ((t (:underline t))))
 '(highlight ((t (:background "darkseagreen2"))))
 '(line-number ((t (:inherit default :foreground "green"))))
 '(line-number-current-line ((t (:inherit line-number :weight bold))))
 '(lsp-face-highlight-textual ((t (:underline t))))
 '(lsp-headerline-breadcrumb-path-error-face ((t (:inherit lsp-headerline-breadcrumb-path-face :underline (:color "Red1" :style wave)))))
 '(minibuffer-prompt ((t (:foreground "blue"))))
 '(mode-line ((t (:background "brightmagenta" :foreground "white" :box (:line-width -1 :style released-button)))))
 '(mode-line-inactive ((t (:inherit mode-line :background "brightblack" :foreground "brightmagenta" :box (:line-width -1 :color "grey75") :weight light))))
 '(powerline-active1 ((t (:inherit mode-line :background "white" :foreground "black"))))
 '(powerline-active2 ((t (:inherit mode-line :background "brightgreen" :foreground "white"))))
 '(powerline-inactive1 ((t (:inherit powerline-active1))))
 '(powerline-inactive2 ((t (:inherit powerline-active2))))
 '(shadow ((t (:foreground "grey30"))))
 '(success ((t (:foreground "green" :slant italic))))
 '(tty-menu-disabled-face ((t (:background "blue" :foreground "brightblack"))))
 '(tty-menu-enabled-face ((t (:background "blue" :foreground "white" :weight bold))))
 '(tty-menu-selected-face ((t (:inherit tty-menu-enabled-face :foreground "gray70"))))
 '(warning ((t (:foreground "DarkOrange" :slant italic)))))
