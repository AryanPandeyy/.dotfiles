;; bugswriter's minimal emacs config

(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(use-package ewal
  :init (setq ewal-use-built-in-always-p nil
              ewal-use-built-in-on-failure-p t
              ewal-built-in-palette "sexy-material"))
(use-package doom-themes
      :if window-system
      :ensure t
      :config
     (load-theme 'doom-molokai t)
      (doom-themes-org-config)
      (doom-themes-visual-bell-config)
      (menu-bar-mode -1)
      (tool-bar-mode -1)
      (fringe-mode -1)
      (scroll-bar-mode -1))
;;(use-package ewal-spacemacs-themes
;;  :if window-system
;;  :ensure t
;;  :init
;;  (load-theme 'ewal-spacemacs-classic t)
;;  (menu-bar-mode -1)
;;  (tool-bar-mode -1)
;;  (scroll-bar-mode -1)
;;  (window-divider-mode 1))
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(add-to-list 'default-frame-alist
	     '(font . "JetBrainsMono Nerd Font-14"))
;; (use-package exec-path-from-shell
;;   :ensure t
;;   :config
;;   (exec-path-from-shell-initialize))
(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Enable Evil
(require 'evil)
(evil-mode 1)
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
(key-chord-mode 1)
;(;; defun baal-setup-lsp-company ()
;;   (setq-local company-backends
;;               '(company-capf company-dabbrev company-dabbrev-code)))

;; (add-hook 'lsp-completion-mode-hook #'baal-setup-lsp-company)
(setq company-idle-delay 0 ;; How long to wait before popping up
       company-minimum-prefix-length 1 ;; Show the menu after one key press
      ;; company-tooltip-limit 15 ;; Limit on how many options to display
      ;; company-show-numbers t   ;; Show numbers behind options
      ;; company-tooltip-align-annotations t ;; Align annotations to the right
      ;; company-require-match nil           ;; Allow free typing
      ;; company-selection-wrap-around t ;; Wrap around to beginning when you hit bottom of suggestions
      ;; company-dabbrev-ignore-case t ;; Don't ignore case when completing
      ;; company-dabbrev-downcase t ;; Don't automatically downcase completions
      )
(use-package which-key
  :ensure t
  :config
  (which-key-mode))
(use-package company
  :ensure t
  :bind
  ("C-j" . company-complete)
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package json-mode
  :ensure t)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(use-package web-mode
  :ensure t
  :mode (("\\.js\\'" . web-mode)
	 ("\\.jsx\\'" . web-mode)
	 ("\\.ts\\'" . web-mode)
	 ("\\.tsx\\'" . web-mode)
         ("\\.html\\'" . web-mode))
  :commands web-mode)
(setq lsp-log-io nil)
(setq lsp-keymap-prefix "C-c l")
(setq lsp-restart 'auto-restart)
;; (setq lsp-ui-sideline-show-diagnostics t)
;; (setq lsp-ui-sideline-show-hover t)
;; (setq lsp-ui-sideline-show-code-actions t)
(setq lsp-enable-snippet t)
(use-package lsp-mode
  :ensure t
  :hook (
	 (web-mode . lsp-deferred)
	 (html-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration)
	 )
  :commands lsp-deferred)

(require 'lsp-java)
(add-hook 'java-mode-hook #'lsp)
;; (setq lsp-java-java-path "/usr/lib/jvm/java-17-openjdk/bin/java")
;; (setq lsp-java-configuration-runtimes '[(:name "JavaSE-11"
;; 					       :path "/usr/lib/jvm/java-11-openjdk"
;; 					       :default t)
;; 					(:name "JavaSE-17"
;; 						:path "/usr/lib/jvm/java-17-openjdk/")])
;; (use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
;; (use-package dap-java :ensure nil)
(require 'lsp-html)
(add-hook 'html-mode-hook #'lsp)
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred	 
;;(use-package lsp-mode
;;  :ensure t
;;  :commands lsp
;;  :hook ((rjsx-mode . lsp)
;;	  (mhtml-mode . lsp)
;;	 (css-mode . lsp)
;;	 ))
;;(use-package rjsx-mode
;;  :ensure t
;;  :mode "\\.js\\'")
;(defun setup-tide-mode()
;  (interactive)
;  (tide-setup)
;  (flycheck-mode +1)
;  (setq flycheck-syntax-automatically '(save mode-enabled))
;  (tide-hl-identifier-mode +1)
;  (company-mode +1))
;(use-package tide
;  :ensure t
;  :after (rjsx-mode company flycheck)
;  :hook (rjsx-mode . setup-tide-mode))
;; (use-package prettier
;;  :ensure t
;;  :diminish
;;  :hook ((mhtml-mode css-mode scss-mode web-mode java-mode) . prettier-mode))
(use-package emmet-mode
  :ensure t
  :bind
  ("C-<tab>" . emmet-expand-line)
  :diminish
  :config
  (add-to-list 'emmet-jsx-major-modes 'your-jsx-major-mode)
  :custom
  (emmet-indentation 2)
  (emmet-move-cursor-between-quotes t)
  :hook ((mhtml-mode css-mode scss-mode) . emmet-mode))
;; (use-package flycheck
;;  :ensure t
;;  :hook ((js2-mode rjsx-mode css-mode scss-mode java-mode) . flycheck-mode))
;; (setq use-dialog-box nil)
;; (setq use-file-dialog nil)
;; (setq make-backup-files nil)
;; (setq auto-save-default nil)

(global-subword-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner "~/.emacs.d/avatar.png")
  (setq dashboard-banner-logo-title "I am just a coder for fun"))

(use-package switch-window
  :ensure t
  :config
  (setq switch-window-input-style 'minibuffer)
  (setq switch-window-increase 4)
  (setq switch-window-threshold 2)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
	'("a" "s" "d" "f" "j" "k" "l"))
  :bind
  ([remap other-window] . switch-window))

(use-package hungry-delete
  :ensure t
  :config (global-hungry-delete-mode))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package magit
  :ensure t
  :config
  (setq magit-push-always-verify nil)
  (setq git-commit-summary-max-length 50)
  :bind
  ("M-g" . magit-status))

(defun entry-time-stamp()
  (interactive)
  (insert (format-time-string "<i class='ts'>%a %I:%m.%P %d.%m.%4Y</i>")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(custom-safe-themes
   '("ba72dfc6bb260a9d8609136b9166e04ad0292b9760a3e2431cf0cd0679f83c3a" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(org-fontify-done-headline nil)
 '(org-fontify-todo-headline nil)
 '(package-selected-packages
   '(lsp-ui flycheck impatient-mode key-chord lsp-pyright evil-visual-mark-mode evil yasnippet which-key ewal exec-path-from-shell lsp-java json-mode counsel-etags web-mode tide company-box company rjsx-mode ewal-doom-themes emmet-mode prettier lsp-mode ## magit org-bullets hungry-delete switch-window dashboard ewal-spacemacs-themes use-package))
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
