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

(use-package ewal-spacemacs-themes
  :if window-system
  :ensure t
  :init
  (load-theme 'ewal-spacemacs-classic t)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (window-divider-mode 1))

(add-to-list 'default-frame-alist
	     '(font . "JetBrains Mono-16"))

(setq use-dialog-box nil)
(setq use-file-dialog nil)
(setq make-backup-files nil)
(setq auto-save-default nil)

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
(setq gc-cons-threshold 10000000000000000)
;;(setq read-process-output-max (1280*1280)) ;; 1mb

;; which key and company
(use-package which-key
  :ensure t
  :init
  (which-key-mode))
(use-package company
  :ensure t
  :config
  (setq company-idle-delay  0))

(defvar my-linum-current-line-number 0)

(setq linum-format 'my-linum-relative-line-numbers)

(defun my-linum-relative-line-numbers (line-number)
  (let ((test2 (1+ (- line-number my-linum-current-line-number))))
    (propertize
     (number-to-string (cond ((<= test2 0) (1- test2))
                             ((> test2 0) test2)))
     'face 'linum)))

(defadvice linum-update (around my-linum-update)
  (let ((my-linum-current-line-number (line-number-at-pos)))
    ad-do-it))
(ad-activate 'linum-update)
;; line number
(global-linum-mode)

(defun lsp-java-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'java-mode-hook #'lsp-java-install-save-hooks)

;; Start LSP Mode and YASnippet mode
(add-hook 'java-mode-hook #'yas-minor-mode)

;;evil mode
(require 'evil)
(evil-mode 1)

;; vterm
(use-package vterm
  :ensure t
  :init
  (global-set-key (kbd "<C-return>") 'vterm))

;;murder current buffer
(defun kill-curr-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'kill-curr-buffer)

;;kill all buffer
(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
(global-set-key (kbd "C-M-s-k") 'kill-all-buffers)

(setq display-time-24hr-format t)
(setq display-time-format "%H:%M")
(display-time-mode 1)

;;electric pair mode
(setq electric-pair-pairs '(
			     (?\{ . ?\})
			     (?\( . ?\))
			     (?\[ . ?\])
			     (?\" . ?\")
			     ))
(electric-pair-mode t)

;; emmet config
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


;; web and lsp
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
(use-package lsp-mode
  :ensure t
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :config
  (setq lsp-enable-symbol-highlighting nil
	lsp-eldoc-enable-hover nil
	lsp-signature-render-documentation nil))

;;dap mode
;; (use-package dap-mode
;;   :ensure t
;;   :after lsp-mode
;;   :config
;;   (dap-mode t)
;;   (dap-ui-mode t)
;;   (dap-register-debug-template
;;    "um debug"
;;    (list :type "java"
;;          :request "attach"
;;          :hostName "172.18.0.200"
;;          :port 5005))
;;   )

(use-package dap-java
  :ensure nil
  :after (lsp-java)
  :config
  (global-set-key (kbd "<f6>") 'dap-step-in)
  (global-set-key (kbd "<f5>") 'dap-next)
  (global-set-key (kbd "<f7>") 'dap-continue)
  )

;;highlight bracket
(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode highlight-parentheses-mode
  (lambda nil (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)


;; lsp-ui
(setq lsp-ui-sideline-show-diagnostics t)
(setq lsp-ui-sideline-show-code-actions t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-eldoc-enable-hover nil)
 '(lsp-java-format-on-type-enabled nil)
 '(lsp-signature-render-documentation nil)
 '(package-selected-packages
   '(vterm highlight-indentation highlight-parentheses yasnippet which-key web-mode use-package switch-window org-bullets magit lsp-ui lsp-pyright lsp-java hungry-delete flycheck ewal-spacemacs-themes evil emmet-mode dashboard company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
