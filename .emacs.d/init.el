(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(use-package doom-themes
      :if window-system
      :ensure t
      :config
      (load-theme 'doom-one t)
      (doom-themes-org-config)
      (doom-themes-visual-bell-config)
      (menu-bar-mode -1)
      (tool-bar-mode -1)
      (fringe-mode -1)
      (scroll-bar-mode -1))

(add-to-list 'default-frame-alist
	     '(font . "JetBrains Mono-11"))

(use-package swiper
  :ensure t
  :bind ("C-s" . 'swiper))

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
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-banner-logo-title "I am just a coder for fun")
  (setq dashboard-footer-messages '("😈 Happy hacking!   "
                                    "👽 Happy hacking!   "
                                    "👻 Happy hacking!   "
                                    "💀 Happy hacking!   ")))

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

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(use-package ido-vertical-mode
  :ensure t
  :init
  (ido-vertical-mode 1))
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind
  ("M-x" . smex))

(use-package projectile
  :ensure t
  :init
  (projectile-mode 1))

(defun entry-time-stamp()
  (interactive)
  (insert (format-time-string "<i class='ts'>%a %I:%m.%P %d.%m.%4Y</i>")))

;;evil mode
(use-package evil
  :ensure t)
(evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
(require 'evil)
(evil-mode 1)

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(95))
(add-to-list 'default-frame-alist '(alpha . (95)))
;;which key config
(use-package which-key
  :ensure t
  :init
  (which-key-mode))

;; line number for emacs
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)


;;indentation
(use-package highlight-indent-guides
  :ensure t)
(setq highlight-indent-guides-method 'character)

;;murder current buffer
(defun kill-curr-buffer ()

;;electric pair mode
(setq electric-pair-pairs '(
			     (?\{ . ?\})
			     (?\( . ?\))
			     (?\[ . ?\])
			     (?\" . ?\")
			     (?\< . ?\>)
			     ))
(electric-pair-mode t)
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'kill-curr-buffer)

;;emmet config
(use-package emmet-mode
  :ensure t
  :bind
  ("C-<tab>" . emmet-expand-line)
  :hook ((web-mode . emmet-mode)
         (css-mode . emmet-mode)))

(use-package web-mode
  :ensure t
  :mode
  ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'"
   "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.[t]?html?\\'"))

;; company config
(use-package company
  :ensure t
  ;; :bind (:map company-active-map ("<tab>" . company-complete-selection))
  :config
  (setq company-idle-delay 0.1)
  (setq company-tooltip-minimum-width 60)
  (setq company-tooltip-maximum-width 60)
  (setq company-tooltip-limit 12)
  (setq company-minimum-prefix-length 3)
  (setq company-tooltip-align-annotations t))
(define-key company-active-map (kbd "TAB") 'company-complete-selection)
(define-key company-active-map (kbd "<tab>") 'company-complete-selection)
(global-company-mode)
;; (setq company-backends '((company-ctags company-yasnippet)))
;; (setq company-idle-delay 0.1)
;; (setq company-minimum-prefix-length 3)

;;lsp config
(setq lsp-keymap-prefix "C-c l")
(use-package lsp-mode
  :ensure t
    :hook ((c-mode          ; clangd
          c++-mode        ; clangd
          c-or-c++-mode   ; clangd
          java-mode       ; eclipse-jdtls
          js-mode         ; ts-ls (tsserver wrapper)
          js-jsx-mode     ; ts-ls (tsserver wrapper)
          typescript-mode ; ts-ls (tsserver wrapper)
          python-mode     ; pyright
          web-mode        ; ts-ls/HTML/CSS
          haskell-mode    ; haskell-language-server
          ) . lsp-deferred)
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp
  :config
  (setq lsp-auto-guess-root t)
  (setq lsp-log-io nil)
  (setq lsp-restart 'auto-restart)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-eldoc-hook nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-semantic-tokens-enable nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-imenu nil)
  (setq lsp-enable-snippet t)
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  (setq lsp-idle-delay 0.5))
;;;;;;;;;;;;;;;;;;; tab width;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default tab-width 2)
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred


;;lsp-ui config
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :config
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-use-childframe t)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-border (face-foreground 'default))
  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-ui-peek-always-show t)
  (setq lsp-ui-sideline-delay 0.05))
 

;;flymake
(use-package flymake
  :ensure t)

;;dap mode
(use-package dap-mode
  :diminish
  :bind
  (:map dap-mode-map
        (("<f12>" . dap-debug)
         ("<f8>" . dap-continue)
         ("<f9>" . dap-next)
         ("<M-f11>" . dap-step-in)
         ("C-M-<f11>" . dap-step-out)
         ("<f7>" . dap-breakpoint-toggle))))

;; lsp-java config
(use-package lsp-java
  :ensure t
  :after lsp-mode
  :if (executable-find "mvn")
  :init
  (use-package request :defer t)
  :custom
  (lsp-java-server-install-dir (expand-file-name "~/.emacs.d/eclipse.jdt.ls/server/"))
  (lsp-java-workspace-dir (expand-file-name "~/.emacs.d/eclipse.jdt.ls/workspace/")))

;;highlight lines
(global-hl-line-mode 1)

;;yas snippet
(use-package yasnippet
  :ensure t)


;;treemacs config
(setq treemacs-toggle-fixed-width t)

;; flycheck
(use-package flycheck
  :ensure t)
(define-key flymake-mode-map (kbd "M-n") 'flycheck-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flycheck-previous-error)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(mu4e yasnippet-snippets flycheck yasnippet lsp-java lsp-ui lsp-pyright lsp-mode company web-mode emmet-mode highlight-indent-guides which-key evil projectile smex ido-vertical-mode magit org-bullets hungry-delete switch-window dashboard swiper doom-themes use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
