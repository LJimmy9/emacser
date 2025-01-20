(require 'package)
(setq use-package-always-ensure t)

(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(use-package catppuccin-theme
  :vc (:url "https://github.com/catppuccin/emacs"
	     :branch "main")
  :config
  (setq catppuccin-flavor 'frappe)
  (load-theme 'catppuccin :no-confirm)
  )

(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :config
  (add-hook 'dired-mode-hook 'auto-revert-mode)
  (setq-default dired-listing-switches "-alh")
  (setq dired-dwim-target t)
  (setq dired-auto-revert-buffer 1)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t))

(use-package undo-fu
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

(use-package company
  :config
  (global-company-mode))
(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-max-candidates 20)
  )

(use-package evil
  :init
  (setq evil-want-integration t
	  evil-want-minibuffer t
	  evil-want-keybinding nil
	  evil-want-C-u-scroll t
	  evil-want-C-d-scroll t
	  evil-want-C-i-jump t
	  evil-respect-visual-line-mode t
	  evil-undo-system 'undo-fu)
  :config
  (evil-mode 1)
  (eval-after-load "evil-maps"
    (dolist (map '(evil-motion-state-map
		     ;; evil-insert-state-map
		     evil-emacs-state-map))
	(define-key (eval map) "\C-n" nil)
	(define-key (eval map) "\C-p" nil)
	))
  )

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  )

(use-package evil-nerd-commenter
  :after evil
  )

(use-package evil-leader
  :config
  (evil-leader/set-leader "<SPC>")
  (global-evil-leader-mode)
  )

(use-package expand-region
  )

(use-package magit
  :config
  (with-eval-after-load 'magit-mode
    (add-hook 'after-save-hook 'magit-after-save-refresh-status t)
    ))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package xclip
  :config
  (xclip-mode))

(use-package vertico
    :init
    (vertico-mode 1)
    (setq vertico-cycle t)
    (setq vertico-resize t))

(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
		("RET" . vertico-directory-enter)
		("DEL" . vertico-directory-delete-char)
		("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package marginalia
  :hook (after-init . marginalia-mode))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides nil))

(use-package savehist
  :ensure nil ; it is built-in
  :hook (after-init . savehist-mode))

(use-package wgrep
  :bind ( :map grep-mode-map
	    ("e" . wgrep-change-to-wgrep-mode)
	    ("C-x C-q" . wgrep-change-to-wgrep-mode)
	    ("C-c C-c" . wgrep-finish-edit)))
