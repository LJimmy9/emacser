(setq-default initial-major-mode 'emacs-lisp-mode)
(defvar prog-modes '(
		     c-mode
		     c++-mode
		     java-mode
		     python-mode
		     ruby-mode
		     perl-mode
		     php-mode
		     js-mode
		     emacs-lisp-mode
		     ))
(dolist (mode prog-modes)
  (add-hook (intern (concat (symbol-name mode) "-hook")) #'display-line-numbers-mode)
  )
(dolist (mode prog-modes)
  (add-hook mode #'hs-minor-mode))

(setq backup-directory-alist `(("." . ,(expand-file-name ".tmp/backups/"
							 user-emacs-directory))))

(setq backup-by-copying t)

(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))
(when (file-exists-p custom-file) ; Don’t forget to load it, we still need it
  (load custom-file))

(add-hook 'before-save-hook #'whitespace-cleanup)
(setq-default sentence-end-double-space nil)
(global-subword-mode 1)
(setq scroll-conservatively 1000)

(setq delete-by-moving-to-trash t)
(setopt use-short-answers t)
(global-auto-revert-mode 1)
(setq undo-limit        100000000
      auto-save-default t)

(setq window-combination-resize t) ; take new window space from all other windows

(setq x-stretch-cursor t)

(setq-default dired-listing-switches "-alh")

(setq dired-dwim-target t)
(setq dired-auto-revert-buffer 1)
(global-visual-line-mode t)
