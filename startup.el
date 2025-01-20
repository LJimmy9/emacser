(setq backup-directory-alist `(("." . ,(expand-file-name ".tmp/backups/"
							   user-emacs-directory))))

(setq custom-file (locate-user-emacs-file ".custom.el"))
(load custom-file :no-error-if-file-is-missing)

(add-hook 'before-save-hook #'whitespace-cleanup)
(setq-default sentence-end-double-space nil)
(global-subword-mode 1)
(global-display-line-numbers-mode)

(setq scroll-conservatively 1000)
(save-place-mode t)

(ffap-bindings)
(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(setq delete-by-moving-to-trash t)
(setopt use-short-answers t)

(global-auto-revert-mode t) ; Doesn't work on macos for some reason?
(auto-revert-mode t)

(setq undo-limit 10000000)

(global-hl-line-mode t)

(setq window-combination-resize t) ; take new window space from all other windows

(global-visual-line-mode t)
