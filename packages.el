(add-hook 'server-after-make-frame-hook #'my/set-font)

(setq package-archives '(("melpa"  . "https://melpa.org/packages/")
			 ("gnu"    . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(defvar bootstrap-version)
(defvar comp-deferred-compilation-deny-list ())
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(straight-use-package '(use-package :build t))
(setq use-package-always-ensure t)

(use-package general
  :straight (:build t)
  :init
  (general-auto-unbind-keys)
  :config
  (general-create-definer jl/undefine
    :keymaps 'override
    :states '(normal emacs))
  (general-create-definer jl/evil
    :states '(normal))
  (general-create-definer jl/leader-key
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")
  (general-create-definer jl/major-leader-key
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix ","
    :global-prefix "M-m"))

(use-package evil
  :straight (:build t)
  :after (general)
  :init
  (setq evil-want-integration t
	evil-want-keybinding nil
	evil-want-C-u-scroll t
	evil-want-C-i-jump nil)
  (require 'evil-vars)
  :config
  (general-define-key
   :keymaps 'evil-motion-state-map
   "SPC" nil
   ","   nil
   "C-e" nil
   "C-y" nil
   "C-f" nil
   "C-b" nil
   "C-d" nil
   "gj" nil
   "gk" nil
   )
  (general-define-key
   :keymaps 'evil-insert-state-map
   "C-t" nil
   "U"   nil
   "C-a" nil
   "C-d" nil
   "C-y" nil
   )
  (evil-mode 1)
  (setq evil-want-fine-undo t)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))


(use-package evil-collection
  :after evil
  :straight (:build t)
  :config
  (evil-collection-init))


(use-package evil-nerd-commenter
  :after evil
  :straight (:build t)
  :config
  (general-define-key
   :keymaps 'evil-motion-state-map
   "gcc" #'evilnc-comment-or-uncomment-lines
  ))

(use-package doom-themes
  :straight (:build t)
  :defer t
  :init (load-theme 'doom-nord-aurora t))

(require 'time)
(setq display-time-format "%Y-%m-%d %H:%M")
(display-time-mode 1)

(let ((battery-str (battery)))
  (display-battery-mode 1))

(use-package expand-region
  :config
  (general-define-key
   :keymaps 'evil-motion-state-map
   "vv" #'er/expand-region
   ))

(use-package magit
  :ensure t)

(use-package rainbow-delimiters
  :straight (:build t)
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))
