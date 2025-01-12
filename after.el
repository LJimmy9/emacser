(evil-set-leader nil (kbd "SPC"))

(define-key evil-motion-state-map (kbd "SPC") nil)
(define-key evil-motion-state-map (kbd ",") nil)
(define-key evil-motion-state-map (kbd "C-e") nil)
(define-key evil-motion-state-map (kbd "C-y") nil)
(define-key evil-motion-state-map (kbd "C-f") nil)
(define-key evil-motion-state-map (kbd "C-b") nil)
(define-key evil-motion-state-map (kbd "ge") #'er/expand-region)
(define-key evil-motion-state-map (kbd "gr") #'er/contract-region)

(define-key evil-motion-state-map (kbd "gj") nil)
(define-key evil-motion-state-map (kbd "gk") nil)
(define-key evil-insert-state-map (kbd "C-t") nil)
(define-key evil-insert-state-map (kbd "U") nil)
(define-key evil-insert-state-map (kbd "C-a") nil)
(define-key evil-insert-state-map (kbd "C-d") nil)
(define-key evil-insert-state-map (kbd "C-y") nil)

(evil-define-key 'normal 'global (kbd "<leader>qq") 'evil-save-modified-and-close)
(evil-define-key 'normal 'global (kbd "<leader>aa") 'indent-region)
(evil-define-key 'normal 'global (kbd "<leader>ar") 'align-regexp)
(evil-define-key 'normal 'global (kbd "<leader>ss") 'save-buffer)
(evil-define-key 'normal 'global (kbd "<leader>xe") 'eval-last-sexp)
(evil-define-key 'normal 'global (kbd "<leader>gg") 'magit-status)
(evil-define-key 'normal 'global (kbd "gcc") 'evilnc-comment-or-uncomment-lines)
(evil-define-key 'normal 'global (kbd "<leader>er") (lambda ()
						      (interactive)
						      (dired (file-name-directory (or (buffer-file-name) locate-user-emacs-file)))))


(evil-define-key 'normal 'global (kbd "<leader>fg") 'consult-ripgrep)
(evil-define-key 'normal 'global (kbd "<leader>ff") 'consult-fd)
(evil-define-key 'normal 'global (kbd "<leader>fo") 'consult-outline)
(evil-define-key 'normal 'global (kbd "<leader>fl") 'consult-line)
(evil-define-key 'normal 'global (kbd "<leader>fb") 'consult-buffer)
(evil-define-key 'normal 'global (kbd "<leader>fm") 'consult-mark)
(evil-define-key 'normal 'global (kbd "<leader>fr") 'consult-register)
