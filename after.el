(define-key evil-motion-state-map (kbd "SPC") nil)
(define-key evil-motion-state-map (kbd ",") nil)
(define-key evil-motion-state-map (kbd "C-e") nil)
(define-key evil-insert-state-map (kbd "C-e") nil)
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


(define-key minibuffer-local-map (kbd "C-n") 'vertico-next)
(define-key minibuffer-local-map (kbd "C-p") 'vertico-prev)


(evil-leader/set-key
  "aa" 'indent-region
  "ar" 'align-regexp
  "qq" 'evil-save-modified-and-close
  "ss" 'save-buffer
  "xe" 'eval-last-sexp
  "gg" 'magit-status
  "rr" 'compile
  "er" (lambda ()
	     (interactive)
	     (dired (file-name-directory (or (buffer-file-name) locate-user-emacs-file))))
  )

(evil-define-key 'normal 'global (kbd "gcc") 'evilnc-comment-or-uncomment-lines)
