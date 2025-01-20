(dolist (module '("startup.el" "packages.el" "after.el"))
  (load (expand-file-name module
			    (expand-file-name user-emacs-directory))))
