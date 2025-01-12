(setq	inhibit-startup-message   t
      frame-resize-pixelwise    t  ; fine resize
      package-native-compile    t) ; native compile packages

(blink-cursor-mode 0)              ; disable blinking cursor

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))

(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))


(setq ns-use-native-fullscreen :true)
(add-to-list 'default-frame-alist '(fullscreen . fullscreen))

(defun my/set-font ()
  (set-face-attribute 'default nil
		      :font "Cascadia Mono"
		      :height 140))

(my/set-font)
(add-hook 'server-after-make-frame-hook #'my/set-font)
