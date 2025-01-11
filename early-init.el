(setq package-enable-at-startup nil
      inhibit-startup-message   t
      frame-resize-pixelwise    t  ; fine resize
      package-native-compile    t) ; native compile packages
(scroll-bar-mode -1)               ; disable scrollbar
(tool-bar-mode -1)                 ; disable toolbar
(tooltip-mode -1)                  ; disable tooltips
(set-fringe-mode 10)               ; give some breathing room
(menu-bar-mode -1)                 ; disable menubar
(blink-cursor-mode 0)              ; disable blinking cursor

(setq package-enable-at-startup nil)

(setq ns-use-native-fullscreen :true)
(add-to-list 'default-frame-alist '(fullscreen . fullscreen))

(defun my/set-font ()
  (set-face-attribute 'default nil
		      :font "Cascadia Mono"
		      :height 140))

(my/set-font)
(add-hook 'server-after-make-frame-hook #'my/set-font)
