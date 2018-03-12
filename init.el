(package-initialize)

(setq inhibit-splash-screen t)
(linum-mode 1)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;(set-background-color "#222D32")
;(set-foreground-color "#CFD8DC")

(load-theme 'adapta-noko-maia t)


(global-set-key (kbd "C-x g") 'magit-status)


;;Package stuff
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(custom-set-variables
 '(package-selected-packages (quote (magit))))

