(package-initialize)

;;Install packages automatically
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(setq package-enable-at-startup nil)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(add-to-list 'load-path "~/.emacs.d/lisp/")
;;move custom variables out of the way
(setq custom-file "~/.emacs.d/custom.el")


;;Turn off unneeded ui elements
(setq inhibit-splash-screen t)
(global-linum-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq ring-bell-function 'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)

;;Save backup files out of the way
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )


;;Theme
(if (display-graphic-p)
    (progn
      (load-theme 'adapta-noko-maia t))
  (load-theme 'terminal-adapta-noko-maia t))
;company colors
(require 'color)
(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

(add-to-list 'load-path "~/.emacs.d/lisp/powerline")
(require 'powerline)
(powerline-default-theme)

(global-hl-line-mode +1)

(when (eq system-type 'windows-nt)
  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono for Powerline-9" ))
  (set-face-attribute 'default t :font "DejaVu Sans Mono for Powerline-9" ))

;;useful plugins

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))


;;better auto-complete in menus
(use-package smex
  :ensure t)

(use-package ivy
    :ensure t
    :diminish ivy-mode
    :config
    (ivy-mode t)
    (setq ivy-initial-inputs-alist nil))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (add-hook 'after-init-hook 'which-key-mode))

(use-package undo-tree
  :ensure t
  :defer 5
  :diminish global-undo-tree-mode
  :config
  (global-undo-tree-mode 1))

;navigation
(use-package avy
  :ensure t
  :bind (("C-," . avy-goto-word-1)
	 ("C-'" . avy-goto-char)))

(use-package crux
  :ensure t
  :bind (("C-a" . crux-move-beginning-of-line)))



;;projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :ensure t
  :config
  (add-hook 'after-init-hook 'counsel-projectile-mode))

;;file browser
(use-package neotree
  :ensure t
  :config
  (global-set-key (kbd "C-c t") 'neotree-toggle)
  (setq neo-smart-open t)
  (setq neo-theme 'arrow))

;;auto complete in code
(use-package company
  :ensure t
  :bind (("C-å" . company-complete))
  :diminish
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay t))
(global-company-mode 1)

;;Flycheck
(use-package flycheck
  :ensure t)

;;Web stuff
(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (setq-default js2-ignored-warnings '("msg.extra.trailing.comma")))

(use-package rjsx-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :mode ("\\.html\\'")
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-engines-alist
        '(("django" . "focus/.*\\.html\\'")
          ("ctemplate" . "realtimecrm/.*\\.html\\'"))))

(use-package prettier-js
  :ensure t
  :config
  (setq prettier-js-args '(
                        "--trailing-comma" "es5"
                        "--single-quote" "true"
                        "--print-width" "100"
                        ))
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'rjsx-mode-hook 'prettier-js-mode))

(defun jc/use-eslint-from-node-modules ()
  "Set local eslint if available."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))



;;Elm
(use-package elm-mode
  :ensure t
  :config
  (setq elm-format-on-save t)
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
  (add-to-list 'company-backends 'company-elm)
  (use-package flycheck-elm
    :ensure t
    :config
    (add-hook 'flycheck-mode-hook 'flycheck-elm-setup)
    (add-hook 'elm-mode-hook #'flycheck-mode)))

;;##Org##
;prettier bullets
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode))


;;##EXWM

;; (use-package exwm
;;   :ensure t
;;   :bind
;;   (("s-a" . async-shell-command))
;;   :config
;;   (require 'exwm-config)
;;   (exwm-config-default))


;;##Docker##
(use-package dockerfile-mode
  :ensure t
  :config
  (require 'dockerfile-mode)
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package docker
  :ensure t
  :config
  (docker-global-mode 0))
