(package-initialize)

;;Install packages automatically
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(setq package-enable-at-startup nil)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(add-to-list 'load-path "~/.emacs.d/lisp/")
;;move custom variables
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;Turn off unneeded ui elements
(setq inhibit-splash-screen t)
(global-linum-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;;Save backup files out of the way
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )


;;Theme
(when (display-graphic-p)
  (load-theme 'adapta-noko-maia t))

(add-to-list 'load-path "~/.emacs.d/lisp/powerline")
(require 'powerline)
(powerline-default-theme)

(global-hl-line-mode +1)

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


;; ;;Helm mode
;; (use-package helm
;;   :init (helm-mode 1)
;;   :bind (
;; 	 ("C-c h" . helm-command-prefix)
;; 	 ("<tab>" . helm-execute-persistent-action)
;; 	 ("C-i" . helm-execute-persistent-action)
;; 	 ("C-z"  . helm-select-action)
;; 	 ("M-x" . helm-M-x))
;;   :config
;;   (require 'helm-config)
;;   (setq helm-split-window-in-side-p t 
;;       helm-move-to-line-cycle-in-source t
;;       helm-ff-search-library-in-sexp t 
;;       helm-scroll-amount 8 
;;       helm-ff-file-name-history-use-recentf t
;;       helm-echo-input-in-header-line t))



