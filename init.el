;; Define the init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Define and initialise package repositories
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; use-package to simplify the config file
;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure and load use-package
(setq use-package-always-ensure t)

(eval-when-compile
  (defvar use-package-verbose t)
  (require 'use-package))

;; Theme
;;(load-theme 'tango-dark)
(use-package monokai-theme
  ;;:disabled t
  :config
  (load-theme 'monokai t))


;; System
(setq inhibit-startup-message t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Keys
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key (kbd "C-x C-1") #'delete-other-windows)
(global-set-key (kbd "C-x C-2") #'split-window-below)
(global-set-key (kbd "C-x C-3") #'split-window-right)
(global-set-key (kbd "C-x C-0") #'delete-window)

;; ido
(use-package ido
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (setq resize-mini-widows t
	ido-use-virtual-buffers t
	ido-auto-merge-work-directories-length -1))




