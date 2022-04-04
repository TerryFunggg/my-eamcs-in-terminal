;; -*- coding: utf-8; -*-

(setq inhibit-startup-screen t)
(defalias 'yes-or-no-p 'y-or-n-p)

;; ----------------------------------------------------------------------------
;; UTF-8 by default
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)

;; ----------------------------------------------------------------------------
;; Define the init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; ----------------------------------------------------------------------------
;; Backup file config
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq backup-by-copying t)
(setq create-lockfiles nil)
(setq auto-save-default nil)

;; ----------------------------------------------------------------------------
;; UI
(menu-bar-mode -1)

(when (version<="26.0.50" emacs-version)
  (global-display-line-numbers-mode))
(column-number-mode 1)


;; ----------------------------------------------------------------------------
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

;; ----------------------------------------------------------------------------
;; Theme
(use-package monokai-theme
  :config
  (load-theme 'monokai t))

;; ----------------------------------------------------------------------------
;; Keys
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key (kbd "C-x C-1") #'delete-other-windows)
(global-set-key (kbd "C-x C-2") #'split-window-below)
(global-set-key (kbd "C-x C-3") #'split-window-right)
(global-set-key (kbd "C-x C-0") #'delete-window)

;; ----------------------------------------------------------------------------
;; ido
(use-package ido
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  ;; show result vertically
  (setf (nth 2 ido-decorations) "\n")
  (setq resize-mini-widows t
	ido-use-virtual-buffers t
	ido-auto-merge-work-directories-length -1
	ido-enable-flex-matching t))

;; ----------------------------------------------------------------------------
;; winner mode
(winner-mode)

;; ----------------------------------------------------------------------------
;; Dired
(progn
  (require 'dired-x)
  (setq dired-dwim-target t)
  (setq dired-recursive-copies (quote always))
  (setq dired-recursive-deletes (quote top)))

;; ----------------------------------------------------------------------------
;; minibuffer config
(progn
  (savehist-mode 1)
  (setq enable-recursive-minibuffers t
        max-mini-window-height 0.5))
  
  

;; ----------------------------------------------------------------------------
;; icomplete
(use-package icomplete
  :config
  (icomplete-mode 1)
  (setq icomplete-separator "\n"
	icomplete-hide-common-prefix nil
	icomplete-in-buffer t))

;; ----------------------------------------------------------------------------
;; Seleciton
(delete-selection-mode 1)

;; ----------------------------------------------------------------------------
;; indentation
(setq-default tab-width 4)
