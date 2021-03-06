;; -*- coding: utf-8; -*-
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq inhibit-startup-screen t)
;; set bookmark as startup
(require 'bookmark)
(list-bookmarks)
(switch-to-buffer "*Bookmark List*")
(setq bookmark-save-flag 1)
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
(setq
 auto-save-default nil)
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
;;(use-package monokai-theme
;;  :config
;;  (load-theme 'monokai t))
(use-package leuven-theme
  :config
  (load-theme 'leuven t))
;; ----------------------------------------------------------------------------
;; ido
(use-package ido
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  ;; show result vertically
  ;; (setf (nth 2 ido-decorations) "\n")
  (setq resize-mini-widows t
	;;ido-use-virtual-buffers t
	;;ido-auto-merge-work-directories-length -1
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
  (setq
   ;;icomplete-separator "\n"
	icomplete-hide-common-prefix nil
	icomplete-in-buffer t))

;; ----------------------------------------------------------------------------
;; Seleciton
(delete-selection-mode 1)

;; ----------------------------------------------------------------------------
;; indentation
(setq-default tab-width 4)

;; ----------------------------------------------------------------------------
;; recentf
(use-package recentf
  :config
  (recentf-mode 1))

;; ----------------------------------------------------------------------------
;; hippie-expand
(setq hippie-expand-try-functions-list
      '(
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        ;; try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-complete-file-name-partially
        try-complete-file-name
        ;; try-expand-all-abbrevs
        ;; try-expand-list
        ;; try-expand-line
		))

;; ----------------------------------------------------------------------------
;; electric
(electric-pair-mode 1)
(setq electric-pair-pairs
      '(
        (?\" . ?\")
        (?\{ . ?\})))

;; ----------------------------------------------------------------------------
;; ace-window
(use-package ace-window
  :config
  (setq aw-dispatch-always t))

;; ----------------------------------------------------------------------------
;; avy-jump
(use-package avy)

;; ----------------------------------------------------------------------------
(use-package htmlize
  :ensure t)

;; ----------------------------------------------------------------------------
;; org mode config
(use-package org
  :init
  (require 'ox)
  (setq my/org-agenda-directory "~/Documents/org-agenda/"
		org-directory "~/Documents/org"
		org-agenda-files (list (concat my/org-agenda-directory "next.org")
                               (concat my/org-agenda-directory "inbox.org")
                               (concat my/org-agenda-directory "repeaters.org")
                               (concat my/org-agenda-directory "projects.org"))
		
		org-hide-emphasis-markers t
		org-export-with-smart-quotes t
		org-html-coding-system 'utf-8-unix
		org-agenda-exporter-settings
		'((ps-number-of-columns 2)
          (ps-landscape-mode t)
          (org-agenda-add-entry-text-maxlines 5)
          (htmlize-output-type 'css))
        ;;org-agenda-span 'day
		;;        org-agenda-start-day "+0d"

		;; some keywords setting
		org-todo-keywords '((sequencep "TODO(t)" "NEXT(n)" "|" "DONE(d)" "HOLD(h)"))
		org-tag-alist (quote (("@home" . ?h)
                              ("@office" . ?o)
                              ("book" . ?b)
                              ("websit" . ?w)
                              ("video" . ?v)
                              ("repeat"  . ?r)
                              ("project" . ?p)))
		;; template
		org-capture-templates
        '(("i" "Inbox" entry (file "~/Documents/org/inbox.org")
           "* TODO %?")
          ("q" "Quick Note" entry (file "~/Documents/org/Drafts.org")
           "* %?\n %T\n "))		
		;; custom agenda
		org-agenda-custom-commands
        '(("p" "Agenda"
           ((agenda ""
                    ((org-agenda-span 'day)))
            (todo "NEXT"
               ((org-agenda-overriding-header "In Progress")
                (org-agenda-files (list
                                   (concat my/org-agenda-directory "projects.org")
                                   (concat my/org-agenda-directory "next.org")))))
            (todo "TODO"
               ((org-agenda-overriding-header "Projects")
                (org-agenda-files (list (concat my/org-agenda-directory "projects.org")))))
            (todo "TODO"
               ((org-agenda-overriding-header "One-off Tasks")
                (org-agenda-files (list (concat my/org-agenda-directory "next.org")))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
            (todo "TODO"
               ((org-agenda-overriding-header "To Refile")
                (org-agenda-files (list (concat my/org-agenda-directory "inbox.org")))))
            nil
			("/home/pi/www/org-agenda/index.html"))))

		org-refile-targets '(((concat my/org-agenda-directory "inbox.org") :level . 0)
							 ((concat my/org-agenda-directory "next.org") :level . 0)
							 ((concat my/org-agenda-directory "done.org") :level .0))))

          ;; ("p" "Post" plain
          ;;       (file create-blog-post)
          ;;       (file "~/.doom.d/post.orgcaptmpl"))
	  


;; ----------------------------------------------------------------------------
;; Keys
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key (kbd "C-x C-1") #'delete-other-windows)
(global-set-key (kbd "C-x C-2") #'split-window-below)
(global-set-key (kbd "C-x C-3") #'split-window-right)
(global-set-key (kbd "C-x C-0") #'delete-window)
(global-set-key (kbd "C-t") #'hippie-expand)
(global-set-key (kbd "C-x v") 'scroll-down-command)
(global-set-key (kbd "C-x :") 'goto-line)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "C-x j") 'avy-goto-char-timer)
(global-set-key (kbd "C-x C-a") 'org-agenda)

;; ----------------------------------------------------------------------------
;; abv.
(defalias 'dw 'kill-word)
(defalias 'mk 'set-mark-command)
(defalias 'hl 'highlight-symbol-at-point)
(defalias 'uhl 'unhighlight-regexp)
(defalias 'eb 'end-of-buffer)
(defalias 'bb 'beginning-of-buffer)
(defalias 'll 'list-matching-lines)
(defalias 'dll 'delete-matching-lines)
(defalias 'cut 'kill-region)
(defalias 'copy 'kill-ring-save)
(defalias 'list-buffers 'ibuffer)

(defun my/save-agenda-view (&optional arg)
  (interactive "P")
  (org-agenda arg "p")
  (org-store-agenda-views))
