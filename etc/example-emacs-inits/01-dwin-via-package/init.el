;; dwin/etc/example-emacs-inits/01-dwin-via-package/init.el      -*- lexical-binding: t; -*-
;; - an example emacs init.el loading dwin via package and melpa.

;; 1. package system
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(unless package--initialized (package-initialize))
(unless package-archive-contents (package-refresh-contents))
(require 'use-package)

;; 2. dwin
(use-package dwin
  :ensure t
  :config
  (dwin-setup)
  ;; a. arrange desktop windows
  ;; just use M-x dwin-grab. You can alias it to something shorter, e.g.
  ;; (defalias #'win #'dwin-grab)

  ;; b. directional navigation
  (dwin-keymap-desktopglobal-set "M-<left>"  #'dwin-windmove-left)
  (dwin-keymap-desktopglobal-set "M-<right>" #'dwin-windmove-right)
  (dwin-keymap-desktopglobal-set "M-<up>"    #'dwin-windmove-up)
  (dwin-keymap-desktopglobal-set "M-<down>"  #'dwin-windmove-down)

  ;; c. named navigation
  ;; c.1 move back to emacs
  (dwin-keymap-desktopglobal-set "C-<f11>" #'dwin-switch-to-emacs-or)

  ;; c.2 firefox
  (defun my/firefox (&optional prefix)
    (interactive (list current-prefix-arg))
    (dwin-switch-to-app "firefox" prefix))
  (dwin-keymap-desktopglobal-set "<f11>" #'my/firefox)

  ;; c.3 zotero
  (defun my/zotero (&optional prefix)
    (interactive (list current-prefix-arg))
    (dwin-switch-to-app "zotero" prefix))
  (dwin-keymap-desktopglobal-set "M-<f11>" #'my/zotero)
)
