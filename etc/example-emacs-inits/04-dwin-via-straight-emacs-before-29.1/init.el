;; dwin/etc/example-emacs-init/init.el      -*- lexical-binding: t; -*-
;; - an example emacs init.el loading dwin with straight.el
;;   on somewhat older emacs 28.1 and 28.2.
;; - dwin does not run on even older emacs <28.1.

;;_ 1. straight package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;;_ 2. dwin 
(use-package dwin
  :straight (dwin :type git :host github :repo "lsth/dwin")
  :init
  (when (version< emacs-version "29.1")
    ;; for forward compatibility (see dwin-compat.el)
    (setf dwin-compat--set-transient-map-ORIG (symbol-function 'set-transient-map))
    (defalias 'keymap-set #'dwin-compat--keymap-set)
    (defalias 'set-transient-map #'dwin-compat--set-transient-map))
    
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
