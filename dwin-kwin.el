;;; dwin-kwin.el --- The window manager / compositor proxy for KDE/KWin   -*- lexical-binding: t; -*-
;;
;; Author: Lars Schmidt-Thieme <schmidt-thieme@ismll.de>
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;
;; Proxy for KDE/KWin window manager / compositor:
;; - works on Wayland and X11.
;; - uses kdotool instead of xdotool.
;; - supports automatic installation of desktop global keys
;;   (simply application launcher files in ~/.local/share/application).
;;   See `dwin-sync-desktopglobal-keys'.

;;; Code:

;;_ 0. requirements
(require 'dwin-core)

;;_ 1. customization
;;_ 1.1 customization for dwin KDE/KWin functions

(defcustom dwin-kde-application-dir
  "~/.local/share/applications/"
  "Directory for user application launchers."
  :type 'directory
  :group 'dwin)

(defcustom dwin-kde-application-filename-template
  "net.local.emacs-key-{key-name}.desktop"
  "Template for the filename of KDE application launchers."
  :type 'string
  :group 'dwin)

(defcustom dwin-kde-application-template
  "[Desktop Entry]
Exec=_emacs-key \"{key-emacs}\"
Name={doc} (emacs desktop key)
NoDisplay=true
StartupNotify=false
Type=Application
X-KDE-GlobalAccel-CommandShortcut=true
X-KDE-Shortcuts={key}
"
  "Template for a KDE application launcher.
The following placeholders will be filled replaced:
- {key}: the key (in wm manager style) to bind.
- {key-emacs}: the key (in Emacs notation) to bind.
- {key-name}: the key with special characters replaced by an underscore.
- {doc}: a short description what the key does."
  :type 'string
  :group 'dwin)

;; this did not work:
;; Exec=emacsclient -e \"(dwin-input-key \\\"{key-emacs}\\\")\"

(defcustom dwin-kde-config-item-template
  "[services][net.local.{}.desktop]
_launch={key}
"
  "Template for a key binding in ~/.config/kglobalshortcutsrc."
  :type 'string
  :group 'dwin)



;;_ 2. general utility functions
;;_ 2.1 file handling
(defun dwin--sanitize-filename (s)
  "Return S transformed into a safe Linux filename."
  (let ((s (downcase s)))
    ;; remove or replace unwanted characters
    (setq s (replace-regexp-in-string "[[:cntrl:]]" "" s))
    (setq s (replace-regexp-in-string "[/]" "_" s))
    (setq s (replace-regexp-in-string "[[:space:]]+" "_" s))
    ;; optional: remove other punctuation
    (setq s (replace-regexp-in-string "[^[:alnum:]._-]" "_" s))
    ;; avoid empty result
    (if (string-empty-p s) "_"
      s)))

(defun dwin--file-to-string (filename)
  "Return the contents of file FILENAME as string.
Returns an empty string, if there is no such file."
  (if (file-exists-p filename)
      (with-temp-buffer
	(insert-file-contents filename)
	(buffer-string))
    ""))

;;_ 2.2 simple templates (with named placeholders "{name}")

(defun dwin--format (fmt alist)
  "Replace {name} in FMT using ALIST of (\"name\" . value)."
  (let ((s fmt)
	;; convert alist to alist of strings:
	(alist-str (mapcar (lambda (cell) (cons (format "%s" (car cell))
						(format "%s" (cdr cell))))
			   alist)) )
    (dolist (pair alist-str s)
      (setq s (replace-regexp-in-string (format "{%s}" (regexp-quote (car pair)))
                                        (cdr pair) s t t)))))

(defun dwin--template-to-regexp (fmt alist)
  "Convert template FMT into a regexp.
ALIST should be an alist mapping template variables to sub-regexp,
e.g., \".*\"."
  ;; quote all regexp special chars but { and }.
  (let* ((special-chars "^$.*+?[]\\()|")
	 (fmt-quoted (replace-regexp-in-string
		      (format "[%s]" (regexp-quote special-chars))
		      (lambda (c) (concat "\\" c))
		      fmt)))
    (dwin--format fmt-quoted alist)))

;;_ 2.3 other utility functions

(defun dwin--first-line-of-docstring (sym)
  "Get the first line of the docstring of SYM."
  (let* ((doc (documentation sym t))
         (first (and doc (car (split-string doc "\n")))))
    first))

;;_ 3. sync desktop global keys with the window manager / compositor (here: for KDE/KWin)

(defun dwin--emacs-key-to-kde (keystr)
  "Convert an Emacs key description KEYSTR (e.g. \"C-M-<f1>\")
into a KDE-style key string (e.g. \"Ctrl+Alt+F1\")."
  (let ((mods '(( "C-" . "Ctrl+" )
                ( "M-" . "Alt+"  )
                ( "S-" . "Shift+" )
                ( "s-" . "Meta+" )))
        (s keystr)
        out)
    ;; Replace modifier prefixes
    (dolist (m mods)
      (when (string-prefix-p (car m) s)
        (push (cdr m) out)
        (setq s (substring s (length (car m))))))

    ;; Normalize angle-bracket keys like <f1>, <left>, <return>
    (when (string-match "^<\\(.*?\\)>$" s)
      (setq s (match-string 1 s)))

    ;; Capitalize angle-bracket keys
    (setq s (capitalize s))

    (apply #'concat (append (nreverse out) (list s)))))

(defun dwin-desktopglobal--kde-create-application (vars)
  "Create KDE application launcher for binding VARS.
VARS is an alist with keys
- key: the key to bind (in wm style)
- key-emacs: the key to bind (Emacs style)
- key-name: the key sanitized to be used as filename.
- doc: a one line description of the function to bind.
Return the filename of the newly created application launcher."
  (dwin-message 2 "create app launcher for %s" (alist-get 'key vars))
  (let* ((filename (dwin--format dwin-kde-application-filename-template vars))
	 (path (expand-file-name filename dwin-kde-application-dir))
	 (contents (dwin--format dwin-kde-application-template vars))
	 (contents-cur (dwin--file-to-string path)))
    (unless (string= contents contents-cur)
      (write-region contents nil path))
    filename))


(defun dwin-kde-sync-desktopglobal-keys ()
  "Sync the desktopglobal keys in Emacs with the window manager.
Currently only implemented for KDE/KWin.
For KDE/KWin this will do:
- create an application launcher file in ~/.local/share/applications
  for every bound key.
- delete application launcher files in ~/.local/share/applications
  no longer bound to a key."
  ;; 1. create application launcher per key:
  (let ((launchers-active nil))
    (dolist (cell dwin-desktopglobal-map)
      (let* ((key (car cell))
	     (fun (cdr cell))
	     (key-wm (dwin--emacs-key-to-kde key))
	     (key-wm-name (dwin--sanitize-filename key-wm))
	     (doc (dwin--first-line-of-docstring fun))
	     (vars (list (cons 'key key-wm) (cons 'key-emacs key)
			 (cons 'key-name key-wm-name) (cons 'doc doc))) )
	(push
	 (dwin-desktopglobal--kde-create-application vars)
	 launchers-active)))
    ;; 2. remove no longer used application launcher files
    (let* ((launcher-re (concat "\\`"
				(dwin--template-to-regexp
				 dwin-kde-application-filename-template
				 '((key . ".*") (key-emacs . ".*")
				   (key-name . ".*") (doc . ".*")))
				"\\'"))
	   (_unused (message "launcher-re: %s" launcher-re))
	   (launchers-all (directory-files dwin-kde-application-dir
					   nil nil t))
	   (_unused (message "launchers-all: %s" launchers-all))
	   (launchers-emacs-key (seq-filter (lambda (s) (string-match-p launcher-re s))
					    launchers-all))
	   (_unused (message "launchers-emacs-key: %s" launchers-emacs-key))
	   (_unused (message "launchers-active: %s" launchers-active))
	   (launchers-unused (cl-set-difference launchers-emacs-key launchers-active
						:test #'string=))
	   (_unused (message "launchers-unused: %s" launchers-unused)) )
      (dolist (launcher launchers-unused)
	;; (delete-file launcher)))))
	(dwin-message 2 "delete-file %s" launcher)
	(delete-file (expand-file-name launcher
				       dwin-kde-application-dir))))))


;;_ 4. wm proxy for KDE/KWin (kdotool)
(defun dwin--new-proxy-kwin ()
  "Create new KWin proxy object with directional window-switching methods."
  (let ((self (dwin--new-proxy-dotool-based)))
    ;; further methods:
    (dwin-extend self
	 (list
	  ;; overwrite attributes (new values)
	  (cons '_class "proxy-kwin")
	  (cons 'dotool-name "kdotool")
	  ;; public methods:
	  ;; "kdotool search --pid <pid>" throws odd error "Error: missing argument for option '--pid'"
	  ;; search for two properties (--all) with an empty condition ("") for the 2nd one works.
          (cons 'search-pid (lambda (pid) (dwin-call self 'dotool
						     "search" "--all"
						     "--pid" (format "%s" pid)
						     "")))
	  (cons 'short-windowid (lambda (id)
				  (substring id 1 5)))
	  ;; desktop global keys:
	  (cons 'sync-desktopglobal-keys #'dwin-kde-sync-desktopglobal-keys)) )
    (when dwin-kwin-use-shortcuts
      (dwin-extend self
	 (list
	  ;; private methods:
	  (cons 'invoke-shortcut (lambda (name)
				   (dwin-message 2 "invoke-shortcut: %s" name)
				   (condition-case err
				       (dbus-call-method
					:session
					"org.kde.kglobalaccel"
					"/component/kwin"
					"org.kde.kglobalaccel.Component"
					"invokeShortcut"
					name)
				     (error (message
					     "KWin D-Bus error: %s"
					     err)))))
	  ;; public methods:
	  (cons 'switch-left  (lambda () (dwin-call self
						    'invoke-shortcut
						    "Switch Window Left")))
	  (cons 'switch-right (lambda () (dwin-call self
						    'invoke-shortcut
						    "Switch Window Right")))
	  (cons 'switch-up    (lambda () (dwin-call self
						    'invoke-shortcut
						    "Switch Window Up")))
	  (cons 'switch-down  (lambda () (dwin-call self
						    'invoke-shortcut
						    "Switch Window Down"))) )))
	 self))

(provide 'dwin-kwin)
;;; dwin-kwin.el ends here
