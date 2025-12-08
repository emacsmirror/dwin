;;; dwin.el --- Navigate and arrange desktop windows   -*- lexical-binding: t; -*-
;;
;; Author: Lars Schmidt-Thieme <schmidt-thieme@ismll.de>
;; Maintainer: Lars Schmidt-Thieme <schmidt-thieme@ismll.de>
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1") (compat "30.1.0.1"))
;; Keywords: frames, processes, convenience
;; URL: https://github.com/lsth/dwin
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;
;; dwin enables interaction with desktop windows, i.e., windows of
;; external applications such as Firefox or okular, managed
;; by an external X11 window manager or Wayland compositor
;; (the latter currently only for KDE KWin).
;;
;; Provides three types of desktop window interactions:
;; 1. ⧉ arranging desktop windows from within Emacs with keys,
;;    e.g., move windows around, resize them etc.,
;; 2. 🏷️ navigation by name,
;;    e.g., switching to firefox, zotero, back to Emacs, and
;; 3. 🔀 directional navigation,
;;    e.g., moving to the window to the right or below.
;;
;; Install from MELPA with
;;
;;    (use-package dwin
;;      :config
;;      (dwin-setup))
;;
;; ⧉ arrange desktop windows with `dwin-grab', i.e., move them
;; around, resize them, etc.
;;
;; 🏷️ Navigation by name is done with `dwin-switch-to-app' that will
;; i) start an app, if it has no window yet,
;; ii) switch to its window, if it is not active, or
;; iii) switch back to Emacs, if it is already active (toggle).
;; For the latter, switching back to Emacs from outside Emacs,
;; a shortcut command for toggling the application has to be
;; bound desktop global by the window manager / compositor,
;; e.g., via
;;
;;   (defun my/firefox (&optional prefix)
;;     (interactive (list current-prefix-arg))
;;     (dwin-switch-to-app "firefox" prefix))
;;   (dwin-keymap-desktopglobal-set "<f11>" #'my/firefox)
;;
;; By default, navigation by name will switch to the first window of
;; an application, if it has several.  You can use a prefix arg to
;; switch to a specific one, e.g., C-2 M-x my/firefox or C-2 <f11> to
;; switch to the second one.
;;
;; To switch back from outside Emacs to Emacs from any other window,
;; bind `dwin-switch-to-emacs-or' to a desktop global key:
;;
;;   (dwin-keymap-desktopglobal-set "C-<f11>" #'dwin-switch-to-emacs-or)
;;
;;
;; 🔀 Directional navigation between desktop windows and Emacs
;; windows is accomplished by `dwin-windmove-left' that has to be
;; bound to desktop global keys the same way as above:
;;
;;  (dwin-keymap-desktopglobal-set "M-<left>"  #'dwin-windmove-left)
;;  (dwin-keymap-desktopglobal-set "M-<right>" #'dwin-windmove-right)
;;  (dwin-keymap-desktopglobal-set "M-<up>"    #'dwin-windmove-up)
;;  (dwin-keymap-desktopglobal-set "M-<down>"  #'dwin-windmove-down)
;;
;; These functions try to move inside Emacs via windmove, and if
;; this fails, use the window manager to move out of Emacs.
;; The same method also uses the window manager to move
;; directional from desktop windows to other desktop windows
;; or back to Emacs.
;;
;; How it is implemented
;;
;; dwin enables Emacs to use functions provided by the window manager
;; to implement window interactions.  We captured all required
;; methods in a window manager proxy object `dwin-proxy' whose
;; methods can be called via `dwin-call'.  The proxy has to be created
;; once before use, e.g., during Emacs initialization, using
;; `dwin-setup'.  Currently two proxies are implemented:
;; - one for generic X11 window managers and
;; - one for KDE/KWin on Wayland or X11.
;; It should be possible to implement further ones.
;; The X11 proxy uses xdotool.
;; The KDE proxy uses
;; i) dbus calls to org.kde.kglobalaccel (KDE's shortcuts
;;    application), esp.  for directional navigation, and
;; ii) kdotool for navigation by name.
;; See file dwin-kwin.el.
;;
;; That navigation works globally, also outside Emacs, requires that
;; the window manager forwards some keys globally to Emacs;
;; for KDE one can bind a one-line script _emacs-key that uses
;; Emacsclient to forward the key to Emacs, using `dwin-input-key'.
;; The KWin keybinding is accomplished by
;; `dwin-sync-desktopglobal-keys' (which is triggered automatically
;; when you set desktop global keys with `dwin-keymap-desktopglobal-set').
;; It assembles a brief .desktop application file and puts it into
;; KWin's application directory ~/.local/share/applications, where
;; KWin will take it up automatically.
;;
;; See etc/example-emacs-inits/01-dwin-via-package/init.el for an
;; example configuration.
;;
;; Custom variables are in dwin-core.el.
;; 
;; More information on the dwin webpage: https://github.com/lsth/dwin

;;; Code:
;;_ 0. required packages
(require 'dwin-core)  ;; the abstract base class of a wm proxy
(require 'dwin-kwin)  ;; an implementation for KDE KWin

;;_ 1. see dwin-core.el
;;_ 2. see dwin-core.el
;;_ 3. see dwin-core.el

;;_ 4. window manager proxy, here x11 generic
;; we define three object constructors for window manager proxies:
;; 1. proxy-dotool-based: to pool all methods common for xdotool and kdotool.
;;    - in dwin-core.el
;; 2. proxy-x11-generic: to talk to x11 via xdotool, not to any wm specifically.
;;    - here
;; 3. proxy-kwin: to talk to kwin via kdotool and dbus.
;;    - in dwin-kwin.el

;;_ 4.2 wm proxy for X11 generic (xdotool)
(defun dwin-x11--window-type (window property)
"Get X11 PROPERTY of WINDOW."
(let* ((cmd (format "xprop -id %s %s" window property))
       (output (shell-command-to-string cmd))
       (value (when (string-match "\\([^=]+\\) = \\(.*\\)" output)
		(match-string 2 output))))
  value))

(defun dwin-x11--window-normalp (window)
  "Check if X11 WINDOW is of type normal."
  (string= (dwin-x11--window-type window "_NET_WM_WINDOW_TYPE")
	   "_NET_WM_WINDOW_TYPE_NORMAL"))

(defun dwin--new-proxy-x11-generic ()
  "Create new X11 proxy object."
  (let ((self (dwin--new-proxy-dotool-based)))  ;; despite origin from X11, will also work with Wayland/KDE.
    ;; further methods:
    (dwin-extend self
	 (list
	  ;; overwrite attributes (new values)
	  (cons '_class "proxy-x11-generic")
	  ;; private methods
	  ;; xdotool returns x11 windows of all types (normal, tooltip etc.).
	  ;; Usually we need only the normal ones.
	  ;; Filter windows to retain just the normal ones.
	  ;; TODO: when overwriting methods, keep the overwritten ones and call them here.
	  ;; TODO: also add the filtered version of search-class and search.
	  (cons 'search-filter #'dwin-x11--window-normalp)
	  (cons 'search-pid (lambda (pid &optional pred)
			      "Get all X11 windows for given PID that fulfill PRED."
			      (let* ((pred (or pred (dwin-get self 'search-filter)))
				     (wins (dwin-call self 'dotool
						      "search"  ;;  "--all"
						      "--pid"
						      (format "%s" pid) )))
				;; (dwin-collect wins pred))))
				(seq-filter pred wins))))
	  (cons 'search-class (lambda (class &optional pred)
				"Get all X11 windows of a given CLASS that fulfill PRED."
				(let* ((pred (or pred (dwin-get self 'search-filter)))
				       (wins (dwin-call self 'dotool
							"search" "--class" class)))
				  ;; (dwin-collect wins pred))))
				  (seq-filter pred wins))))
	  ;; = super.search: (as &rest args and &optional pred cannot be combined)
	  (cons 'search-unfiltered (lambda (&rest query-args)
			  "Get all X11 windows matching query formulated by QUERY-ARGS.
Does yield all windows without applying any filters."
			   (dwin-call-apply self 'dotool
					    (append (list "search") query-args) )))
	  (cons 'search (lambda (&rest query-args)
			  "Get all X11 windows matching query formulated by QUERY-ARGS.
Filters using `search-filter'."
			  (let* ((pred (dwin-get self 'search-filter))
				 (wins (dwin-call-apply self 'search-unfiltered query-args)))
			    ;; (dwin-collect wins pred)))) ))))
			    (seq-filter pred wins)))) ))))

;;_ 4.3 wm proxy for KDE/KWin (kdotool)
;; see dwin-kwin

;;_ 4.4 setting up the wm proxy
(defvar dwin-proxy nil
  "Proxy object for the current window manager.")

(defun dwin-get-window-manager ()
  "Return the name of the current window manager.
Detects KWin via D-Bus, EXWM if running under Emacs,
otherwise returns \"unknown\"."
  (cond
   ;; EXWM detection
   ((and (featurep 'exwm) (bound-and-true-p exwm--connection))
    "exwm")
   ;; KWin detection via D-Bus (check if service exists)
   ((and (fboundp 'dbus-call-method)
         (condition-case nil
             ;; List names on the session bus and check for
	     ;; org.kde.KWin
             (member "org.kde.KWin"
                     (dbus-call-method :session
                                       "org.freedesktop.DBus"
                                       "/org/freedesktop/DBus"
                                       "org.freedesktop.DBus"
                                       "ListNames"))
           (error nil)))
    "kwin")
   ((string= (getenv "XDG_SESSION_TYPE") "x11")
    "X11-generic")
   ;; fallback
   (t "unknown")))

(defun dwin-setup-proxy ()
  "Setup the desktop window manager dwin.
Create a proxy object to interact with the current window manager.
The object is stored in `dwin-proxy'."
  (let ((wm (dwin-get-window-manager)))
    (pcase wm
      ("kwin" (setq dwin-proxy (dwin--new-proxy-kwin)))
      ("x11-generic" (setq dwin-proxy (dwin--new-proxy-x11-generic)))
      (_ (progn
	   (setq dwin-proxy nil)
	   (message "no proxy for window manager '%s'." wm) )))))

;;;###autoload
(defun dwin-setup ()
  "Setup the desktop window manager dwin.
Create a proxy object via `dwin-setup-proxy' to interact with the current
window manager / compositor and store in `dwin-proxy'.
Also start the Emacs server, if not running already."
  (unless (server-running-p)
    (server-start))
  (dwin-setup-proxy)
  (unless (executable-find "_emacs-keys")
    (dwin-install-emacs-keys-script))
  (dwin-message 1 "dwin %s setup." (dwin-get dwin-proxy '_class)))

;;_ 4.5 convenience functions using the setup wm proxy
(defun dwin-switch-to-desktop (number &optional relative)
  "Switch to desktop NUMBER.
If RELATIVE is t, switch relative to the current desktop."
  (dwin-call dwin-proxy 'set_desktop number relative))

(defun dwin-get-all-windows ()
  "Get a list of all current/valid/live window ids."
  (dwin-call dwin-proxy 'search-class ""))

(defun dwin-window-alive-p (window)
  "Return t, if WINDOW is still alive.
A window is considered alive, if it still is reported by search."
  (let ((windows (dwin-get-all-windows)))
    (when (member window windows) t)))

(defun dwin-filter-alive-windows (windows)
  "Return the sublist of WINDOWS that are still alive."
  (cl-intersection windows (dwin-get-all-windows)))
  
(defun dwin-sync-desktopglobal-keys ()
  "Sync the desktopglobal keys in Emacs with the window manager."
  (dwin-call dwin-proxy 'sync-desktopglobal-keys))


;;_ 5. navigation by name
(defvar dwin-process-per-app (make-hash-table :test 'equal)
  "Keep track of processes of apps started from within Emacs.
This deliberately does not cover processes started outside Emacs.
The key is the cmd used to start the process.
The value is an alist with fields
- process: the process object,
- windows: windows created briefly after startup.
  (i.e., new windows spawned as reaction to a launcher
  app like \='firefox --new-window\='.
Used by `dwin-switch-to-app'.")

(defvar dwin-last-window-per-app (make-hash-table :test 'equal)
  "Keep track of the window the user has explicitly selected last time.
Used by `dwin-switch-to-app'.")

(defun dwin-reset-window-memory ()
  "Reset windows remembered for last app started and last window requested."
  (setq dwin-process-per-app (make-hash-table :test 'equal))
  (setq dwin-last-window-per-app (make-hash-table :test 'equal)))

(defun dwin-buffer-first-line (&optional buf)
  "Get the first line of buffer BUF, w/o properties."
  (with-current-buffer (or buf (current-buffer))
      (save-excursion
	(goto-char (point-min))
	(buffer-substring-no-properties
	 (point)
	 (line-end-position)))) )

(defun dwin-buffer-link (buf-or-name &optional name)
  "Return a buffer link to BUF-OR-NAME.
Use NAME as link text, if given.
Can be used in messages."
  (let* ((buf (get-buffer buf-or-name))
	 (link-text (or name (buffer-name buf))))
    (propertize link-text
		'mouse-face 'highlight
		'help-echo "mouse-1: switch"
		'local-map (let ((map (make-sparse-keymap)))
                             (define-key map [mouse-1]
					 (lambda () (interactive)
					   (switch-to-buffer buf)))
                             map))))

(defun dwin-message-with-link (link &rest args)
  "Message the user, including a (propertized) LINK in the *messages* buffer.
ARGS contain the message format and arguments."
  (apply #'message args)
  (with-current-buffer "*Messages*"
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert link "\n"))))
  

;;;###autoload
(defun dwin-run (cmd)
  "Launch application/shell command CMD.
Command name or arguments with spaces have to be quoted with \"\",
like in a shell.
Keeps track of the last application launched in
`dwin-process-per-app'."
  (interactive (list (read-shell-command "$ ")))
  (let* ((buf-name-output (generate-new-buffer-name
			   (concat "* " cmd " -- output *")))
	 (result nil)
	 (wins-before (dwin-call dwin-proxy 'search-class ""))
	 (proc (start-process-shell-command cmd buf-name-output cmd)))
    (dwin-message 2 "dwin-run proc=%s" proc)
    ;; check if it dies immediately
    (sleep-for dwin-app-startup-grace-period) ;; let the process start
    (cond ((and (not (process-live-p proc))
		(not (eq (process-exit-status proc) 0)))
	   ;; a. process ended with error
	   (let ((error-msg (format "❌ error starting %s: %s"
				    cmd
				    (dwin-buffer-first-line buf-name-output))))
	     (dwin-message-with-link
	      (dwin-buffer-link buf-name-output "[📄output]")
	      error-msg)
	     (setq result (list (cons 'error error-msg))) ))
	  ((not (process-live-p proc))
	   ;; b. process ended w/o error: maybe it created a window? (launcher)
	   (let* ((wins-after (dwin-call dwin-proxy 'search-class ""))
		  (wins-new (cl-set-difference wins-after wins-before :test #'equal)))
	     ;; remember new windows:
	     (setq result (list (cons 'windows wins-new)) )))
	  (t
	   ;; c. process runs: remember process & pid:
	   (setq result (list (cons 'process proc)))))
    (when (not (alist-get 'error result nil))
      ;; do not record errors
      (puthash cmd result dwin-process-per-app))
    result))

(defun dwin-window-for-command-started-by-us (cmd)
  "Return the window of the command CMD started last by us via `dwin-run'."
  (let* ((proc-or-windows (gethash cmd dwin-process-per-app nil))
	 (proc (alist-get 'process proc-or-windows nil))
	 ;; a. windows captured at startup
	 (windows-start (alist-get 'windows proc-or-windows nil))
	 ;; 
	 (pid (when (process-live-p proc)
		(process-id proc)))
	 ;; b. windows associated with the process
	 (windows-pid (when pid
			(dwin-call dwin-proxy 'search-pid pid)))
	 (windows (append windows-pid windows-start))
	 (win (when windows
		(nth 0 windows))) ) ; for now just take the first
    win))

(defun dwin-expand-tilde-only (path)
  "Expand a leading ~ in PATH, but leave the rest of the path relative."
  (if (string-match-p "^~" path)
      (concat (getenv "HOME") (substring path 1))
    path))

(defun dwin-normalize-shell-command (cmd)
  "Normalize a shell command CMD, expanding all ~ to the home directory.
Only the normalized command we then can find with pgrep."
  (let* ((cmd-parts (split-string-and-unquote cmd))
	 (cmd-parts-norm (mapcar #'dwin-expand-tilde-only cmd-parts))
	 (cmd-norm (combine-and-quote-strings cmd-parts-norm)))
    cmd-norm))

(defun dwin-find-pids-for-command (cmd)
  "Find all PIDs for a given CMD."
  (let* ((cmd-norm (dwin-normalize-shell-command cmd))
	 (pgrep-cmd (concat "pgrep -f " (combine-and-quote-strings
					 (list cmd-norm))))
	 (pids-text (split-string (shell-command-to-string pgrep-cmd)))
	 (pids (mapcar #'string-to-number pids-text)))
	 pids))

(defun dwin-find-windows-for-command (cmd)
  "Find all windows for a given CMD.
Check two sources:
- all running processes having CMD as command and their windows.
- windows created when we started CMD."
  (let* ((pids (dwin-find-pids-for-command cmd))
	 (windows-processes (mapcan (lambda (pid) (dwin-call dwin-proxy
							     'search-pid pid))
				    pids))
	 (windows-start (dwin-filter-alive-windows
			 (alist-get 'windows
				    (gethash cmd dwin-process-per-app nil) nil)))
	 (windows (append windows-processes windows-start)))
    (dwin-message 2 "win/cmd: '%s'\n  @proc: %s\n  @start: %s"
		  cmd windows-processes windows-start)
    windows))

(defun dwin-select-frame-set-input-focus ()
  "Give Emacs frame focus and raise it.
Same as `select-frame-set-input-focus', but the latter on some
machines did not work for me."
  (interactive)
  (let* ((wins (dwin-call dwin-proxy 'search-pid (emacs-pid)))
	 (window (when wins (nth 0 wins))))
    (when window
      (dwin-call dwin-proxy 'windowactivate window)
      (dwin-call dwin-proxy 'windowraise window))))

;;;###autoload
(defun dwin-switch-to-app (cmd &optional prefix)
  "Launch app CMD, if it has not been started yet.
Otherwise activate its window, if it is not active yet.
Otherwise activate Emacs (toggle app, go back to Emacs).
If there are more than one window, we will activate:
- the one explicitly requested via the PREFIX argument
  (starting at 1, not 0),
- otherwise the last one explicitly requested
  (remembered in `dwin-last-window-per-app'),
- otherwise the (last) one started within Emacs
  (remembered in `dwin-process-per-app'),
- otherwise the first one."
  (interactive
   (list ;; (read-from-minibuffer "switch to app: ")
	 (read-shell-command "switch to app: ")
	 ;; TODO@2027/11: replace the following line by the modern standard,
	 ;; for backwards compatibility keep the old for now.
	 ;; Also edit README.md and the examples accordingly.
	 ;; "P"))
	 (list current-prefix-arg)))
  ;; 1. check windows
  (let* ((wins (dwin-find-windows-for-command cmd))
	 (active-win (dwin-call dwin-proxy 'getactivewindow))
	 (is-active (member active-win wins))
	 ;;
	 ;; 2. handle the prefix arg
	 (prefix-num (prefix-numeric-value prefix))
	 (win-prefix nil))
    (when (and wins prefix (<= prefix-num (length wins)) (> prefix 0))
      (setq win-prefix (nth (- prefix-num 1) wins))
      (puthash cmd win-prefix dwin-last-window-per-app) )
    (let ((win (or
		win-prefix
		(gethash cmd dwin-last-window-per-app nil)
		(dwin-window-for-command-started-by-us cmd)
		(nth 0 wins))))
      ;; 3. do it
      (cond
       ((not wins)
	(dwin-message 1 "launch %s" cmd)
	(dwin-run cmd) )
       ((not is-active)
	(dwin-message 1 "activate %s" cmd)
	(dwin-call dwin-proxy 'windowactivate win)
	nil  ; return nothing
	)
       (t
	(dwin-message 1 "toggle/reactivate Emacs")
	(dwin-switch-to-emacs-or) )))))

;;;###autoload
(defun dwin-switch-to-emacs-or ()
  "Switch back to Emacs, if Emacs currently is not the active window.
Otherwise call `switch-to-buffer'.
The later function can be customized in
`dwin-switch-to-emacs-function'."
  (interactive)
  (if (not (frame-focus-state))
      ;; (select-frame-set-input-focus (selected-frame))
      (dwin-select-frame-set-input-focus)
    (call-interactively dwin-switch-to-emacs-function)))

;;_ 6. directional navigation
(defun dwin-windmove--move (move-emacs wm-cmd)
  "Move to a window in a direction.
First checks for Emacs window via function MOVE-EMACS.
Then tries external windows via `dwin-proxy'/WM-CMD."
  (if (frame-focus-state)
      ;; 1. Emacs focused: try to move inside first
      (condition-case _
	  (funcall move-emacs)
	(error
	 (dwin-call dwin-proxy wm-cmd)))
    ;; 2. Emacs not focused: move from external window directly.
    (dwin-call dwin-proxy wm-cmd)) )

;;;###autoload
(defun dwin-windmove-left ()
  "Move to the window on the left.
First checks for Emacs windows via `windmove-left'.
Then tries X windows via `dwin-proxy'/switch-left."
  (interactive)
  (dwin-windmove--move #'windmove-left 'switch-left))

;;;###autoload
(defun dwin-windmove-right ()
  "Move to the window on the right.
First checks for Emacs windows via `windmove-right'.
Then tries X windows via `dwin-proxy'/switch-right."
  (interactive)
  (dwin-windmove--move #'windmove-right 'switch-right))

;;;###autoload
(defun dwin-windmove-up ()
  "Move to the window one up.
First checks for Emacs windows via `windmove-up'.
Then tries X windows via `dwin-proxy'/switch-up."
  (interactive)
  (dwin-windmove--move #'windmove-up 'switch-up))

;;;###autoload
(defun dwin-windmove-down ()
  "Move to the window one down.
First checks for Emacs windows via `windmove-down'.
Then tries X windows via `dwin-proxy'/switch-down."
  (interactive)
  (dwin-windmove--move #'windmove-down 'switch-down))


;;_ 7. arrange windows
(defun dwin-resize (window width height &optional relative)
  "Set WINDOW to a new size WIDTH and HEIGHT.
If RELATIVE is t, make the new size relative to the old one."
  (when relative
    (let ((geom (dwin-call dwin-proxy 'getwindowgeometry window)))
      (setq width (+ width (nth 2 geom)))
      (setq height (+ height (nth 3 geom))) ))
  (dwin-call dwin-proxy 'windowsize window width height))

(defun dwin--window-names ()
  "Get a list of all windows as (id . name) pairs (cons cells)."
  (let* ((wins (dwin-call dwin-proxy 'search-class ""))
	 (names (mapcar (dwin-get dwin-proxy 'getwindowname) wins))
	 (names-with-ids (cl-mapcar (lambda (a b)
				      (format "%s (%s)" a
					      (dwin-call dwin-proxy 'short-windowid b)))
					      ;; b))
				    names wins))
	 (name-id-pairs (cl-mapcar 'cons names-with-ids wins)))
    name-id-pairs))

;; (dwin-setup)
;; (dwin--window-names)
;; (dwin-call dwin-proxy 'getwindowname "{0b82ec55-ef36-4a0b-8de6-5c201ac0039f}")
;; (dwin-call dwin-proxy 'short-windowid "{0b82ec55-ef36-4a0b-8de6-5c201ac0039f}")

(defvar dwin--history-window-names nil
  "History of window names picked by the user.
Used in `dwin-grab'.")

(defun dwin-read-window (&optional prompt)
  "Let the user select a window in the minibuffer.
Show PROMPT.
Returns a pair/cons (NAME . ID)."
  (let ((window-name-id-pairs (dwin--window-names)))
    (assoc
     (completing-read (or prompt "window: ") window-name-id-pairs
		      nil t nil 'dwin--history-window-names)
     window-name-id-pairs)))

(defun dwin-current-window-or-ask (prompt)
  "Return the `dwin-current-window', if bound and set; otherwise ask the user.
PROMPT gives a short text to use when asking.
Uses `dwin-read-window' to ask the user.
`dwin-current-window' should be set only locally."
  (if (and (boundp 'dwin-current-window) dwin-current-window)
      dwin-current-window
    (dwin-read-window prompt)))

;; short commands with some doc, so we can bind them to keys
(defun dwin-select (window)
  "Select WINDOW as `dwin-current-window'."
  (interactive (list (dwin-read-window "window: ")))
  (setq-local dwin-current-window (cdr window)))

(defun dwin-select-emacs ()
  "Select Emacs' own window as `dwin-current-window'."
  (interactive)
  (let ((wins (dwin-call dwin-proxy 'search-pid (emacs-pid))))
    (when wins
      (setq-local dwin-current-window (nth 0 wins)))))

;; (setq dwin-current-window nil)  (setq win nil) window

(defun dwin-activate (window)
  "Activate WINDOW.
If missing, take `dwin-current-window', else ask the user."
  (interactive (list (dwin-current-window-or-ask "window: ")))
  (dwin-call dwin-proxy 'windowactivate window))

(defun dwin-raise (window)
  "Raise WINDOW.
If missing, take `dwin-current-window', else ask the user."
  (interactive (list (dwin-current-window-or-ask "window: ")))
  (dwin-call dwin-proxy 'windowraise window))

(defun dwin-close (window)
  "Close WINDOW.
If missing, take `dwin-current-window', else ask the user."
  (interactive (list (dwin-current-window-or-ask "window: ")))
  (dwin-call dwin-proxy 'windowclose window))

(defun dwin-minimize (window)
  "Minimize WINDOW.
If missing, take `dwin-current-window', else ask the user."
  (interactive (list (dwin-current-window-or-ask "window: ")))
  (dwin-call dwin-proxy 'windowminimize window))

(defun dwin-move-left (window)
  "Move WINDOW to the left.
If missing, take `dwin-current-window', else ask the user."
  (interactive (list (dwin-current-window-or-ask "window: ")))
  (dwin-call dwin-proxy 'windowmove window (- dwin-move-x) 0 t))

(defun dwin-move-right (window)
  "Move WINDOW to the right.
If missing, take `dwin-current-window', else ask the user."
  (interactive (list (dwin-current-window-or-ask "window: ")))
  (dwin-call dwin-proxy 'windowmove window dwin-move-x 0 t))

(defun dwin-move-up (window)
  "Move WINDOW up.
If missing, take `dwin-current-window', else ask the user."
  (interactive (list (dwin-current-window-or-ask "window: ")))
  (dwin-call dwin-proxy 'windowmove window 0 (- dwin-move-y) t))

(defun dwin-move-down (window)
  "Move WINDOW down.
If missing, take `dwin-current-window', else ask the user."
  (interactive (list (dwin-current-window-or-ask "window: ")))
  (dwin-call dwin-proxy 'windowmove window 0 dwin-move-y t))

(defun dwin-resize-decrease-width (window)
  "Decrease width of WINDOW.
If missing, take `dwin-current-window', else ask the user."
  (interactive (list (dwin-current-window-or-ask "window: ")))
  (dwin-resize window (- dwin-resize-x) 0 t))

(defun dwin-resize-increase-width (window)
  "Increase width of WINDOW.
If missing, take `dwin-current-window', else ask the user."
  (interactive (list (dwin-current-window-or-ask "window: ")))
  (dwin-resize window dwin-resize-x 0 t))

(defun dwin-resize-decrease-height (window)
  "Decrease height of WINDOW.
If missing, take `dwin-current-window', else ask the user."
  (interactive (list (dwin-current-window-or-ask "window: ")))
  (dwin-resize window 0 (- dwin-resize-y) t))

(defun dwin-resize-increase-height (window)
  "Increase height of WINDOW.
If missing, take `dwin-current-window', else ask the user."
  (interactive (list (dwin-current-window-or-ask "window: ")))
  (dwin-resize window 0 dwin-resize-y t))

(defun dwin-move-to-desktop (window desktop)
  "Move WINDOW to DESKTOP.
If missing, take `dwin-current-window', else ask the user."
  (interactive (list
		(dwin-current-window-or-ask "window: ")
		(read-minibuffer "move window to desktop number: ")))
  (dwin-call dwin-proxy 'set_desktop_for_window window desktop))

(defvar dwin-arrange-keymap
  (let ((map (make-sparse-keymap)))
    ;; choose window
    (keymap-set map "W" #'dwin-select)
    (keymap-set map "." #'dwin-select-emacs)
    ;; visibility
    (keymap-set map "a" #'dwin-activate)
    (keymap-set map "c" #'dwin-close)
    (keymap-set map "M" #'dwin-minimize)
    (keymap-set map "r" #'dwin-raise)
    ;; move
    (keymap-set map "<left>"  #'dwin-move-left)
    (keymap-set map "<right>" #'dwin-move-right)
    (keymap-set map "<up>"    #'dwin-move-up)
    (keymap-set map "<down>"  #'dwin-move-down)
    (keymap-set map "D"       #'dwin-move-to-desktop)
    ;; resize
    (keymap-set map "-"   #'dwin-resize-decrease-width)
    (keymap-set map "+"   #'dwin-resize-increase-width)
    (keymap-set map "M--" #'dwin-resize-decrease-height)
    (keymap-set map "M-+" #'dwin-resize-increase-height)
    ;; quit
    (keymap-set map "q" 'ignore) ; quit
    map)
  "Keymap for dwin arrange commands.")

(defun dwin-grab (window)
  "Arrange WINDOW interactively using `dwin-arrange-keymap'."
  (interactive
   (list (cdr (dwin-read-window "window to arrange: "))))
  (save-excursion
    (switch-to-minibuffer)
    (setq-local dwin-current-window window)
    (set-transient-map
     dwin-arrange-keymap
     (lambda () (not (eq this-command 'ignore)))
     (lambda () (message "arranging windows done."))
     "arrange window %k: ")))

(provide 'dwin)
;;; dwin.el ends here
