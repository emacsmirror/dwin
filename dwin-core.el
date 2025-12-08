;;; dwin-core.el --- Core functions needed for all platforms   -*- lexical-binding: t; -*-
;;
;; Author: Lars Schmidt-Thieme <schmidt-thieme@ismll.de>
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;
;; Core functions of dwin, required for all platforms.

;;; Code:

;;_ 0. required packages
(require 'cl-lib)
(require 'seq)
(require 'dbus)
(require 'server)

(declare-function keymap-set nil)
(when (version< emacs-version "29.1")  ; 28.2: needs it; 29.1: does not need it
  (require 'dwin-compat))

;;_ 1. some customization
;;_ 1.1 customization for dwin functions
(defgroup dwin nil
  "Customization options for the desktop window manager (dwin) package."
  :group 'applications
  :prefix "dwin-")

(defcustom dwin-switch-to-emacs-function 'switch-to-buffer
  "Function `dwin-switch-to-emacs-or' will call when already in Emacs."
  :type 'function
  :group 'dwin)

(defcustom dwin-move-x 100
  "Default positions to move a window horizonally."
  :type 'integer
  :group 'dwin)

(defcustom dwin-move-y 100
  "Default positions to move a window vertically."
  :type 'integer
  :group 'dwin)

(defcustom dwin-resize-x 100
  "Default positions to resize a window horizontally."
  :type 'integer
  :group 'dwin)

(defcustom dwin-resize-y 100
  "Default positions to resize a window vertically."
  :type 'integer
  :group 'dwin)

(defcustom dwin-kwin-use-shortcuts t
  "If the KDE/KWin proxy should use KWin shortcuts.
If nil, implements the behavior in elisp.
To take effect, the proxy has to be reset via `dwin-setup'."
  :type 'boolean
  :group 'dwin)

(defcustom dwin-app-startup-grace-period 0.3
  "Seconds to wait for an app to start to capture new windows."
  :type 'float
  :group 'dwin)

(defcustom dwin-visited-windows-history-capacity 100
  "Number of recent window visits to remember.
Earlier window visits will be forgotten."
  :type 'integer
  :group 'dwin)

(defcustom dwin-automatically-sync-desktop-global-keys t
  "If t, will call sync desktop global keys automatically.
Whenever set via `dwin-desktopglobal-set'.
During Emacs startup, the sync is delayed until the end of
the startup."
  :type 'boolean
  :group 'dwin)


(defcustom dwin-log-level 1
  "How verbose dwin should send messages to the user.
0 = none, 1 = info, 2 = debug."
  :type '(integer :tag "Log level")
  :group 'dwin)

;;_ 1.2 customization for utility functions

(defcustom dwin-user-bin-directories '("~/.local/bin" "~/bin")
  "Directories where users typically keep binaries."
  :type '(repeat directory)
  :group 'dwin)

;;_ 2. some prerequisites / utilities
;;_ 2.1 simple alist objects
(defun dwin-get (obj attr)
  "Get attribute ATTR (a symbol) of OBJ (an alist object)."
  (let ((ref-cons (assoc attr obj)))
    (if ref-cons
        (cdr ref-cons)
      (error "Attribute/method '%s' not found in obj '%s'" attr obj))))

(defun dwin-set (obj attr val)
  "Set attribute ATTR (a symbol) of OBJ (an alist object) to value VAL."
  (let ((ref-cons (assoc attr obj)))
    (if ref-cons
	(setcdr ref-cons val)
      (error "Attribute/method '%s' not found in obj '%s'" attr obj))))

(defun dwin-call (obj method &rest args)
  "Call METHOD (a symbol) on OBJ (an alist object).
ARGS are passed as arguments to the method function."
  (let ((fn (dwin-get obj method)))
    (apply fn args)))

(defun dwin-call-apply (obj method args)
  "Call METHOD (a symbol) on OBJ (an alist object).
ARGS is a list of arguments provided to the function.
This is different from `dwin-call' where args can be
several arguments that are then captured by a list.
See the following example that one cannot implement
with `dwin-call' (without changing lines 1+2), but
with `dwin-call-apply'.
Example:
    (let* ((x (list (cons \='fun (lambda(&rest args) (apply \='+ args)))))
           (values1 (list 1 2 3)))
      (dwin-call-apply x \='fun (append values1 (list 4 5)))) ;; 15"
  (let ((fn (dwin-get obj method)))
    (apply fn args)))

(defun dwin-extend (self extensions)
  "Update object SELF with new attributes and functions EXTENSIONS (an alist).
If an attribute or a method exists, overwrite it (using `dwin-set').
If an attribute or a method does not exist yet, define it (appending to SELF).
This function operates in place and will mutate SELF.
Better than using `dwin-set' directly, because it also will work for new
attributes and functions."
  (dolist (pair extensions)
    (let ((key (car pair))
          (val (cdr pair)))
      (let ((cell (assoc key self)))
        (if cell
	    ;; a. attribute/method exists: update
            (dwin-set self key val)
          ;; b. new attribute/method: append
          (nconc self (list pair))))))
  self)

(defalias 'dwin-method #'dwin-get
 "Alias for `dwin-get'. Obsolete. Use `dwin-get' directly.")
(make-obsolete 'dwin-method 'dwin-get "0.1.2")

;;_ 2.2 logging
(defun dwin-message (level format &rest args)
  "Send a `message' with FORMAT and ARGS.
Skip, if LEVEL exceeds `dwin-log-level'."
  (when (<= level dwin-log-level)
    (apply #'message format args)))

;;_ 2.3 functions on basic data structures
;; (defun dwin-collect (lst pred)
;;   "Return a sublist of LST of all elements x for which (PRED x) is t."
;;   (cl-loop for x in lst
;;            for r = (funcall pred x)
;;            when r collect x))

(defun dwin-range (a &optional b step)
  "Return a list of integers in a given range.
If only A is given, return integers from 0 to A-1.
If only A and B are given, return integers from A to B-1.
If A, B, and STEP are given, return integers from A to B-1 in steps of STEP."
  (let* ((start (if b a 0))
	 (end (if b b a))
	 (step (or step 1)))
    (cl-loop for i from start below end by step
             collect i)))

(defun dwin-argmin-index (lst pred)
  "Find the index of the element in LST for which PRED returns the smallest value."
  (cl-loop with best-idx = 0
	   with best-val = (funcall pred (nth 0 lst))
	   for x in (cdr lst)
	   for i from 1
           for val = (funcall pred x)
           when (< val best-val)
	   do (setq best-idx i
		    best-val val)
	   finally return best-idx))

(defun dwin-argmin (lst pred)
  "Find the element in LST for which PRED returns the smallest value."
  (cl-loop with best-elem = (nth 0 lst)
	   with best-val = (funcall pred (nth 0 lst))
	   for x in (cdr lst)
           for val = (funcall pred x)
           when (< val best-val)
	   do (setq best-elem x
		    best-val val)
	   finally return best-elem))

(defun dwin-argmax (lst pred)
  "Find the index of the element in LST for which PRED returns the largest value."
  (dwin-argmin lst (lambda (x) (- (funcall pred x)))))

;;_ 2.4 functions for files
(defun dwin-install-binary (path msg)
  "Symlink PATH into a user-local bin directory, if not already installed.
Add MSG if we have to ask the user for permission."
  (interactive "fBinary to install: ")
  ;; 0. ensure PATH is executable
  (set-file-modes path (logior (file-modes path) #o755))
  ;; 1. Determine user bin directory: the first that exists and is in PATH
  (let ((userbin (seq-find (lambda (dir)
                             (and (file-directory-p dir)
                                  (member (expand-file-name dir) exec-path)))
                           dwin-user-bin-directories)))
    (unless userbin
      (user-error "No candidate directory exists and is in PATH: %s"
		  dwin-user-bin-directories))
    ;; 2. target symlink
    (let ((target (expand-file-name (file-name-nondirectory path) userbin)))
      ;; 3. check if already installed
      (if (and (file-exists-p target)
               (file-symlink-p target)
               (string= (file-truename target) (file-truename path)))
          (message "Already installed in %s" target)
        ;; 4. if not installed: ask user
        (when (yes-or-no-p (format "%sSymlink %s into %s? " msg path userbin))
          (make-symbolic-link (expand-file-name path) target)
          (message "Installed %s → %s" path target))))))


;;_ 3. desktop global keys / input keys programatically (e.g., from emacsclient)
;;;###autoload
(defun dwin-input-key (key)
  "Input KEY as if typed by the user.

  Useful to send keys from outside Emacs via emacsclient.
  KEY is a string that `kbd' can understand.
  Does only work for KEYs bound to commands, not for prefix keys.
  Uses and consumes a prefix arg currently typed.
  When using commands that modify buffers from emacsclient,
  you need to set a `current-buffer', e.g.,
    emacsclient -e \\'(with-current-buffer \"*scratch*\"
      (dwin-input-key \"a\"))\\'

  Example: (dwin-input-key \"C-<f11>\")."
  (interactive (list
		(read-from-minibuffer "Key to input: ")))
  (dwin-message 2 "dwin-input-key: %s" key)
  (if (eq this-command 'describe-key)
      ;; 1. the user is trying to get help for the key: show it
      (progn
	(describe-key (kbd key))
	(message "describe-help still active. Say C-g to cancel."))
    ;; 2. otherwise: execute the command bound to the key
    (let* ((key-vector (read-kbd-macro key))
	   (last-command-event (aref key-vector
				     (- (length key-vector) 1)))
					; then self-insert-command
					; will work
	   (cmd (key-binding (kbd key))))
      (if cmd
	  ;; `command-execute' uses prefix arg, `call-interactively'
	  ;; does not !
	  (command-execute cmd nil key-vector)
	(message "no key binding for %s." key) ))))

(defvar dwin-desktopglobal-map nil
  "Keymap/alist for desktop global keys.
Contains cons cells/pairs (key function).")

(declare-function dwin-sync-desktopglobal-keys "dwin.el")

(defun dwin--maybe-auto-sync-desktopglobal-keys ()
  "Sync desktop global keys automatically if requested.
Will sync if `dwin-automatically-sync-desktop-global-keys' is t.
During Emacs startup, will defer sync until the end."
  (when dwin-automatically-sync-desktop-global-keys
    (if after-init-time
	(dwin-sync-desktopglobal-keys)
      ;; otherwise schedule the sync for the end of init,
      ;; so we can build the keymap w/o syncing each binding.
      (unless (member #'dwin-sync-desktopglobal-keys
		      after-init-hook)
	(add-hook 'after-init-hook
		  #'dwin-sync-desktopglobal-keys))) ))
  
(defun dwin-keymap-desktopglobal-set (key fun)
  "Set KEY to function FUN desktop global.
Desktop global means, also outside Emacs, in the desktop
window manager.
This will also set it globally in Emacs via
`keymap-global-set'. To activate desktop keys outside
Emacs, you need to call `dwin-sync-desktopglobal-keys'
afterwards.
KEY has to be a simple key, not a key sequence."
  ;; TODO: check that key is a simple key.
  (setf (alist-get key dwin-desktopglobal-map) fun)
  (keymap-global-set key fun)
  (dwin--maybe-auto-sync-desktopglobal-keys))

(defun dwin-keymap-desktopglobal-unset (key)
  "Unset KEY desktop globally.
Desktop global means, also outside Emacs, in the desktop
window manager.
This will also unset it globally in Emacs via
`keymap-global-unset'. To deactivate desktop keys outside
Emacs, you need to call `dwin-sync-desktopglobal-keys'
afterwards.
KEY has to be a simple key, not a key sequence."
  (setq dwin-desktopglobal-map (assoc-delete-all key dwin-desktopglobal-map 'string=))
  (keymap-global-unset key)
  (dwin--maybe-auto-sync-desktopglobal-keys))

(defun dwin-install-emacs-keys-script ()
  "Install the script _emacs-key in a user bin directory."
  (interactive)
  (let* ((package-dir (file-name-directory (locate-library "dwin")))
	 (script-fn (expand-file-name "bin/_emacs-key" package-dir)))
    (dwin-install-binary script-fn "Install dwin _emacs-key script (required):\n")))


;;_ 4. window manager proxy, here abstract base class
;; we define three object constructors for window manager proxies:
;; 1. proxy-dotool-based: to pool all methods common for xdotool and kdotool.
;;    - here
;; 2. proxy-x11-generic: to talk to x11 via xdotool, not to any wm specifically.
;;    - in dwin.el
;; 3. proxy-kwin: to talk to kwin via kdotool and dbus.
;;    - in dwin-kwin.el

;;_ 4.1 common wm proxy methods for dotool based interaction (xdotool, kdotool)
(defvar dwin--visited-windows nil
  "Windows visited in order of visits, most recent first.")

(defun dwin--record-window-visit (window)
  "Record the visit of WINDOW.
For example, to be able to go back to a previously visited window
later on."
  ;; a. prepend element window
  (setq dwin--visited-windows (cons window dwin--visited-windows))
  ;; b. check full list
  (when (>= (length dwin--visited-windows) dwin-visited-windows-history-capacity)
    (setcdr (nthcdr (- dwin-visited-windows-history-capacity 1) dwin--visited-windows)
	    nil))) ; shorten to capacity

(defun dwin--try-twice (fun &rest args)
  "Call FUN with ARGS. If it returns nil, wait 0.1s and try it again.
Unfortunately, kdotool on some machines seems to have some timing
issues when calling it quickly. Often calling it again gets the
right answer."
  (let ((result (apply fun args)))
    (if result
	result
      (sleep-for 0.1)
      (apply fun args)))) ; try it again

(defun dwin--next-window-in-direction (proxy direction &optional window)
  "Get the next window from WINDOW in DIRECTION.
DIRECTION can be \='left, \='right, \='up or \='down.
If WINDOW is missing, the active window is taken as starting point.
PROXY is the window manager proxy.
TODO: break ties."
  (dwin-message 2 "next window in direction %s" direction)
  (let* ((win-start (or window (dwin-call proxy 'getactivewindow)))
	 ;; never should be nil, tries twice already on its own.
	 (_unused (dwin-message 2 "win-start: %s" win-start))
	 (desktop (dwin--try-twice #'dwin-call proxy 'get_desktop_for_window win-start))
	 ;; should not be nil, so try twice.
	 (_unused (dwin-message 2 "desktop: %s" desktop))
	 (wins (remove win-start
		       (dwin-call proxy 'search "--desktop" (format "%s" desktop) "")))
	 (_unused (dwin-message 2 "wins: %s" wins))
	 (geom-start (dwin--try-twice #'dwin-call proxy 'getwindowgeometry win-start))
	 ;; should not be nil, so try twice; still is sometimes nil...
	 (_unused (dwin-message 2 "geom-start: %s" geom-start))
	 (geoms (mapcar (lambda (win) (dwin-call proxy 'getwindowgeometry win)) wins))
	 (_unused (dwin-message 2 "geoms: %s" geoms))
	 ;;
	 (pred-filter (pcase direction
			(`left  (lambda (idx) (< (nth 0 (nth idx geoms)) (nth 0 geom-start))))
			(`right (lambda (idx) (> (nth 0 (nth idx geoms)) (nth 0 geom-start))))
			(`up    (lambda (idx) (< (nth 1 (nth idx geoms)) (nth 1 geom-start))))
			(`down  (lambda (idx) (> (nth 1 (nth idx geoms)) (nth 1 geom-start))))))
	 (pred-score (pcase direction
		       (`left  (lambda (idx) (- (nth 0 (nth idx geoms)))))
		       (`right (lambda (idx) (nth 0 (nth idx geoms))))
		       (`up    (lambda (idx) (- (nth 1 (nth idx geoms)))))
		       (`down  (lambda (idx) (nth 1 (nth idx geoms))))))
	 (indices-nonnil (seq-filter (lambda (idx) (nth idx geoms))
				     (dwin-range (length wins))))
	 (indices-qual (seq-filter pred-filter indices-nonnil))
	 (_unused (dwin-message 2 "indices-qual: %s" indices-qual))
	 (index-best   (when indices-qual (dwin-argmin indices-qual pred-score)))
	 (_unused (dwin-message 2 "geoms: %s" index-best)) )
    (dwin-message 2 "geom-start: %s" geom-start)
    (dwin-message 2 "geoms: %s" geoms)
    (dwin-message 2 "indices-qual: %s" indices-qual)
    (dwin-message 2 "index-best: %s" index-best)

    (when index-best (nth index-best wins))))

(defun dwin--switch-direction (proxy direction)
  "Switch from active window in DIRECTION using PROXY."
  (dwin-message 2 "dwin--switch-direction: %s" direction)
  (let ((win (dwin--next-window-in-direction proxy direction)))
    (when win
      (dwin-call proxy 'windowactivate win))))

(defun dwin--new-proxy-dotool-based ()
  "Create new (abstract) base object for dotool based proxies."
  (let ((self nil))  ;; forward reference for self
    (setq self
           (list
	    ;; private attributes
	    (cons '_class "proxy-dotool-based")
	    (cons 'dotool-name "xdotool")
	    ;; private methods:
	    ;; yields a list of lines
	  ;; xdotool and kdotool >= 0.2.2 signalsexitcode 1 on empty search results. avoid propagating an error.
	  (cons 'dotool (lambda (&rest args)
			  (let ((exitcode))
                            (condition-case err
				(let*
				    ((lines (apply #'process-lines-handling-status
						   (dwin-get self 'dotool-name)
						   (lambda (status) (setq exitcode status))
						   args)))
				  (if (or (zerop exitcode)
					  (null lines)) ; also OK if no output (empty search results)
				      lines
				    (error (message
					    "%s error: %s\n  exit code %s\n  output: %s"
					    (dwin-get self 'dotool-name)
					    args
					    exitcode
					    (string-join lines)))) )
			      (error (message "%s error: %s\n  %s"
					      (dwin-get self 'dotool-name)
					      args
					      err))))))
            ;; (cons 'dotool (lambda (&rest args)
	    ;; 		    ;; (dwin-message 2 "%s %s" (dwin-get self 'dotool-name) args)
            ;;                 (condition-case err
	    ;; 			(apply #'process-lines
	    ;; 			       (dwin-get self 'dotool-name) args)
	    ;; 		      (error (message "%s error: args %s: %s"
	    ;; 				      (dwin-get self 'dotool-name)
	    ;; 				      (print1-to-string args)
	    ;; 				      err)))))
	    ;; public methods:
            (cons 'switch-left  (lambda () (dwin--switch-direction self 'left)))
            (cons 'switch-right (lambda () (dwin--switch-direction self 'right)))
            (cons 'switch-up    (lambda () (dwin--switch-direction self 'up)))
            (cons 'switch-down  (lambda () (dwin--switch-direction self 'down)))
            (cons 'getactivewindow (lambda ()
				     (let ((wins (dwin-call self 'dotool
							    "getactivewindow")))
				       (if wins
					   (nth 0 wins); there is always only one active window.
					 ;; sometimes kdotool fails to report: try again
					 (sleep-for 0.1)
					 (let ((wins (dwin-call self 'dotool
								"getactivewindow")))
					   (when wins
					     (nth 0 wins)))))))
            (cons 'get_desktop (lambda ()
				 (let ((output (dwin-call self 'dotool "get_desktop")))
				   (when output
				     (string-to-number (nth 0 output))))))
            (cons 'search (lambda (&rest query-args) (dwin-call-apply
						      self 'dotool
						      (append (list "search") query-args) )))
            (cons 'search-class (lambda (class) (dwin-call self 'dotool
							   "search"
							   "--class"
							   class)))
            (cons 'search-pid (lambda (pid) (dwin-call self 'dotool
						       "search" ;; "--all"
						       "--pid"
						       (format "%s" pid) )))
            (cons 'getwindowname (lambda (id) (nth 0 (dwin-call
						      self 'dotool
						      "getwindowname" id))))
            (cons 'getwindowgeometry (lambda (id)
				       (let ((lines (dwin-call
						     self 'dotool
						     "getwindowgeometry"
						     id)))
					 (when (and lines (>= (length lines) 3))
					   (mapcar #'string-to-number
						   (append
						    (split-string
						     (string-trim
						      (replace-regexp-in-string
						       "Position: " ""
						       (nth 1 lines)))
						     "," t)
						    (split-string
						     (string-trim
						      (replace-regexp-in-string
						       "Geometry: " ""
						       (nth 2 lines)))
						     "x" t)))))))
            (cons 'windowactivate (lambda (id)
				    (dwin--record-window-visit id)
				    (dwin-call self 'dotool "windowactivate" id)))
            (cons 'windowclose (lambda (id) (dwin-call self 'dotool
						       "windowclose" id)))
            (cons 'windowminimize (lambda (id) (dwin-call
						self 'dotool
						"windowminimize" id)))
            (cons 'windowraise (lambda (id) (dwin-call
					     self 'dotool
					     "windowraise" id)))
            (cons 'windowsize (lambda (id width height) (dwin-call
							 self 'dotool
							 "windowsize" id
							 (format "%s" width)
							 (format "%s" height))))
            (cons 'windowmove (lambda (id x y &optional relative)
				(if relative
				    (dwin-call self 'dotool
					       "windowmove" "--relative"
					       id (format "%s" x)
					       (format "%s" y))
				  (dwin-call self 'dotool "windowmove" id
					     (format "%s" x) (format "%s" y)))))
            ;; (cons 'windowstate (lambda (id) (dwin-call self 'dotool
	    ;;   "windowstate" id))) ; --add/remove/toggle <property>
            (cons 'get_desktop_for_window (lambda (id)
					    (let ((lines (dwin-call self 'dotool
								    "get_desktop_for_window"
								    (format "%s" id))))
					      (when lines
						(string-to-number (nth 0 lines))))))
            (cons 'set_desktop_for_window (lambda (id number)
					    (dwin-call self 'dotool
						       "set_desktop_for_window"
						       (format "%s" id)
						       (format "%s" number))))
            (cons 'set_desktop (lambda (number &optional relative)
				 (if relative
				     (dwin-call self 'dotool
						"set_desktop" "--relative"
						(format "%s" number))
				   (dwin-call self 'dotool "set_desktop"
					      (format "%s" number)))))
	    (cons 'short-windowid (lambda (id) id))
	    ;; desktop global keys:
	    (cons 'sync-desktopglobal-keys
		  (lambda ()
          	    (error (concat "Only implemented yet for KDE/KWin. "
				   "Configure keys manually!")))) ))
    self))



(provide 'dwin-core)
;;; dwin-core.el ends here
