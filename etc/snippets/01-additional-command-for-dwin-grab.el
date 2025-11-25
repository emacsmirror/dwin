;; dwin: example for extension commands for `dwin-grab'.

(defun my-say-hello ()
  "Test function that just says hello."
  (interactive)
  (message "Hello!"))

(defun my-show-geometry ()
  "Show position and size of `dwin-current-window."
  (interactive)
  (message "geometry: %s"
	   (dwin-call dwin-proxy 'getwindowgeometry dwin-current-window)))

(keymap-set dwin-arrange-keymap "!" #'my-say-hello)
(keymap-set dwin-arrange-keymap "g" #'my-show-geometry)
