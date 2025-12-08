# [Emacs Desktop Window Manager (dwin)](../README.md) - further details

## 1. Desktop global keys

### 1.1 The `_emacs-key` script
The script [`_emacs-key`](etc/bin/_emacs-key) uses emacsclient to 
forward a key to Emacs. You can install it to a user bin directory,
e.g., ~/.local/bin with `dwin-install-emacs-keys-script`
or copy it manually. If dwin does not find in on your PATH,
it will ask to install it.

### <a id="key-bindings">1.2 Key bindings in your window manager / compositor</a>
For KDE/KWin dwin configures desktop global keys automatically. For 
debugging you can check the keys in 
⚙️ KDE/System Settings/Shortcuts.
The files in your `~/.local/share/applications` should look like [etc/_local-share-applications/*.desktop](etc/_local-share-applications/).

For other X11 window managers you have to do this yourself manually.

For 🔀 **directional navigation**, configure the following global shortcuts: 

- `Alt+Left` to `_emacs-key "M-<left>"`,
- `Alt+Right` to `_emacs-key "M-<right>"`,
- `Alt+Up` to `_emacs-key "M-<up>"`,
- `Alt+Down` to `_emacs-key "M-<down>"`.

For basic 🏷️ **named navigation**, configure the following global shortcuts: 

- `Ctrl+F11` to `_emacs-key "C-<f11>"`.

For **toggling** 🏷️ **named navigation** with a single key, configure 
the following global shortcuts: 

- `F11` to `_emacs-key "<f11>"`, and 
- `Alt+F11` to `_emacs-key "M-<f11>"`.

## 2. <a id="why-not-ydotool">Why cannot we just use ydotool to send keys to emacs?</a>

Tools like ydotool seem to be able to send key events only to the
active/focused window and would be able only to implement the cases
where one wants to move from within emacs, not the cases where
one wants to move back to emacs from other applications (or
between other applications). One could reimplement dwin in
bash, then there is no need to input keys into emacs anymore.

Unfortunately sending dbus events to kwin from bash was unstable
for me: running
     ```bash
     qdbus6 org.kde.kglobalaccel /component/kwin \
	 org.kde.kglobalaccel.Component invokeShortcut "Switch Window Left"
	 ```
often (not always) yielded
      ```
     Cannot find org.kde.kglobalaccel.Component in object 
	 /component/kwin at org.kde.kglobalaccel
	 ```
This never happened when sending the same events from emacs.


