# CHANGELOG

## 0.2.1 (work in progress, unreleased)
### 🚀 highlights
### ✨ new features & enhancements

- On KDE/KWin dwin configures desktop global keys now automatically,
  manual adding shortcuts in KDE System Settings / Shortcuts is no longer
  needed.
- The _emacs-key script is installed automatically.
- For KDE/KWin, the key binding now is stored in the application .desktop 
  file directly, making modifying ~/.config/kglobalshortcutsrc superfluous; 
  the respective example file has been removed.
- Other scripts (kwin-left etc.) have been removed and _emacs-key is used
  directly now.

### ❤️ community contributions
### 🐛 bugs fixed

- Fast window switching sometimes triggers timing issues in kdotool,
  that have been remedied by querying it again at least to some extent.

### 💥 breaking changes and deprecations

- Code is now organized in three files: dwin-core.el, dwin-kwin.el and
  dwin.el.

## 0.2.0 (2025-11-26)
### 🚀 highlights

#### dwin now works with any/most X11 window managers

dwin now has two window manager proxies:

1. one for any X11 window manager, controlled through xdotool, and
2. one for KDE/KWin on Wayland or X11, controlled through kdotool
  and dbus messages to KWin.

#### dwin now can switch back to windows created by launchers 

A launcher program asks other processes to create a new window and 
then terminates, e.g., `firefox --new-window <url>`. dwin now 
captures windows created at other application startup, so that you 
can switch back to them later.

### ✨ new features & enhancements

- dwin can now be installed with Emacs' built-in package / use-package
  directly from MELPA.
- users now can add commands to dwin-grab,
  as it has a proper key-map.
- the emacs server no longer has to be started explicitly,
  but dwin-setup starts it, unless it is running already.
- dwin now also runs on somewhat older emacs: from 29.1 onward,
  and with some compatibility measures also from 28.1 onward.

## 0.1.1 (2025-11-23)

### 🐛 bugs fixed

- many small improvements in code formatting and documentation, 
  in preparation for a MELPA submission.

## 0.1 (initial version, 2025-11-22)

### 🚀 highlights

#### dwin lets users arrange desktop windows (`dwin-grab`)

Users can pick any desktop window and reposition and resize it, raise it etc.

#### dwin provides directional navigation for users (`dwin-windmove-left/right/up/down`)

Bound to desktop global keys like `M-<left>/<right>/<up>/<down>`, 
one can navigate to windows in that direction from the currently active window.

#### dwin provides navigation by name (`dwin-switch-to-app`)

Users can switch to any other application like firefox or okular. With a desktop 
global key bound, they also can switch back to emacs and toggle with a key
between both applications.
