;;; interface.el --- Emacs look and feel

;;; Commentary:

;; This file handles anything to do with how Emacs looks and feels.
;; Modifications to the interface, font, and the modeline etc are
;; all defined in this file.

;;; License:

;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

;;; Code:

(require 'ivy)
(require 'ivy-rich)
(require 'all-the-icons-ivy-rich)
(require 'counsel-projectile)

;; set font to use with Emacs
(set-frame-font "JetBrains Mono 13" nil t)

;; disable some UI components that are unnecessary
(setq inhibit-startup-screen t)    ; don't show the startup screen
(setq ring-bell-function 'ignore)  ; do not ring bell (sound)
(tool-bar-mode -1)                 ; disable the tool bar
(menu-bar-mode -1)                 ; disable the menu bar on top
(scroll-bar-mode -1)               ; disable vertical scroll bar
(blink-cursor-mode -1)             ; do not blink cursor at position

;; better scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; emacs theme for overall look and feel
(load-theme 'doom-one t)

;; use doom modeline this requires all-the-icons, which is installed under packages.el,
;; but it's still required to run (all-the-icons-install-fonts)
;; TODO: auto-run (all-the-icons-install-fonts) in packages.el maybe?
(doom-modeline-mode 1)

;; use counsel and projectile modes for project switcing etc
(counsel-mode)
(counsel-projectile-mode)

;; use ivy as completion engine, and configure all ivy related stuff
(ivy-mode 1)
(setq ivy-height 17)
(all-the-icons-ivy-rich-mode 1)
(ivy-rich-mode 1)
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;; not sure what this is for, but has something to do with an error on macOS
;; TODO: document this better :)
(setq dired-use-ls-dired nil)

(provide 'interface)
;;; interface.el ends here
