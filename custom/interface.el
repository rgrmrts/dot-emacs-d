;;; interface.el --- Emacs look and feel

;;; Commentary:

;; This file handles anything to do with how Emacs looks and feels.
;; Modifications to the interface, font, and the modeline etc are
;; all defined in this file.

;;; License:

;; MIT License

;; Copyright (c) 2020 rgrmrts

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:

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

;; set theme to doom-solarized-light
(load-theme 'doom-solarized-light t)
;; use doom modeline
(doom-modeline-mode 1)

(provide 'interface)
;;; interface.el ends here
