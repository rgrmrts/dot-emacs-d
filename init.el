;;; init.el --- Emacs initialization file

;;; Commentary:

;; This file will configure and customize Emacs just the way I want it, aiming
;; to be more or less minimal and functional.
;; I got a lot of inspiration from Prelude, which I've used for several years now
;; but decided to kick the and start from scratch.

;;; License:

;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

;;; Code:

;; hack to allow GUI emacs app to find libgccjit for native compilation
(setenv "LIBRARY_PATH" "/usr/local/opt/gcc/lib/gcc/11:/usr/local/opt/libgccjit/lib/gcc/:/usr/local/opt/gcc/lib/gcc/11/gcc/x86_64-apple-darwin20/11.1.0")

;; some top level config for emacs
(setq comp-speed 2)                          ; native compilation optimization level
(setq gc-cons-threshold 250000000)           ; garbage collection frequency, every 50mb
(setq large-file-warning-threshold 1000000)  ; warn when opening big files
(setq default-directory "~/src/")            ; use custom default directory
(setq whitespace-line-column 100)            ; use 100 char lines as the norm

;; store all backup and autosave files in the tmp dir
(setq auto-save-visited-file-name t)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;; cleanup whitespace before saving
(add-hook 'before-save-hook 'whitespace-cleanup)

;; autosave lock files are annoying to deal with, especially with git
(setq create-lockfiles nil)

;; set the path where these configs live
(add-to-list 'load-path "~/src/dot-emacs-d")
(add-to-list 'load-path "~/src/dot-emacs-d/custom")

;; load all the custom packages, ui tweaks, language configurations etc.
(require 'packages)
(require 'keybindings)
(require 'interface)
(require 'languages)

(provide 'init)
;;; init.el ends here
