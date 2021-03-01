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
(setenv "LIBRARY_PATH" "/usr/local/opt/gcc/lib/gcc/10:/usr/local/opt/libgccjit/lib/gcc/10:/usr/local/opt/gcc/lib/gcc/10/gcc/x86_64-apple-darwin20/10.2.0")

;; some top level config for emacs
(setq comp-speed 3)                          ; native compilation optimization level
(setq gc-cons-threshold 250000000)           ; reduce garbage collection frequency, every 250mb
(setq large-file-warning-threshold 1000000)  ; warn when opening big files
(setq default-directory "~/src/")            ; use custom default directory
(setq whitespace-line-column 100)            ; use 100 char lines as the norm

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(tuareg flycheck-guile geiser which-key use-package toml-mode rust-mode python-mode popup nlinum-relative markdown-mode magit flycheck-inline exec-path-from-shell doom-themes doom-modeline dockerfile-mode counsel-projectile all-the-icons-ivy-rich ag)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
