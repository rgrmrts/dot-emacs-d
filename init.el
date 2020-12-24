;;; init.el --- Emacs initialization file

;;; Commentary:

;; This file will configure and customize Emacs just the way I want it

;;; Code:

(setq comp-speed 2)  ; native compilation optimization level
(setq gc-cons-threshold 250000000)  ; reduce garbage collection frequency, every 250mb
(setq large-file-warning-threshold 100000000)  ; warn when opening big files
(setq default-directory "~/src")  ; use custom default directory

(require 'package)

;; use melpa and elpa to install packages
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; initialize the internal record of packages
(package-initialize)

;; update the packages list if it's empty
(unless package-archive-contents
  (package-refresh-contents))

;; install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

;; load all the custom packages, ui tweaks etc.
(require 'custom-packages)
(require 'custom-interface)
(require 'custom-languages)

(provide 'init)
;;; init.el ends here
