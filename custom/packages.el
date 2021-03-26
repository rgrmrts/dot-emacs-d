;;; packages.el --- Handles all the packages required

;;; Commentary:

;; This Emacs customization requires a bunch of different packages,
;; this file ensures they're all installed and updated when Emacs
;; boots up for the first time.

;;; License:

;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

;;; Code:

(require 'package)

;; use melpa and elpa to install packages
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; initialize the internal record of packages
(package-initialize)

;; update the packages list if it's empty
(unless package-archive-contents
  (package-refresh-contents))

;; this is the list of packages we're installing
(defvar list-of-packages
  '(ag
    all-the-icons
    all-the-icons-ivy-rich
    clojure-mode
    cider
    counsel
    counsel-projectile
    dockerfile-mode
    doom-modeline
    doom-themes
    exec-path-from-shell
    geiser
    flycheck
    flycheck-guile
    flycheck-inline
    ivy
    ivy-rich
    lsp-mode
    magit
    markdown-mode
    nlinum
    nlinum-relative
    projectile
    python-mode
    rust-mode
    swiper
    toml-mode
    tuareg
    use-package
    yaml-mode))

(defun ensure-all-packages-are-installed (list)
  "Ensures all packages in LIST are installed."
  (dolist (package list)
    (unless (package-installed-p package)
      (package-install package))))

;; runs every time we restart Emacs (which shouldn't be an issue if running
;; on boot and using emacsclient)
(ensure-all-packages-are-installed list-of-packages)

;; configure shell after installing exec-path-from-shell
;; this can potentially be moved into a separate package
;; that configures eshell, but I don't really have a lot
;; config for eshell right now
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(provide 'packages)
;;; packages.el ends here
