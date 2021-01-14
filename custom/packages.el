;;; packages.el --- Handles all the packages required

;;; Commentary:

;; This Emacs customization requires a bunch of different packages,
;; this file ensures they're all installed and updated when Emacs
;; boots up for the first time.

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

(require 'package)
(require 'cl-extra)

;; use melpa and elpa to install packages
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; initialize the internal record of packages
(package-initialize)

;; update the packages list if it's empty
(unless package-archive-contents
  (package-refresh-contents))

;; this is the list of packages we're installing
(defvar packages
  '(doom-modeline
    doom-themes
    nlinum
    nlinum-relative
    ag
    all-the-icons
    helm
    helm-ag
    helm-projectile
    ivy
    projectile
    use-package
    flycheck
    flycheck-inline
    swiper
    magit))

(defun check-or-install (package)
  "Install a PACKAGE unless it's already installed."
  (unless (package-installed-p package)
    (package-install package)))

(defun ensure-all-packages-are-installed (list)
  "Ensures all packages in LIST are installed."
  (cl-every #'check-or-install list))

;; runs every time we restart Emacs (which shouldn't be an issue if running
;; on boot and using emacsclient)
(ensure-all-packages-are-installed packages)

(provide 'packages)
;;; packages.el ends here
