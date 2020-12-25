;;; init.el --- Emacs initialization file

;;; Commentary:

;; This file will configure and customize Emacs just the way I want it, aiming
;; to be more or less minimal and functional.
;; I got a lot of inspiration from Prelude, which I've used for several years now
;; but decided to kick the and start from scratch.

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

;; some top level config for emacs
(setq comp-speed 3)                            ; native compilation optimization level
(setq gc-cons-threshold 250000000)             ; reduce garbage collection frequency, every 250mb
(setq large-file-warning-threshold 100000000)  ; warn when opening big files
(setq default-directory "~/src/")               ; use custom default directory
(setq whitespace-line-column 100)              ; use 100 char lines as the norm

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
