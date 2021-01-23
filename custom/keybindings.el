;;; keybindings.el --- Emacs look and feel

;;; Commentary:

;; All custom keybindings are defined in this file.

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

(require 'projectile)

;; set mac command and option keys
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; specific and custom key-bindings are all defined here
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-unset-key (kbd "C-x C-c"))
(global-set-key (kbd "C-x C-c C-x C-c C-x C-c") 'save-buffers-kill-emacs)
(global-unset-key (kbd "C-x C-f"))
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-b") 'counsel-switch-buffer)
(global-set-key (kbd "C--") 'undo)
(global-set-key (kbd "C-=") 'undo-redo)
(global-set-key (kbd "C-s") 'swiper)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(provide 'keybindings)
;;; keybindings.el ends here
