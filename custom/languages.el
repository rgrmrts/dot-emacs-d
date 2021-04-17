;;; languages.el --- Provides setup for language integrations

;;; Commentary:

;; This file sets up and configures any language or environment specific
;; configuration and installing tools to make Emacs an integrated IDE like
;; experience.

;;; License:

;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

;;; Code:

;; config for ocaml with merlin
(require 'opam-user-setup "~/src/dot-emacs-d/custom/opam-user-setup.el")

(provide 'languages)
;;; languages.el ends here
