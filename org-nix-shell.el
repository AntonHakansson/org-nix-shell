;;; org-nix-shell.el --- Org local nix-shell -*- lexical-binding: t -*-

;; Copyright (C) 2024 Anton Hakansson

;; Maintainer: Anton Hakansson <anton@hakanssn.com>
;; URL: https://github.com/AntonHakansson/
;; Version: 0.1.2
;; Package-Requires: ((emacs "27.1") (org) (envrc))
;; Keywords: org-mode, org-babel, nix, nix-shell

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;  Load a buffer-local nix-shell environment from the current Org buffer and use the
;;  environment in org-mode and org-babel src blocks. `org-nix-shell' works by loading a
;;  [[https://direnv.net/][direnv]] environment, constructed on demand, in an out-of-tree
;;  directory
;;
;;; Basic Usage:
;;
;; First create a nix shell derivation in a src block named `nix-shell'.
;;
;; #+name: nix-shell
;; #+begin_src nix
;;   { pkgs ? import <nixpkgs> {} }:
;;   pkgs.mkShell {
;;     buildInputs = [
;;       pkgs.hello
;;     ];
;;   }
;; #+end_src
;;
;; Then, if `org-nix-shell-mode' is enabled, the shell environment loads before executing
;; a org-babel src block.
;;
;; #+begin_src sh
;; hello
;; #+end_src
;;
;; Alternatively you can also manually load the nix shell with `org-nix-shell-load-direnv'.
;;
;;
;;; NEWS:
;; Version 0.1.2
;; - Introduced `org-nix-shell-dired' that opens the direnv directory with dired.
;; - Fixed `org-nix-shell-ctrl-c-ctrl-c' to never block babel execution.
;; - Updated docs.
;;
;; Version 0.1.1
;; - Introduced `org-nix-shell-mode' that seamlessly loads nix-shell environment on
;;   org-ctrl-c-ctrl-c-hook.
;;
;; Version 0.1.0
;; - Initial release
;;
;;; Code:
(require 'envrc)

(defgroup org-nix-shell nil
  "Buffer-local nix shell environment in `org-mode'."
  :group 'extensions :group 'processes
  :link '(url-link :tag "Homepage" "https://github.com/AntonHakansson/org-nix-shell")
  :prefix "org-nix-shell-")

(defcustom org-nix-shell-get-direnv-path #'org-nix-shell--default-direnv-path
  "Function to get path to a per-buffer direnv directory."
  :type 'function
  :options '(#'org-nix-shell--default-direnv-path)
  :group 'org-nix-shell)

(defcustom org-nix-shell-src-block-name "nix-shell"
  "A unique src block name that specify the nix shell environment."
  :type 'string
  :options '("nix-shell")
  :group 'org-nix-shell)

(defcustom org-nix-shell-envrc-format "use nix"
  "The content of `.envrc'.
Use format string %s to get the direnv path."
  :type 'string
  :options '("use nix")
  :group 'org-nix-shell)

(defvar-local org-nix-shell--hash nil
  "Hash of `org-nix-shell-src-block-name' source block.")

(defun org-nix-shell--default-direnv-path ()
  "The default path used for the direnv environment."
  (format "/tmp/org-nix-shell/%s/" (abs (sxhash (buffer-name)))))

(defun org-nix-shell-ctrl-c-ctrl-c ()
  "If point is at a src block load the environment."
  (when (equal (org-element-type (org-element-at-point)) 'src-block)
    ;; We dont want to error here because that blocks the babel execution. The only case
    ;; where it makes sense to block is when there is a nix shell derivation error AND the
    ;; src block depends on the nix-shell environment.
    (condition-case nil
        (org-nix-shell-load-direnv)
      (error nil))))

;;;###autoload
(defun org-nix-shell-dired ()
  "Open the direnv directory with dired."
  (interactive)
  (let ((direnv-path (funcall org-nix-shell-get-direnv-path)))
        (dired direnv-path)))

;;;###autoload
(defun org-nix-shell-load-direnv ()
  "Construct and load nix shell environment from src block with name `org-nix-shell-src-block-name'."
  (interactive)
  (let* ((direnv-path (funcall org-nix-shell-get-direnv-path))
         (nix-shell-path (concat direnv-path "shell.nix"))
         (dotenvrc-path (concat direnv-path ".envrc"))
         (previous-hash org-nix-shell--hash))

    ;; Tangle shell.nix
    (save-excursion
      (org-babel-goto-named-src-block org-nix-shell-src-block-name)
      (let ((src-block (org-element-at-point)))
        (unless (and (equal (org-element-type src-block) 'src-block)
                     (equal (org-element-property :name src-block) org-nix-shell-src-block-name))
          (error "org-nix-shell: No nix-shell src block found in buffer"))
        (setq org-nix-shell--hash (sxhash src-block))
        (unless (eql org-nix-shell--hash previous-hash)
          (make-directory direnv-path t)
          (org-babel-tangle '(4) nix-shell-path))))

    ;; Create .envrc
    (with-temp-buffer
      (insert (format org-nix-shell-envrc-format direnv-path))
      (write-file dotenvrc-path nil))

    ;; Load direnv
    ;; TODO: show errors from nix-shell
    (unless (eql org-nix-shell--hash previous-hash)
      (let ((default-directory direnv-path))
        (envrc-allow)
        (envrc-reload)))))

;;;###autoload
(define-minor-mode org-nix-shell-mode nil
  "Toggle org-nix-shell-mode."
  :global t
  (if org-nix-shell-mode
      (progn
        (add-hook 'org-ctrl-c-ctrl-c-hook #'org-nix-shell-ctrl-c-ctrl-c))
    (remove-hook 'org-ctrl-c-ctrl-c-hook #'org-nix-shell-ctrl-c-ctrl-c)))

(provide 'org-nix-shell)
;;; org-nix-shell.el ends here
