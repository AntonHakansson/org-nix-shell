;;; org-nix-shell.el --- Org local nix-shell -*- lexical-binding: t -*-

;; Copyright (C) 2024 Anton Hakansson

;; Maintainer: Anton Hakansson <anton@hakanssn.com>
;; URL: https://github.com/AntonHakansson/
;; Version: 0.1.4
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
;;  Use a buffer-local nix-shell environment in org-babel src blocks.
;;  `org-nix-shell' works by seamlessly loading a [[https://direnv.net/][direnv]]
;;  environment, constructed on demand, in an out-of-tree directory before executing
;;  source blocks.
;;
;;; Basic Usage:
;;
;; First create a nix shell derivation in a named src block.
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
;; We run source blocks with the shell using a special :nix-shell header argument.
;;
;; #+begin_src sh :nix-shell "nix-shell"
;; hello
;; #+end_src
;;
;; Then, if `org-nix-shell-mode' is enabled, the shell environment is seamlessly loaded
;; when executing a src block.
;;
;; The :nix-shell header argument is like any other org-mode header argument and can be
;; configured with:
;;
;;        #+PROPERTY: header-args :nix-shell "nix-shell"
;;
;;    or in a property drawer like:
;;
;;        * Org Header
;;        :PROPERTIES:
;;        :header-args: :nix-shell "nix-shell"
;;        :END:
;;
;; See `demo.org' for more.
;;
;;; NEWS:
;; Version 0.2.0   (lots of breaking changes)
;; - Source blocks must explicitly set ':nix-shell <named-src-block>' header argument to
;;   load a nix-shell. To use a nix-shell in a specific scope you can use a header-args property like:
;;
;;        #+PROPERTY: header-args: :nix-shell "nix-shell"  (applies globally)
;;
;;    or in a property drawer like: (applies in subtree)
;;
;;        * Org Header
;;        :PROPERTIES:
;;        :header-args: :nix-shell "nix-shell"
;;        :END:
;;
;;   A global header-arg will mimic the old behavior but remember to set :nix-shell to nil
;;   if your source block should not depend on a nix shell and you care about portability
;;   or performance.
;;
;; - Multiple nix-shells support. See above.
;;
;; - Org export integration.
;;
;; - These improvements were possible thanks to a big redesign in how this package works.
;;   Previously we relied on hooks for evaluating a src block but now we use an "advice"
;;   around `org-babel-execute-src-block' that is more robust.
;;
;; Version 0.1.4
;; - Display shell.nix derivation errors.
;; - Fix `org-nix-shell--ctrl-c-ctrl-c': don't reload direnv every time; reload only when
;;   nix shell src block change.
;; - More tests.
;;
;; Version 0.1.3
;; - Small bug fixes and missing dependencies
;; - Continuous Integration and testing workflow
;;
;; Version 0.1.2
;; - Introduced `org-nix-shell-dired' that opens the direnv directory with dired.
;; - Fixed `org-nix-shell--ctrl-c-ctrl-c': never block babel execution.
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
(require 'org)
(require 'org-element)
(require 'envrc)

;; REVIEW: Drop dependency on envrc and direnv? Use nix-shell directly - how can I get
;;         shell environment variables using nix?

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

(defcustom org-nix-shell-nix-instantiate-executable "nix-instantiate"
  "Executable to use for `nix-instantiate'."
  :type 'string
  :group 'org-nix-shell)

(defvar-local org-nix-shell--hash nil
  "Hash of src block.")

(defun org-nix-shell--default-direnv-path (nix-shell-name)
  "The default path used for the direnv environment."
  (let ((hash (abs (sxhash `(nix-shell-name ,(default-value 'process-environment))))))
    (format "%s/org-nix-shell/%s/" (org-babel-temp-stable-directory) hash)))

(defun org-nix-shell--clear-env ()
  (envrc--clear (current-buffer))
  (setq-local org-nix-shell--hash nil))

(defun org-nix-shell-eval (name)
  "Evaluate nix shell src block with name NAME and inherit environment.

Constructs a direnv directory from src block with name NAME."
  (interactive)
  (let* ((direnv-path (funcall org-nix-shell-get-direnv-path name))
         (nix-shell-path (concat direnv-path "shell.nix"))
         (dotenvrc-path (concat direnv-path ".envrc"))
         (src-block (save-excursion
                      (let ((point (org-babel-find-named-block name)))
                        (if point
                            (progn
                              (goto-char point)
                              (make-directory direnv-path t)
                              (org-babel-tangle '(4) nix-shell-path))
                          (user-error "`%s' src block not found in buffer" name)))
                      (org-element-at-point)))
         (previous-hash org-nix-shell--hash))

    (setq-local org-nix-shell--hash (sxhash src-block))

    ;; Format and write .envrc
    (unless (file-exists-p dotenvrc-path)
      (with-temp-buffer
        (insert (format org-nix-shell-envrc-format direnv-path))
        (write-file dotenvrc-path nil))
      ;; Allow direnv directory
      (let ((default-directory direnv-path))
        (condition-case nil
            (envrc-allow)
          (error nil))))

    (let* ((default-directory direnv-path))
      (if (eql org-nix-shell--hash previous-hash)
          ;; Same, unchanged, shell as last time, so we assume the shell environment is already loaded.
          t
        (org-nix-shell--clear-env)
        ;; On my machine direnv always returns zero exit code(success). We rely on
        ;; 'nix-instantiate' command for nix-shell derivation errors instead.
        (let ((exit-code (envrc--call-process-with-global-env org-nix-shell-nix-instantiate-executable nil (get-buffer-create "*org-nix-shell*") nil "shell.nix")))
          (if (zerop exit-code)
              (envrc-reload)
            (display-buffer "*org-nix-shell*")
            (user-error "Error running nix-instantiate")))))))

(defun org-nix-shell--advice (orig-fun &optional arg info params executor-type)
  "Evaluate nix shell from :nix-shell header argument before executing src block.
Intended to be used as a advice around `org-babel-execute-src-block'.
ORIG-FUN, ARG, INFO, PARAMS"
  ;; Ideally we would like something like a org-babel-before-execute-hook instead of
  ;; "advice"
  (let* ((org-babel-current-src-block-location
          (or org-babel-current-src-block-location
              (nth 5 info)
              (org-babel-where-is-src-block-head)))
         (info (if info (copy-tree info) (org-babel-get-src-block-info)))
         (executor-type
          (or executor-type
              ;; If `executor-type' is unset, then we will make an
              ;; informed guess.
              (pcase (and
                      ;; When executing virtual src block, no location
                      ;; is known.
                      org-babel-current-src-block-location
                      (char-after org-babel-current-src-block-location))
                (?s 'inline-src-block)
                (?c 'inline-babel-call)
                (?# (pcase (char-after (+ 2 org-babel-current-src-block-location))
                      (?b 'src-block)
                      (?c 'call-block)
                      (_ 'unknown)))
                (_ 'unknown)))))
    ;; Merge PARAMS with INFO before considering source block
    ;; evaluation since both could disagree.
    (cl-callf org-babel-merge-params (nth 2 info) params)
    (when (org-babel-check-evaluate info)
      (cl-callf org-babel-process-params (nth 2 info))
      (let* ((params (nth 2 info))
             (nix-shell-cons-cell (assq :nix-shell params))
             (nix-shell-name (cdr nix-shell-cons-cell)))
        (if nix-shell-name
            (org-nix-shell-eval nix-shell-name)
          (org-nix-shell--clear-env)))))
  (funcall orig-fun arg info params executor-type))

;;;###autoload
(define-minor-mode org-nix-shell-mode
  "Toggle `org-nix-shell-mode'."
  :global t
  (if org-nix-shell-mode
      (progn
        (envrc-mode +1)
        (advice-add 'org-babel-execute-src-block :around #'org-nix-shell--advice))
    (advice-remove 'org-babel-execute-src-block #'org-nix-shell--advice)))

(provide 'org-nix-shell)
;;; org-nix-shell.el ends here
