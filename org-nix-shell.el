;;; org-nix-shell.el --- Org local nix-shell -*- lexical-binding: t -*-

;; Copyright (C) 2024 Anton Hakansson

;; Maintainer: Anton Hakansson <anton@hakanssn.com>
;; URL: https://github.com/AntonHakansson/
;; Version: 0.3.1
;; Package-Requires: ((emacs "27.1") (org "9.4"))
;; Keywords: processes, outlines

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
;; Use a buffer-local nix-shell environment in org-babel src blocks.
;;
;; Basic Usage:
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
;; when executing a src block or exporting.
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
;;
;;; NEWS:
;; Version 0.3.0   (lots of breaking changes)
;; - Dropped dependency on envrc package. Instead we get the shell environment from
;;   nix-shell using direnv's dump command. No more out-of-tree directory with .envrc.
;;   This gives us a noticeable performance win.
;;
;; - Removed `org-nix-shell-get-direnv-path', `org-nix-shell-envrc-format' and
;;   `org-nix-shell-src-block-name'. Instead we tangle nix shells to \"/tmp/\"
;;
;;
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
(require 'json)

(defgroup org-nix-shell nil
  "Buffer-local nix shell environment in `org-mode'."
  :group 'extensions :group 'processes
  :link '(url-link :tag "Homepage" "https://github.com/AntonHakansson/org-nix-shell")
  :prefix "org-nix-shell-")

(defvar-local org-nix-shell--cache (make-hash-table :test 'equal :size 10)
  "Cached nix shell environment variables from `org-nix-shell--direnv-dump-json'.")

;;;###autoload
(defun org-nix-shell-invalidate-cache ()
  "Clear cached nix shell environment variables.
This will force a nix shell reload on the next `org-babel-execute-src-block'."
  (interactive)
  (clrhash org-nix-shell--cache))

(defun org-nix-shell--direnv-dump-json (nix-shell-path)
  "Return environment variables as json object from NIX-SHELL-PATH.

Behind the scenes we run the command:
nix-shell <nix-shell-path> --run \"direnv dump json\"."
  (with-current-buffer (get-buffer-create "*org-nix-shell*")
    (erase-buffer)
    ;; Output from $shellHook from shell.nix can clobber clean json dump
    (let ((exit-code (call-process "nix-shell" nil t nil nix-shell-path "--run" "echo \"\n:direnv dump json:\"; direnv dump json")))
      (if (zerop exit-code)
          (let ((json-key-type 'string))
            (goto-char (point-min))
            (search-forward ":direnv dump json:")
            (forward-line)
            (json-read-object))
        (display-buffer "*org-nix-shell*")
        (user-error "Error running nix-shell")))))

(defun org-nix-shell--get-direnv (name)
  "Try to find src block with name NAME and return the nix shell environment.

Note that the results may come from the cache `org-nix-shell--cache'.
To force a full reload you may call `org-nix-shell-invalidate-cache'."
  (save-excursion
    (let ((point (org-babel-find-named-block name)))
      (if point
          (progn
            (goto-char point)
            (let* ((info (org-babel-get-src-block-info))
                   (hash (abs (sxhash info)))
                   (cached-direnv (gethash hash org-nix-shell--cache)))
              (if cached-direnv
                  cached-direnv
                (let* ((nix-shell-basename (concat "nix-shell-" name ".nix"))
                       (nix-shell-path (make-temp-file (concat
                                                        (file-name-as-directory "/tmp")
                                                        nix-shell-basename))))
                  (org-babel-tangle '(4) nix-shell-path)
                  (when-let ((direnv (org-nix-shell--direnv-dump-json nix-shell-path)))
                    (puthash hash direnv org-nix-shell--cache))))))
        (user-error "`%s' src block not found in buffer" name)))))

;; Thank you! https://github.com/purcell/envrc
(defun org-nix-shell--merge-environment (process-env direnv)
  "Make a `process-environment' value that merges PROCESS-ENV with DIRENV.
DIRENV is an alist obtained from direnv's output.
Values from PROCESS-ENV will be included, but their values will
be masked by Emacs' handling of `process-environment' if they
also appear in DIRENV."
  (append (mapcar (lambda (pair)
                    (if (cdr pair)
                        (format "%s=%s" (car pair) (cdr pair))
                      ;; Plain env name is the syntax for unsetting vars
                      (car pair)))
                  direnv)
          process-env))

;; Thank you! https://github.com/purcell/envrc
(defun org-nix-shell--apply-env (direnv)
  "Apply shell environment DIRENV."
  (setq-local process-environment (org-nix-shell--merge-environment (default-value 'process-environment) direnv))
  (let ((path (getenv "PATH"))) ;; Get PATH from the merged environment: direnv may not have changed it
    (setq-local exec-path (parse-colon-path path))))

;; Thank you! https://github.com/purcell/envrc
(defun org-nix-shell--clear-env ()
  "Remove shell environment set by `org-nix-shell--apply-env'."
  (kill-local-variable 'exec-path)
  (kill-local-variable 'process-environment))

(defun org-nix-shell--process-params (info params)
  "Return `:nix-shell' header argument from source block.

INFO and PARAMS are arguments originally from `org-babel-execute-src-block'"
  (let* ((org-babel-current-src-block-location
          (or org-babel-current-src-block-location
              (nth 5 info)
              (org-babel-where-is-src-block-head)))
         (info (if info (copy-tree info) (org-babel-get-src-block-info))))
    ;; Merge PARAMS with INFO before considering source block
    ;; evaluation since both could disagree.
    (cl-callf org-babel-merge-params (nth 2 info) params)
    (when (org-babel-check-evaluate info)
      (cl-callf org-babel-process-params (nth 2 info))
      (when-let* ((params (nth 2 info))
                  (nix-shell-cons-cell (assq :nix-shell params))
                  (nix-shell-name (cdr nix-shell-cons-cell)))
        nix-shell-name))))


;; Thank you! https://github.com/purcell/inheritenv/
(defun org-nix-shell--inheritenv-apply (func &rest args)
  "Apply FUNC such that the environment it sees will match the current value.
This is useful if FUNC creates a temp buffer, because that will
not inherit any buffer-local values of variables `exec-path' and
`process-environment'.

This function is designed for convenient use as an \"around\" advice.

ARGS is as for ORIG."
  (cl-letf* (((default-value 'process-environment) process-environment)
             ((default-value 'exec-path) exec-path))
    (apply func args)))

(defun org-nix-shell--execute-src-block (orig-fun &optional arg info params executor-type)
  "Execute src block with nix shell environment
Intended to be used as a advice around `org-babel-execute-src-block'.
ORIG-FUN, ARG, INFO, PARAMS, EXECUTOR-TYPE are the same as for
`org-babel-execute-src-block'"
  (let* ((nix-shell-name (org-nix-shell--process-params info params)))
    (if nix-shell-name
        (let* ((direnv (when nix-shell-name (org-nix-shell--get-direnv nix-shell-name)))
               (_ (when nix-shell-name (org-nix-shell--apply-env direnv)))
               (ret (if (>= emacs-major-version 28)
                        (org-nix-shell--inheritenv-apply orig-fun arg info params executor-type)
                      (org-nix-shell--inheritenv-apply orig-fun arg info params)))
               (_ (when nix-shell-name (org-nix-shell--clear-env))))
          ret)
      (if (>= emacs-major-version 28)
          (apply orig-fun arg info params executor-type)
        (apply orig-fun arg info params)))))

;;;###autoload
(define-minor-mode org-nix-shell-mode
  "Toggle `org-nix-shell-mode'."
  :global t
  (if org-nix-shell-mode
      (progn
        (org-nix-shell-invalidate-cache)
        (advice-add 'org-babel-execute-src-block :around #'org-nix-shell--execute-src-block))
    (advice-remove 'org-babel-execute-src-block #'org-nix-shell--execute-src-block)))

(provide 'org-nix-shell)
;;; org-nix-shell.el ends here
