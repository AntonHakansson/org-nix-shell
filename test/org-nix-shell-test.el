;;; org-nix-shell-test.el --- Unit test for org-nix-shell -*- lexical-binding: t -*-

;; Copyright (C) 2024 Anton Hakansson

;; Maintainer: Anton Hakansson <anton@hakanssn.com>
;; URL: https://github.com/AntonHakansson/

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

;;; Code:
(require 'ert)
(require 'org)
(require 'ob-shell)
(require 'org-nix-shell)

;; Test should run non-interactively.
(setq org-confirm-babel-evaluate nil)
(setq org-babel-load-languages
 (mapcar (lambda (e) (cons e t))
         '(emacs-lisp shell)))

(defconst org-nix-shell-hello-shell-preamble "
#+name: nix-shell
#+begin_src nix
  { pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell { buildInputs = [ pkgs.hello ]; }
#+end_src")

(defmacro org-test-with-temp-text (text &rest body)
  "Run BODY in a temporary buffer with Org mode as the active mode holding TEXT.
If the string \"<point>\" appears in TEXT then remove it and
place the point there before running BODY, otherwise place the
point at the beginning of the inserted text."
  (declare (indent 1) (debug t))
  `(let ((inside-text (if (stringp ,text) ,text (eval ,text)))
	     (org-mode-hook nil))
     (with-temp-buffer
       (org-mode)
       (let ((point (string-match "<point>" inside-text)))
	     (if point
	         (progn
	           (insert (replace-match "" nil nil inside-text))
	           (goto-char (1+ (match-beginning 0))))
	       (insert inside-text)
	       (goto-char (point-min))))
       (write-file (make-temp-file "org-test"))
       (font-lock-ensure (point-min) (point-max))
       ,@body)))

(ert-deftest org-nix-shell-test--noop ()
  (org-test-with-temp-text (concat org-nix-shell-hello-shell-preamble "
#+begin_src sh
<point>echo \"org-nix-shell should not disrupt normal workflow\"
#+end_src")
    (org-nix-shell-mode +1)
    (should (org-babel-execute-src-block))
    (goto-char (should (org-babel-where-is-src-block-result)))
    (should (org-babel-read-result))))

(ert-deftest org-nix-shell-test--basic-shell ()
  (org-test-with-temp-text (concat org-nix-shell-hello-shell-preamble "
#+begin_src sh :nix-shell \"nix-shell\"
<point>hello
#+end_src")
    (org-nix-shell-mode +1)
    (should (org-babel-execute-src-block))
    (goto-char (should (org-babel-where-is-src-block-result)))
    (should (equal (org-babel-read-result) '(("Hello" "world!"))))))

(ert-deftest org-nix-shell-test--invalid-nix-shell ()
  (org-test-with-temp-text "
#+name: nix-shell
#+begin_src nix
  { pkgs ? import <nixpkgs> {} }:
  pkgs.mkShbuildInputs = [ pkgs.hello ]; }
#+end_src

#+begin_src sh :nix-shell \"nix-shell\"
  <point>hello
#+end_src"
    (org-nix-shell-mode +1)
    (should-error (org-babel-execute-src-block))))

(ert-deftest org-nix-shell-test--missing-shell ()
  (org-test-with-temp-text "
#+begin_src sh :nix-shell \"nix-shell\"
  <point>hello
#+end_src"
    (org-nix-shell-mode +1)
    (should-error (org-babel-execute-src-block))))

(ert-deftest org-nix-shell-test--export-demo ()
  (with-current-buffer (find-file "../demo.org")
    (org-nix-shell-mode +1)
    (should (org-export-as 'html))))

(provide 'org-nix-shell-test.el)
;;; org-nix-shell-test.el ends here
