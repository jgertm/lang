;;; lang-mode.el --- Lang major mode -*- lexical-binding: t; -*-

;; Copyright (C) 2001  Free Software Foundation, Inc.

;; Author: Tim Jäger
;; Keywords: extensions

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;

;;; Code:

(require 'rx)

(defvar lang-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `lang-mode'.")

(defconst lang-mode-syntax-table
  (let ((table (copy-syntax-table emacs-lisp-mode-syntax-table)))
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    table)
  "Syntax table for `lang-mode'.")

(defconst lang-font-lock-keywords
  `(;; strings
    (,(rx ?\" (* anything) ?\") . font-lock-string-face)
    ;; numbers
    (,(rx (+ digit)) . font-lock-constant-face)
    ;; atomic constants
    (,(rx symbol-start
          (group (| "nil" "true" "false"))
          symbol-end)
     (1 font-lock-constant-face))
    ;; keywords
    (,(rx symbol-start
          (group ":"
                 (+ (| word ?\. ?\-))))
     (1 font-lock-constant-face))
    ;; named types
    (,(rx upper (+ word)) . font-lock-type-face)

    ;; module definition
    (,(rx ?\(
          (group "defmodule")
          (+ space)
          (group (+ (| word ?\. ?\-))))
     (1 font-lock-keyword-face)
     (2 font-lock-type-face nil t))

    ;; type definition
    (,(rx ?\(
          (group "deftype")
          (+ space)
          (group upper (+ word)))
     (1 font-lock-keyword-face)
     (2 font-lock-type-face nil t))

    ;; function definition
    (,(rx ?\(
          (group "defn")
          (+ space)
          (group (+ (| word punctuation))))
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face nil t))

    ;; constant definition
    (,(rx ?\(
          (group "def")
          (+ space)
          (group (+ (| word punctuation))))
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face nil t))

    ;; builtin operations
    (,(rx ?\(
          (group (| "if" "match" "fn")))
     (1 font-lock-keyword-face)))
  "Keyword highlighting specification for `lang-mode'.")

(defconst lang-imenu-generic-expression
  `(("Modules" ,(rx "(defmodule" (+ space) (group (+ (| word ?\. ?\-)))) 1)
    ("Types"   ,(rx "(deftype" (+ space) (group upper (+ word))) 1)
    ("Functions" ,(rx "(defn" (+ space) (group (+ (| word ?\-)))) 1)
    ("Constants" ,(rx "(def" (+ space) (group (+ (| word ?\-)))) 1)))

 (defun lang-indent-line ()
   "Indent current line of Lang code."
   (lisp-indent-line))

 ;;;###autoload
(define-derived-mode lang-mode prog-mode "Lang"
  "A major mode for editing Lang files."
  :group 'lisp
  :syntax-table lang-mode-syntax-table
  (setq-local comment-start ";")
  (setq-local comment-start-skip ";+\\s-*")
  (setq-local font-lock-defaults '(lang-font-lock-keywords))
  (setq-local indent-line-function #'lang-indent-line)
  (setq-local imenu-generic-expression lang-imenu-generic-expression)
  )

(provide 'lang-mode)
;;; lang-mode.el ends here
