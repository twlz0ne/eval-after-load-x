;;; meal.el --- Multi-file support for eval-after-load -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2019/11/20
;; Version: 0.2.0
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/twlz0ne/meal.el
;; Keywords: tool

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Multi-file support for eval-after-load

;;; Installation

;; Clone this repository and add the following to your `.emacs`:
;; 
;; ```
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/meal"))
;; (require 'meal)
;; ```

;;; Usage

;; ```
;; (with-eval-after-load '(and a (or b c))
;;   ...)
;; ```

;; See README.md for more information.

;;; Change Log:

;;  0.1.0  2019/11/20  Initial version.
;;  0.2.0  2019/11/21  Remove dependency on dash-el, drop support for Emacs 24.

;;; Code:

(require 'cl-seq)

(defvar meal-alist '()
  "An alist of functions to be evalled when particular files are loaded.
Each element looks like (SYMBOL LOAD-FUNC)")

(defmacro meal--through-fn (fn form)
  (declare (debug (form symbolp body)))
  (let ((op (pop form)))
    `(,op ,@(mapcar
             (lambda (it)
               (if (listp it)
                   `(meal--through-fn ,fn ,it)
                 `(,fn ',it)))
             form))))

(defmacro meal--through-fn-featurep (form)
  `(meal--through-fn featurep ,form))

(defalias 'meal--flatten
  (if (fboundp 'flatten-tree) 'flatten-tree
    ;; From Emacs 27 nightly:
    ;; https://emba.gnu.org/emacs/emacs/commit/36b05dc84247db1391a423df94e4b9a478e29dc5
    (lambda (tree)
      "Return a \"flattened\" copy of TREE.
In other words, return a list of the non-nil terminal nodes, or
leaves, of the tree of cons cells rooted at TREE.  Leaves in the
returned list are in the same order as in TREE.

\(flatten-tree \\='(1 (2 . 3) nil (4 5 (6)) 7))
=> (1 2 3 4 5 6 7)"
      (let (elems)
        (while (consp tree)
          (let ((elem (pop tree)))
            (while (consp elem)
              (push (cdr elem) tree)
              (setq elem (car elem)))
            (if elem (push elem elems))))
        (if tree (push tree elems))
        (nreverse elems)))))

(defun meal (fn expr form)
  (if (listp expr)
      (let* ((files (cl-remove-if (lambda (it) (memq it '(and or))) (meal--flatten expr)))
             (cond (macroexpand-all `(meal--through-fn-featurep ,expr)))
             (fun-sym (make-symbol "meal-helper"))
             (fun-def (lambda () (funcall form)))
             (fun-wrapper
              (lambda ()
                (when (eval `,cond)
                  (let ((elt (assoc fun-sym meal-alist)))
                    (when elt
                      (setq meal-alist (delq elt meal-alist))
                      (funcall (cdr elt))))))))
        (push (cons fun-sym fun-def) meal-alist)
        (mapc (lambda (file)
                (funcall fn file fun-wrapper))
              files))
    (funcall fn expr form)))

(advice-add 'eval-after-load :around #'meal)

(provide 'meal)

;;; meal.el ends here
