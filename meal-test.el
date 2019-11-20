;;; meal-test.el --- Test meal -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Gong Qijian <gongqijian@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'meal)

(when noninteractive
  (transient-mark-mode))

(defvar meal-test-message-list '())
(add-to-list 'load-path (expand-file-name "features/"))

(ert-deftest meal-test-through-fn ()
  (should
   (equal
    (macroexpand-all '(meal--through-fn-featurep (and a (or b c))))
    '(and (featurep (quote a))
          (or (featurep (quote b))
              (featurep (quote c)))))))

(ert-deftest meal-test-load ()
  (with-eval-after-load 'a                (push "after a" meal-test-message-list))
  (with-eval-after-load 'b                (push "after b" meal-test-message-list))
  (with-eval-after-load 'c                (push "after c" meal-test-message-list))
  (with-eval-after-load '(and a b)        (push "after (and a b)" meal-test-message-list))
  (with-eval-after-load '(and a c)        (push "after (and a c)" meal-test-message-list))
  (with-eval-after-load '(or b c)         (push "after (or b c)"  meal-test-message-list))
  (with-eval-after-load '(and a (or b c)) (push "after (and a (or b c))"  meal-test-message-list))

  (push "load a" meal-test-message-list) (require 'a)
  (push "load b" meal-test-message-list) (require 'b)
  (push "load c" meal-test-message-list) (require 'c)

  (should (equal (reverse meal-test-message-list)
                 '("load a"
                   "after a"
                   "load b"
                   "after b"
                   "after (and a b)"
                   "after (or b c)"
                   "after (and a (or b c))"
                   "load c"
                   "after c"
                   "after (and a c)"))))

(provide 'meal-test)

;;; meal-test.el ends here
