;;; libmpdel-directory-test.el --- Tests for libmpdel.el        -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Damien Cassou

;; Author: Jose A Ortega <jao@gnu.org>
;; Url: https://gitlab.petton.fr/mpdel/mpdel
;; Package-requires: ((emacs "25.1"))
;; Version: 1.1.2

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

;; Tests for libmpdel-directory.el.

;;; Code:

(require 'ert)
(require 'ert-x)

(require 'cl-lib)

(require 'libmpdel-directory)


;; Data structure

(ert-deftest libmpdel-directory-test-directory ()
  (let* ((path "somewhere/around")
         (name "here")
         (nameless (libmpdel--directory-create :path path))
         (dir (libmpdel--directory-create :path path :name name)))
    (should (equal nil (libmpdel--directory-name nameless)))
    (should (equal path (libmpdel--directory-path nameless)))
    (should (equal name (libmpdel--directory-name dir)))
    (should (equal path (libmpdel--directory-path dir)))))

(ert-deftest libmpdel-directory-test-parent ()
  (let* ((path "somewhere/around")
         (name "here")
         (nameless (libmpdel--directory-create :path path))
         (dir (libmpdel--directory-create :path path :name name))
         (pdir (libmpdel--directory-create :path "somewhere"))
         (ppdir (libmpdel-entity-parent (libmpdel-entity-parent dir))))
    (should (equal pdir (libmpdel-entity-parent nameless)))
    (should (equal pdir (libmpdel-entity-parent dir)))
    (should (equal 'directories ppdir))
    (should (equal nil (libmpdel-entity-parent ppdir)))
    (should (equal ppdir (libmpdel-entity-parent pdir)))
    (should (equal nil (libmpdel-entity-parent (libmpdel-entity-parent pdir))))))

(ert-deftest libmpdel-directory-test-entity-to-criteria ()
  (let* ((path "somewhere/around")
         (name "here")
         (nameless (libmpdel--directory-create :path path))
         (dir (libmpdel--directory-create :path path :name name)))
    (should (equal "lsinfo \"somewhere/around\""
                   (libmpdel-entity-to-criteria nameless)))
    (should (equal "lsinfo \"somewhere/around\""
                   (libmpdel-entity-to-criteria dir)))))


(provide 'libmpdel-directory-test)
;;; libmpdel-directory-test.el ends here

;; Local Variables:
;; nameless-current-name: "libmpdel-directory-test"
;; End:
