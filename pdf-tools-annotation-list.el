;;; pdf-tools-annotation-list.el --- List content of all pdf annotations  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Ferdinand Pieper

;; Author: Ferdinand Pieper <mail@pie.tf>
;; Keywords: pdf-tools, annotations

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

;; Provides annotation notes and also extracts text from highlighted regions.

;;; Code:

(require 'pdf-tools)
(require 'cl-lib)

(defconst pdf-tools-annotation-list-ignore-types
  (list 'link)
  "List of annotation types to ignore.")

(defun pdf-tools-annotation-list-get-region (coords)
  "Attempt to get 4-entry region \(LEFT TOP RIGHT BOTTOM\) from several COORDS.
We need this to import annotations and to get marked-up text, because
annotations are referenced by its coords, but functions for these tasks
need region."
  (let ((left0 (nth 0 (car coords)))
        (top0 (nth 1 (car coords)))
        (bottom0 (nth 3 (car coords)))
        (top1 (nth 1 (car (last coords))))
        (right1 (nth 2 (car (last coords))))
        (bottom1 (nth 3 (car (last coords))))
        (n (safe-length coords)))
    ;; we try to guess the line height to move
    ;; the region away from the boundary and
    ;; avoid double lines
    (list left0
          (+ top0 (/ (- bottom0 top0) 3))
          right1
          (- bottom1 (/ (- bottom1 top1) 3)))))

(defun pdf-tools-annotation-list-create-org-pdftools-link (filename page edges id)
  "Returns a formatted org-pdftools compatible link."
  (format "[[%s:%s::%s++%s][%s]]" org-pdftools-link-prefix filename page (nth 1 edges) id))

(defun pdf-tools-annotation-list-create-list ()
  "Return list of all annotations"
  (let* ((annots (sort (pdf-annot-getannots) 'pdf-annot-compare-annotations))
         (extracted-annots (mapcar
     (lambda (annot) ;; traverse all annotations
       (let* ((page (pdf-annot-get annot 'page))
              (has-markup-edges (pdf-annot-get annot 'markup-edges))
              (edges (if has-markup-edges
                         (car (pdf-annot-get annot 'markup-edges))
                       (pdf-annot-get annot 'edges)))
              (contents (pdf-annot-get annot 'contents))
              (id (symbol-name (pdf-annot-get-id annot)))
              (type (symbol-name (pdf-annot-get-type annot)))
              (filename (buffer-name))
              (entry (list
                      :file filename
                      :page page
                      :link (pdf-tools-annotation-list-create-org-pdftools-link
                             filename page edges id)
                      :id id
                      :type type
                      :contents contents
                      :text (when has-markup-edges
                              (pdf-info-gettext
                               page
                               (pdf-tools-annotation-list-get-region
                                (pdf-annot-get annot 'markup-edges)))))))
         entry
         ))
     (cl-remove-if
      (lambda (annot) (member (pdf-annot-get-type annot) pdf-tools-annotation-list-ignore-types))
      annots)
     )))
    extracted-annots))

(defun pdf-tools-annotation-list-entry-to-org (annotation)
  "Converts ANNOTATION to org formatted text"
  (format
   "%s:\n%s%s\n"
   (plist-get annotation :link)
   (let ((text (plist-get annotation :text)))
     (if text
         (format "#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n" text)
       ""))
   (plist-get annotation :contents)))

(defun pdf-tools-annotation-list-insert-in-org (pdf)
  (let* ((pdfbuffer (find-file-noselect pdf))
        (annotations (with-current-buffer pdfbuffer
                       (pdf-tools-annotation-list-create-list))))
    (mapc (lambda (annot)
            (insert (pdf-tools-annotation-list-entry-to-org annot)))
          annotations)
    nil))

(provide 'pdf-tools-annotation-list)
;;; pdf-tools-annotation-list.el ends here
