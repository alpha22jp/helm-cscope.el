;;; helm-cscope.el --- Helm interface for xcscope.el. -*- lexical-binding: t -*-

;; Copyright (C) 2015 alpha22jp <alpha22jp@gmail.com>

;; Author: alpha22jp <alpha22jp@gmail.com>
;; URL: https://github.com/alpha22jp/helm-cscope.el
;; Keywords: cscope
;; Version: 0.1.0
;; Package-Requires: ((xcscope "1.0") (helm "1.5.6") (cl-lib "0.5"))

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

;;; Commentary:

;;; Code:

(require 'helm)
(require 'xcscope)

(defgroup helm-cscope nil
  "cscope for helm"
  :group 'helm)

(defface helm-cscope-file-face
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight file name in the *helm-cscope* buffer."
  :group 'helm-cscope)

(defface helm-cscope-function-face
  '((t :inherit font-lock-type-face))
  "Face used to highlight function name in the *helm-cscope* buffer."
  :group 'helm-cscope)

(defface helm-cscope-lineno-face
  '((t :inherit font-lock-doc-face))
  "Face used to highlight line number in the *helm-cscope* buffer."
  :group 'helm-cscope)

(defun helm-cscope--search (database search-type-arg)
  (let (cmd-args database-file search-dir)

    (setq search-dir (cscope-search-directory-hierarchy database))
    (setq database-file
          (if (file-regular-p database) database
            (concat search-dir cscope-database-file)))
    (setq cmd-args (list "-L" search-type-arg (concat helm-pattern ".*")))

    ;; The database file and the directory containing the database file
    ;; must both be writable.
    (if (or (not (file-writable-p database-file))
            (not (file-writable-p (file-name-directory database-file)))
            cscope-option-do-not-update-database)
        (push "-d" cmd-args))

    (setq default-directory search-dir)
    (push cscope-program cmd-args)
    (apply 'start-process (concat "helm-cscope" search-type-arg) nil cmd-args)))

(defun helm-cscope--fuzzy-goto-line (text line-number)
  (let ((fuzzy-search-text-regexp
         (mapconcat 'regexp-quote
                    (split-string text "[ \f\t\n\r\v]+\\|\\b" t) "\\s-*"))
        old-point new-point forward-point backward-point line-end line-length)

    ;; this is recommended instead of (goto-line line-number)
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line (1- line-number)))

    (setq old-point (point))

    ;; Calculate the length of the line specified by cscope.
    (end-of-line)
    (setq line-end (point))
    (goto-char old-point)
    (setq line-length (- line-end old-point))

    ;; Search forward and backward for the pattern.
    (setq forward-point (re-search-forward
                         fuzzy-search-text-regexp
                         (+ old-point
                            cscope-fuzzy-search-range) t))
    (goto-char old-point)
    (setq backward-point (re-search-backward
                          fuzzy-search-text-regexp
                          (- old-point
                             cscope-fuzzy-search-range) t))
    (if forward-point
        (progn
          (if backward-point
              (setq new-point
                    (if (<= (- (- forward-point line-length)
                               old-point)
                            (- old-point backward-point))
                        forward-point
                      backward-point))
            (setq new-point forward-point)))
      (if backward-point
          (setq new-point backward-point)
        (setq new-point old-point)))
    (goto-char new-point)
    (beginning-of-line)))

(defun helm-cscope--open-file (dir line &optional persistent)
  (when (string-match "\\`\\([^ ]+\\) \\([^ ]+\\) \\([0-9]+\\) \\(.*\\)" line)
    (let ((file (concat dir (match-string 1 line)))
          (line-number (string-to-number (match-string 3 line)))
          (text (match-string 4 line)))
      (unless persistent (ring-insert cscope-marker-ring (point-marker)))
      (find-file file)
      (helm-cscope--fuzzy-goto-line text line-number)
      (if persistent (helm-highlight-current-line)))))

(defun helm-cscope--transform (line)
  (when (string-match "\\`\\([^ ]+\\) \\([^ ]+\\) \\([0-9]+\\) \\(.*\\)" line)
    (format "%s: %s(%s) %s"
            (propertize (match-string 1 line) 'face 'helm-cscope-file-face)
            (propertize (match-string 2 line) 'face 'helm-cscope-function-face)
            (propertize (match-string 3 line) 'face 'helm-cscope-lineno-face)
            (match-string 4 line))))

(defun helm-cscope--make-source (dir arg)
  (helm-build-async-source dir
    :candidates-process (lambda () (helm-cscope--search dir arg))
    :real-to-display 'helm-cscope--transform
    :action (lambda (line) (helm-cscope--open-file dir line))
    :persistent-action (lambda (line) (helm-cscope--open-file dir line t))))

(defalias 'helm-cscope-pop-mark 'cscope-pop-mark)

(defun helm-cscope--find-common (arg)
  (let ((cur-dir (cscope-search-directory-hierarchy
                  (file-name-directory (buffer-file-name))))
        (search-dir-list
         (cl-remove-if-not
          (lambda (e) (and (listp e) (stringp (car e))))
          (cscope-find-info (file-name-directory (buffer-file-name))))))
    (unless (string= cur-dir (car (car search-dir-list)))
      (push (list cur-dir) search-dir-list))
    (helm :sources
          (mapcar (lambda (e) (helm-cscope--make-source
                               (cscope-search-directory-hierarchy (car e)) arg))
                  search-dir-list)
          :input (cscope-extract-symbol-at-cursor nil nil)
          :buffer "*Helm cscope*")))

;;;###autoload
(defun helm-cscope-find-symbol ()
  (interactive)
  (helm-cscope--find-common "-0"))

;;;###autoload
(defun helm-cscope-find-global-definition ()
  (interactive)
  (helm-cscope--find-common "-1"))

;;;###autoload
(defun helm-cscope-find-called-function ()
  (interactive)
  (helm-cscope--find-common "-2"))

;;;###autoload
(defun helm-cscope-find-calling-this-funtcion ()
  (interactive)
  (helm-cscope--find-common "-3"))

(defvar helm-cscope-mode-name " Helm cscope")
(defvar helm-cscope-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode helm-cscope-mode ()
  "Enable for helm-cscope"
  :group      'helm-cscope
  :init-value nil
  :global     nil
  :keymap     helm-cscope-mode-map
  :lighter    helm-cscope-mode-name
  (if helm-cscope-mode
      (run-hooks 'helm-cscope-mode-hook)))

(provide 'helm-cscope)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-cscope.el ends here
