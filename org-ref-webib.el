;;; org-ref-webib.el ---  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017 Yevgnen Koh
;;
;; Author: Yevgnen Koh <wherejoystarts@gmail.com>
;; Version: 1.1.0
;; Package-Requires: ((emacs "25.1") (org-ref "1.1.1"))
;; Keywords:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;
;;
;; See documentation on https://github.com/Yevgnen/org-ref-webib.el.

;;; Code:

(require 'org-mac-link)
(require 'org-ref)
(require 'org-ref-arxiv)

;; Common utilities.

(defvar org-ref-webib-sites
  '((acl . (:site "aclweb\\.org"
                  :key "www\\.aclweb\\.org/.+?/\\([^\\./]+\\)\\(?:/|\\.bib|pdf\\)?"
                  :bibtex "https://www.aclweb.org/anthology/%s.bib"
                  :pdf "https://www.aclweb.org/anthology/%s.pdf"))
    (arxiv . (:site "arxiv\\.org"
                    :key "/\\(?:abs\\|pdf\\)/\\([0-9]+\\.[0-9]+\\)"
                    :bibtex org-ref-webib-arxiv-download-bibtex
                    :pdf "https://arxiv.org/pdf/%s.pdf"))
    (nips . (:site "nips\\.cc"
                   :key "papers\\.nips\\.cc/paper/\\([^/]+\\)\\(?:/bibtex|\\.pdf\\)?"
                   :bibtex "https://papers.nips.cc/paper/%s/bibtex"
                   :pdf "https://papers.nips.cc/paper/%s.pdf"))))

(defun org-ref-webib-sites ()
  (mapcar #'car org-ref-webib-sites))

(defun org-ref-webib-get-prop (site key prop)
  (let ((obj (plist-get (alist-get site org-ref-webib-sites) prop)))
    (if (stringp obj)
        (format obj key)
      obj)))

(defun org-ref-webib-extract-key (site text)
  (let ((obj (plist-get (alist-get site org-ref-webib-sites) :key)))
    (if (stringp obj)
        (if (string-match obj text)
            (match-string 1 text))
      (funcall obj text))))

(defun org-ref-webib-prompt-for-key (old-key)
  (read-string "Bibtex Key: " old-key))

(defun org-ref-webib-entry-existed-p (key)
  (save-excursion
    (bibtex-search-entry key)))

(defun org-ref-webib-write-entry (entry bibfile &optional prompt)
  (let ((key nil))
    (save-window-excursion
      ;; Goto end of file.
      (find-file bibfile)
      (goto-char (point-max))

      ;; Insert empty line before.
      (when (not (looking-at "^"))
        (insert "\n\n"))

      ;; Insert and clean entry.
      (insert entry)
      (org-ref-clean-bibtex-entry)

      ;; Update key when necessary.
      (bibtex-beginning-of-entry)
      (re-search-forward bibtex-entry-maybe-empty-head)
      (when (match-beginning bibtex-key-in-head)
        (setq key (delete-and-extract-region
                   (match-beginning bibtex-key-in-head)
                   (match-end bibtex-key-in-head)))

        ;; Prompt for new key when duplicated or `prompt' is t.
        (if (or (org-ref-webib-entry-existed-p key)
                prompt)
            (setq key (org-ref-webib-prompt-for-key key)))
        (if (and key (not (string-empty-p key)))
            (insert key)
          (bibtex-kill-entry)
          (save-buffer)
          (user-error "Key is not given."))

        ;; Insert empty line after.
        (goto-char (point-max))
        (when (not (looking-at "^"))
          (insert "\n\n")))
      (save-buffer)
      (message "Bibtex: %s saved to %s." key bibfile))
    key))

(defun org-ref-webib-download-pdf (url pdf &optional open callback)
  (if (file-exists-p pdf)
      (user-error "PDF file existed: %s" pdf))
  (url-retrieve
   url
   `(lambda (_data)
      (let ((coding-system-for-write 'binary))
        (delete-region (point-min) url-http-end-of-headers)
        (while (looking-at "^$")
          (delete-char 1))
        (write-region (point-min) (point-max) ,pdf)
        (if (org-ref-pdf-p ,pdf)
            (progn
              (if ,open
                  (org-open-file ,pdf))
              (message "PDF saved to: %s" ,pdf))
          (delete-file ,pdf 'trash)
          (user-error "Failed to download pdf: %s" ,url)))
      (if ,callback
          (funcall ,callback)))))

(defun org-ref-webib-add-pdf (site key newkey &optional open pdf-directory callback)
  (let ((pdf-url (org-ref-webib-get-prop site key :pdf))
        (pdf-directory (or pdf-directory org-ref-pdf-directory))
        (pdf-file (expand-file-name (format "%s.pdf" newkey) pdf-directory)))
    (org-ref-webib-download-pdf pdf-url pdf-file open callback)))

(defun org-ref-webib-add-bibtex (site key &optional bibfile pdf-directory)
  (let ((buf (current-buffer))
        (bibfile (or bibfile (car org-ref-default-bibliography)))
        (pdf-directory (or pdf-directory org-ref-pdf-directory)))

    (let ((obj (org-ref-webib-get-prop site key :bibtex)))
      (if (stringp obj)
          (url-retrieve
           obj
           `(lambda (data)
              (goto-char url-http-end-of-headers)
              (let* ((entry (substring-no-properties (buffer-string) (point)))

                     (bibkey (org-ref-webib-write-entry entry ,bibfile nil)))
                (org-ref-webib-add-pdf
                 ',site ,key bibkey nil ,pdf-directory))))
        (let ((bibkey (org-ref-webib-write-entry (funcall obj key) bibfile nil)))
          (org-ref-webib-add-pdf
           site key bibkey nil pdf-directory))))))

;; arXiv
(defun org-ref-webib-arxiv-download-bibtex (key)
  (arxiv-get-bibtex-entry-via-arxiv-api key))

;; Browser supports.
(defun org-ref-webib-org-link-builder ()
  (dolist (site (org-ref-webib-sites))
    (defalias (intern (format "org-ref-webib-%s-add" site))
      (function
       (lambda (link)
         (if (string-match org-bracket-link-regexp link)
             (let* ((location (match-string 1 link))
                    (key (org-ref-webib-extract-key site location)))
               (unless key
                 (user-error "Failed to extract key: %s" link))
               (org-ref-webib-add-bibtex site key))))))))

(org-ref-webib-org-link-builder)

(defun org-ref-webib-browser-util-builder ()
  (dolist (browser '(safari firefox chrome))
    (dolist (site (org-ref-webib-sites))
      (defalias (intern (format "org-ref-webib-%s-add-from-%s" site browser))
        (function
         (lambda nil
           (interactive)
           (funcall (intern (format "org-ref-webib-%s-add" site))
                    (funcall (intern (format "org-mac-%s-get-frontmost-url" browser))))))))

    (defalias (intern (format "org-ref-webib-add-from-%s" browser))
      (function
       (lambda nil
         (interactive)
         (let ((link (org-mac-safari-get-frontmost-url)))
           (funcall (org-ref-webib-add-dispatcher link) link)))))))

(org-ref-webib-browser-util-builder)

(defun org-ref-webib-add-dispatcher (link)
  (let ((site (caar (cl-remove-if-not (lambda (x)
                                        (string-match (plist-get (cdr x) :site) link))
                                      org-ref-webib-sites))))
    (unless site
      (user-error "Unsupported link: %s" link))
    (function (intern (format "org-ref-webib-%s-add" site)))))

(provide 'org-ref-webib)

;;; org-ref-webib.el ends here
