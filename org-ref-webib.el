;;; org-ref-webib.el ---  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017 Yevgnen Koh
;;
;; Author: Yevgnen Koh <wherejoystarts@gmail.com>
;; Version: 1.0.0
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

;; arXiv
(defun org-ref-webib-arxiv-get-pdf-add-bibtex-entry (link)
  (if (string-match org-bracket-link-regexp link)
      (let ((location (match-string 1 link)))
        (if (string-match "/\\(?:abs\\|pdf\\)/\\([0-9]+\\.[0-9]+\\)" location)
            (let ((arxiv-number (match-string 1 location))
                  (default-biblio (car org-ref-default-bibliography)))
              (unwind-protect
                  (cl-letf (((symbol-function #'org-ref-pdf-p) (lambda (&rest _) nil)))
                    (arxiv-get-pdf-add-bibtex-entry arxiv-number
                                                    default-biblio
                                                    org-ref-pdf-directory))
                (find-file default-biblio)))
          (user-error "Not a arXiv page: %s." location)))))

(defun org-ref-webib-arxiv-get-pdf-add-bibtex-entry-from-safari ()
  (interactive)
  (org-ref-webib-arxiv-get-pdf-add-bibtex-entry (org-mac-safari-get-frontmost-url)))

(defun org-ref-webib-arxiv-get-pdf-add-bibtex-entry-from-firefox ()
  (interactive)
  (org-ref-webib-arxiv-get-pdf-add-bibtex-entry (org-mac-firefox-get-frontmost-url)))

;; ACL (Mostly adjusted from `org-ref-arxiv.el')
(defun org-ref-webib-acl-get-bibtex-entry (acl-number)
  (with-current-buffer
      (url-retrieve-synchronously
       (format "https://www.aclweb.org/anthology/%s.bib" acl-number)
       t)
    (goto-char url-http-end-of-headers)
    (substring-no-properties (buffer-string) (point))))

(defun org-ref-webib-acl-add-bibtex-entry (acl-number bibfile)
  (interactive
   (list (read-string "ACL: ")
         (completing-read
          "Bibfile: "
          (append (f-entries "." (lambda (f) (f-ext? f "bib")))
                  org-ref-default-bibliography))))
  (save-window-excursion
    (find-file bibfile)
    (goto-char (point-max))
    (when (not (looking-at "^")) (insert "\n"))
    (insert (org-ref-webib-acl-get-bibtex-entry acl-number))
    (org-ref-clean-bibtex-entry)
    (bibtex-beginning-of-entry)
    (let ((key (bibtex-completion-get-value "=key=" (bibtex-parse-entry))))
      (goto-char (point-max))
      (when (not (looking-at "^")) (insert "\n"))
      (save-buffer)
      key)))

(defun org-ref-webib-acl-get-pdf (acl-number pdf &optional open)
  (interactive "sACL: \nsPDF: ")
  (let ((pdf-url (format "https://www.aclweb.org/anthology/%s.pdf" acl-number)))
    (url-copy-file pdf-url pdf)
    ;; now check if we got a pdf
    (if (and (org-ref-pdf-p pdf)
             open)
        (org-open-file pdf)
      (delete-file pdf 'trash)
      (message "Error downloading arxiv ACL %s" pdf-url))))

(defun org-ref-webib-acl-get-pdf-add-bibtex-entry (link)
  (if (string-match org-bracket-link-regexp link)
      (let ((location (match-string 1 link)))
        (if (string-match "www\\.aclweb\\.org/.+?/\\([^\\./]+\\)\\(?:/|\\.bib|pdf\\)?" location)
            (let ((acl-number (match-string 1 location))
                  (default-biblio (car org-ref-default-bibliography)))
              (unwind-protect
                  (let* ((key (org-ref-webib-acl-add-bibtex-entry acl-number default-biblio))
                         (pdf-file (expand-file-name (format "%s.pdf" key) org-ref-pdf-directory)))
                    (org-ref-webib-acl-get-pdf acl-number pdf-file))
                (find-file-other-window default-biblio)))
          (user-error "Not a ACL page: %s." location)))))

(defun org-ref-webib-acl-get-pdf-add-bibtex-entry-from-safari ()
  (interactive)
  (org-ref-webib-acl-get-pdf-add-bibtex-entry (org-mac-safari-get-frontmost-url)))

(defun org-ref-webib-acl-get-pdf-add-bibtex-entry-from-firefox ()
  (interactive)
  (org-ref-webib-acl-get-pdf-add-bibtex-entry (org-mac-firefox-get-frontmost-url)))

(defun org-ref-webib-get-pdf-add-bibtex-entry-dispatcher (link)
  (cond ((string-match "arxiv\\.org" link)
         #'org-ref-webib-arxiv-get-pdf-add-bibtex-entry)
        ((string-match "aclweb\\.org" link)
         #'org-ref-webib-acl-get-pdf-add-bibtex-entry)
        (t (user-error "Unknown bibtex website: %s" link))))

(defun org-ref-webib-get-pdf-add-bibtex-entry-from-safari ()
  (interactive)
  (let ((link (org-mac-safari-get-frontmost-url)))
    (funcall (org-ref-get-pdf-add-bibtex-entry-dispatcher link) link)))

(defun org-ref-webib-get-pdf-add-bibtex-entry-from-firefox ()
  (interactive)
  (let ((link (org-mac-firefox-get-frontmost-url)))
    (funcall (org-ref-get-pdf-add-bibtex-entry-dispatcher link) link)))

(provide 'org-ref-webib)

;;; org-ref-webib.el ends here
