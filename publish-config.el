;;; publish-config.el --- The configuration for building my website with org-publish.  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Aidan Hall

;; Author: Aidan Hall <aidan.hall202@gmail.com>
;; Keywords: org

;;; Commentary:

;; By setting this file as the `org-export-async-init-file' in this directory,
;; I can automatically apply some arbitrary configuration for the whole website,
;; including defining custom functions.
;;
;; This setup also keeps any setting changes and definitions for the website out
;; of my main Emacs environment, which is nice.

;;; Code:

;; Paths

(defvar website-dir "~/Documents/websites/orgsite/"
  "The base source directory for the website.")

(defvar website-export-dir "~/Documents/websites/orgsite-html"
  "The base export directory for the website.")

(setq org-html-link-home
      ;; "https://argletrough.neocities.org"
      "https://aidanhall.gitlab.io"
      )

;; Dependencies

(require 'package)
(package-initialize)
(require 'use-package)
(use-package denote
  :ensure t)
(setq denote-directory (file-name-concat website-dir "blog/"))

(use-package htmlize
  :ensure t)

;; Content configuration

(setq
 org-list-allow-alphabetical t
 )

;; Babel Configuration

(with-eval-after-load 'ob
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t))))

;; Export configuration
(setq
 org-export-backends '(html)
 org-export-default-language "en-gb"
 org-export-global-macros '(("summary" . "#+html: <summary>$1$2$3$4$5$6$7$8$9</summary>"))
 org-export-with-section-numbers nil
 org-export-with-toc nil
 )

;; HTML Configuration
(setq
 org-html-doctype "html5"
 org-html-html5-fancy t
 org-html-head-include-default-style nil
 org-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"/files/stylesheet.css\" />"
 org-html-divs '((preamble "div" "preamble")
                 (content "main" "content")
                 (postamble "footer" "postamble"))
 org-html-link-up "./"
 org-html-home/up-format "
<nav>
<ul id=\"navigation\">
<li><a href=\"%2$s/\" >&#127968; Home</a></li>
<li><a href=\"%2$s/blog\" >&#128212; Blog</a></li>
<li><a href=\"%1$s\" >&#12106; Subdir Root</a></li>
</ul></nav>"
 org-html-postamble "Last modified: %C.
Created with %c.
<a href=\"#content\">🔝</a>"
 org-html-footnotes-section "<section id=\"footnotes\">
<h2 class=\"footnotes\">%s: </h2>
<div id=\"text-footnotes\">
%s
</div>
</div>"
 org-html-format-drawer-function
 (apply-partially #'format "<details>\n<summary>%s</summary>\n%s</details>\n")
 )

;; Publishing Configuration

(defun blog-sitemap-entry (entry _style project)
  "Generate a sitemap entry for ENTRY, in PROJECT."
  (cond ((not (directory-name-p entry))
	 (format "%s [[file:%s][%s]] %s"
                 (format-time-string "%F %R" (org-publish-find-date entry project))
		 entry
		 (org-publish-find-title entry project)
                 (if-let ((description
                           (org-publish-find-property entry :description project 'html)))
                     (format " - /%s/" description)
                   (if-let ((subtitle
                             (org-publish-find-property entry :subtitle project 'html)))
                       (format " - /%s/" (string-join subtitle " "))
                     ""))))
	((eq style 'tree)
	 ;; Return only last subdir.
	 (file-name-nondirectory (directory-file-name entry)))
        (t entry)))

(defun blog-publish-sitemap (title list)
  "Sitemap for a blog, with given TITLE and LIST of posts."
  (concat "#+title: " title "\n"
          "#+date: [" (format-time-string "%F %R" (current-time)) "]\n\n"
          (org-list-to-org list)))

(setq
 org-publish-project-alist
 `(("files"
    :base-directory ,(file-name-concat website-dir "files")
    :base-extension any
    :publishing-directory ,(file-name-concat website-export-dir "files")
    :publishing-function org-publish-attachment
    :recursive t)
   ("toplevel"
    :base-directory ,website-dir
    :publishing-directory ,website-export-dir)
   ("blog"
    :base-directory ,denote-directory
    :publishing-directory ,(file-name-concat website-export-dir "blog")
    :auto-sitemap t
    :sitemap-title "Blog"
    :sitemap-sort-files anti-chronologically
    :sitemap-filename "index.org"
    :sitemap-format-entry ,#'blog-sitemap-entry
    :sitemap-function ,#'blog-publish-sitemap
    :html-postamble ,(concat "Posted: %d. " org-html-postamble)
    :recursive t)
   ("writings"
    :base-directory ,(file-name-concat website-dir "writings")
    :publishing-directory ,(file-name-concat website-export-dir "writings")
    :recursive t
    :auto-sitemap t
    :sitemap-filename "index.org")
   ("Aidan Hall"
    :components ("files" "toplevel" "blog" "writings"))))
(provide 'publish-config)
;;; publish-config.el ends here
