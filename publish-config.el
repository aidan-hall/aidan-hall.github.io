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
(require 'org)
(require 'ox-html)
(require 'ox-publish)

;; Paths
(defvar website-dir default-directory
  "The base source directory for the website.")

(defun source-dir (&rest subdirs)
  (apply 'file-name-concat website-dir subdirs))

(defvar website-export-dir (file-name-concat website-dir
                                             "public")
  "The base export directory for the website.")

(defun export-dir (&rest subdirs)
  (apply 'file-name-concat website-export-dir subdirs))

(setq org-html-link-home
      "https://aidanhall.xyz"
      ;; "http://localhost:8000"
      )

(defvar blog-directory (source-dir "blog/"))

(defun published-file-url (filename project)
  "Convert a FILENAME relative to the root of PROJECT to its published URL."
  (file-name-concat org-html-link-home
                    (file-relative-name
                     (file-name-concat
                      (org-publish-property :base-directory project)
                      (if (string= "org" (file-name-extension filename))
                         (file-name-with-extension filename "html")
                       filename))
                     website-dir)))

;; Content configuration

(setq
 org-list-allow-alphabetical t
 user-full-name "Aidan Hall")
(defconst website-title user-full-name)

;; Babel Configuration

(with-eval-after-load 'ob
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t))))

;; Export configuration
(setq
 org-export-backends '(html)
 org-export-global-macros '(("summary" . "#+html: <summary>$1$2$3$4$5$6$7$8$9</summary>"))
 org-export-with-section-numbers nil
 org-export-with-toc nil
 org-export-with-title nil
 org-export-with-smart-quotes t)

;; HTML Configuration
(setq
 org-html-doctype "html5"
 org-html-htmlize-output-type 'css
 org-html-html5-fancy t
 org-html-container-element "section"
 org-html-head-include-default-style nil
 org-html-metadata-timestamp-format "%-d %b %Y, %R"
 org-html-head "
<link href=\"/stylesheet.css\" rel=\"stylesheet\" />
<link href=\"/fonts/computer-modern/serif.css\" rel=\"stylesheet\" />
<link href=\"/fonts/computer-modern/CMUSerif-Roman.woff2\" rel=\"preload\" as=\"font\" type=\"font/woff2\" crossorigin />
<link href=\"/fonts/computer-modern/CMUSerif-Bold.woff2\" rel=\"preload\" as=\"font\" type=\"font/woff2\" crossorigin />
<link href=\"/fonts/computer-modern/CMUSerif-Italic.woff2\" rel=\"preload\" as=\"font\" type=\"font/woff2\" crossorigin />
<link href=\"/fonts/computer-modern/CMUSerif-BoldItalic.woff2\" rel=\"preload\" as=\"font\" type=\"font/woff2\" crossorigin />
<link href=\"/fonts/computer-modern/typewriter.css\" rel=\"stylesheet\" />
<link href=\"/fonts/computer-modern/CMUTypewriter-Regular.woff2\" rel=\"preload\" as=\"font\" type=\"font/woff2\" crossorigin />"
 org-html-divs '((preamble "header" "preamble")
                 (content "main" "content")
                 (postamble "footer" "postamble"))
 org-html-link-up "./"
 org-html-home/up-format "
<nav>
<div id=\"navigation\" class=\"float-bubble\">
<a href=\"/\" >🏡 Home</a>
<a href=\"/blog\" >📔 Blog</a>
<a href=\"/wiki\" >🧠 Wiki</a>
<a href=\"%1$s\" >📇 Index</a>
</div>
</nav>"
 org-html-preamble "<h1 class=\"title\">%t</h1>
<p class=\"subtitle\">%s</p>
<p class=\"blogdate\">%d</p>"
 ;; TODO: Make this a function so we can include e.g. publishing date conditionally.
 org-html-postamble "Copyright © 2021-2025 Aidan Hall. Last modified: %C. <a href=\"#content\">🔝</a>"
 org-html-footnotes-section "<section id=\"footnotes\" class=\"float-bubble\">
<h2 class=\"footnotes\">%s: </h2>
<div id=\"text-footnotes\">
%s
</div>
</div>"
 org-html-format-drawer-function
 (apply-partially #'format "<details>\n<summary>%s</summary>\n%s</details>\n"))

;; Publishing Configuration

(defun blog-sitemap-entry (entry _style project)
  "Generate a sitemap entry for ENTRY, in PROJECT."
  (cond ((not (directory-name-p entry))
	 (format "%s :: [[file:%s][%s]]"
                 (format-time-string "%F %R" (org-publish-find-date entry project))
		 entry
		 (org-publish-find-title entry project)))
	((eq style 'tree)
	 ;; Return only last subdir.
	 (file-name-nondirectory (directory-file-name entry)))
        (t entry)))

(defun blog-publish-sitemap (title list)
  "Sitemap for a blog, with given TITLE and LIST of posts."
  (concat "#+title: " title "\n"
          "#+date: [" (format-time-string "%F %R" (current-time)) "]\n\n"
          "[[file:atom.xml][@@html:<img src=\"/files/pics/feed-icon.webp\"/>@@ Feed]]\n\n"
          (org-list-to-org list
                           '(:dtstart "@@html:<span class=\"feeddate\">"
                                      :dtend " </span>@@ "))))

(defconst rfc3339-utc-format "%FT%TZ")

(defun atom-timestamp (&optional time)
  "Produce a timestamp suitable for Atom from TIME."
  (format-time-string rfc3339-utc-format time))

(defun blog-feed-title (subdir)
  "Format a title for a feed at SUBDIR on the website."
  (concat website-title " - " (capitalize subdir)))

(defun blog-atom-sitemap (title list)
  "Generate an Atom 1.0 sitemap, with the given TITLE and LIST of posts."
  (concat "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
          "<feed xmlns=\"http://www.w3.org/2005/Atom\">"
          ;; "<link href=\"" org-html-link-home "\"/>"
          "<author><name>" user-full-name "</name></author>"
          "<id>" org-html-link-home "/" title "</id>"
          "<title>" (blog-feed-title title) "</title>"
          "<updated>" (atom-timestamp) "</updated>"
          ;; TODO: Entry list formatting
          (org-list-to-generic list
                               '())
          "</feed>"))

(defun blog-atom-entry (entry _style project)
  "Generate an Atom feed item for ENTRY, in PROJECT."
  (let ((article-url (published-file-url entry project)))
    (concat
     "<entry>"
     "<title>" (org-html-encode-plain-text (org-publish-find-title entry project)) "</title>"
     "<id>" article-url "</id>"
     "<updated>" (atom-timestamp (org-publish-find-date entry project)) "</updated>"
     "<link href=\"" article-url "\"/>"
     (if-let ((description
               (org-publish-find-property entry :description project 'html)))
         (concat "<summary>" (org-html-encode-plain-text description) "</summary>")
       (if-let ((subtitle
                 (org-publish-find-property entry :subtitle project 'html)))
           (concat "<summary>" (org-html-encode-plain-text (string-join subtitle " ")) "</summary>")))
     "</entry>")))

(defun feed-project (subdir &rest properties)
  "Generate entries for a project at SUBDIR, with the given TITLE and overriding PROPERTIES."
  (let ((title (capitalize subdir))
        (common-properties
         (list :base-directory (source-dir subdir)
               :publishing-directory (export-dir subdir)
               :auto-sitemap t
               :sitemap-sort-files 'anti-chronologically)))

    `((,title
       ,@properties
       ,@common-properties
       :sitemap-function ,#'blog-publish-sitemap
       :sitemap-format-entry ,#'blog-sitemap-entry
       :sitemap-title ,title
       :sitemap-filename "index.org"
       :html-head ,(concat org-html-head
                           "<link rel=\"alternate\" type=\"application/atom+xml\" href=\"atom.xml\" title=\""
                           (blog-feed-title title)
                           "\">"))

      (,(concat title " Atom Feed")
       ,@common-properties
       :sitemap-filename "atom.xml"
       :sitemap-format-entry blog-atom-entry
       :sitemap-function blog-atom-sitemap
       :sitemap-title ,subdir
       :base-extension "atom\\|org"
       :exclude "index.org\\|.*~$"
       ;; This project only exists to generate the atom sitemap.
       ;; I am choosing to consider the fact this also publishes the
       ;; Org files directly to be a feature.
       :publishing-function org-publish-attachment))))

(setq
 org-publish-use-timestamps-flag nil
 org-publish-project-alist
 `(("files"
    :base-directory ,(source-dir "files")
    :base-extension any
    :publishing-directory ,(export-dir "files")
    :publishing-function org-publish-attachment
    :recursive t)
   ("root-files"
    :base-directory ,(source-dir "root-files")
    :base-extension any
    :publishing-directory ,website-export-dir
    :publishing-function org-publish-attachment
    :recursive t)
   ("toplevel"
    :base-directory ,website-dir
    :publishing-directory ,website-export-dir
    :auto-sitemap t)
   ,@(feed-project "blog")
   ,@(feed-project "wiki")))


(provide 'publish-config)
;;; publish-config.el ends here
