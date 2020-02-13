(let ((default-directory (concat default-directory "/lisp")))
  (normal-top-level-add-subdirs-to-load-path))

(require 'htmlize)
(require 'org)

(defun string-from-file (path)
  "Get the content of the file at PATH as a string"
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(setq org-html-head-extra
      (if (getenv "STIX_ORG_GITHUB_PAGES_GA")
	  (string-from-file "resources/ga.html") ""))

(setq org-html-preamble-format '(("en" "")))
(setq org-html-postamble-format
  '(("en" "<p class=\"author\">Author: %a</p>
<p class=\"date\">Date: %d</p>")))
(setq org-html-htmlize-output-type 'css)
(setq org-html-self-link-headlines nil)
(setq org-html-toplevel-hlevel 1)
