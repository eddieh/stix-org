(let ((default-directory (concat default-directory "/lisp")))
  (normal-top-level-add-subdirs-to-load-path))

(require 'htmlize)
(require 'org)

(setq org-html-doctype "html5")
(setq org-html-html5-fancy t)
(setq org-html-head-include-default-style nil)
(setq org-html-head "")
(setq org-html-head-include-scripts nil)
(setq org-html-preamble t)
(setq org-html-preamble-format '(("en" "")))
(setq org-html-postamble t)
(setq org-html-postamble-format
  '(("en" "<p class=\"author\">Author: %a</p>
<p class=\"date\">Date: %d</p>")))
(setq org-html-htmlize-output-type 'css)
(setq org-html-with-latex 'verbatim)
(setq org-html-self-link-headlines nil)
(setq org-html-toplevel-hlevel 1)
