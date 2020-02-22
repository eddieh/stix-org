(let ((default-directory (concat default-directory "/lisp")))
  (normal-top-level-add-subdirs-to-load-path))

(require 'htmlize)
(require 'org)
(require 'ox)

(defconst stix-org/example-description
  (replace-regexp-in-string (regexp-quote "\n") " " "Example Emacs
Org file demonstrating features, structures, and markup exported
with the STIX theme for scientific and technical documents"))

(defun string-from-file (path)
  "Get the content of the file at PATH as a string"
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(setq org-html-head-extra
      (if (getenv "STIX_ORG_GITHUB_PAGES_GA")
	  (concat (string-from-file "resources/ga.html")
		  (replace-regexp-in-string
		   (regexp-quote "__DESC__") stix-org/example-description
		   (string-from-file "resources/social.html") t))
	""))

(setq org-html-preamble-format '(("en" "")))
(setq org-html-postamble-format
  '(("en" "<p class=\"author\">Author: %a</p>
<p class=\"date\">Date: %d</p>")))
(setq org-html-htmlize-output-type 'css)
(setq org-html-self-link-headlines nil)
(setq org-html-toplevel-hlevel 1)

;; We override org-export-get-environment so that the ext-plist is
;; applied after the file local options, that way we can use a
;; different description when publishing to GitHub Pages.
(defun example-org-html-export-to-github-pages ()
  (message "Export to HTML")
  (defun org-export-get-environment (&optional backend subtreep ext-plist)
    (dolist (pair (org-export--list-bound-variables))
      (set (make-local-variable (car pair)) (nth 1 pair)))
    (org-combine-plists
     (org-export--get-global-options backend)
     (org-export--get-inbuffer-options backend)
     ext-plist
     (and subtreep (org-export--get-subtree-options backend))))
  (org-html-export-to-html nil nil nil nil
			   `(:description ,stix-org/example-description)))
