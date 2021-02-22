(defun guitar--get-attrs (src-block)
  (org-export-read-attribute :attr_guitar src-block))

(defun guitar-html-src-block (src-block contents info)
    "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS is nil.  INFO is a plist used as a communication
channel.

If #+attr_guitar :row t is used for the source block, emit a table and evaluate lisp code.

If #+attr_guitar :small t is used for the source block, emit a small and centered container
and evaluate lisp code.

Otherwise, delegate to normal html block code. "
    (let ((attrs (guitar--get-attrs src-block)))
      (message (format "%s" (type-of (plist-get attrs :container))))
      (pcase (plist-get attrs :container)
        ("small" (let ((code (org-element-property :value src-block)))
                   (with-temp-buffer
                     (save-excursion
                       (insert "<div class=\"diagram-container\">")
                       (insert "<div class=\"diagram\">")
                       (cl-loop for diagram in (read (format "(%s)" code)) do
                                (insert (org-babel-execute:lisp (format "%S" diagram) '((:result-params "output")))))
                       (insert "</div>")
                       (insert "</div>")
                       (buffer-string)))))
        ("row" (let ((code (org-element-property :value src-block)))
                 (with-temp-buffer
                   (save-excursion
                     (insert "<div class=\"diagram-container\">")
                     (cl-loop for diagram in (read (format "(%s)" code)) do
                              (insert "<div class=\"diagram\">")
                              (insert (org-babel-execute:lisp (format "%S" diagram) '((:result-params "output"))))
                              (insert "</div>"))
                     (insert "</div>")
                     (buffer-string)))))
        (otherwise (org-export-with-backend 'html src-block contents info)))))

(org-export-define-derived-backend 'guitar-html 'html
  :translate-alist '((src-block . guitar-html-src-block)))

(defun guitar-doc-export ()
  (interactive)
  (save-window-excursion
    (with-current-buffer (find-file "~/dev/guitar-improv/guitar.org")
      (guitar---export))))

(defun guitar-toggle-export-on-save ()
  (interactive)
  (c/toggle-org-export-on-save 'guitar-doc-export))

;; Mostly a copy of org-html-export-to-html
(defun guitar---export
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a HTML file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (let* ((extension (concat
		     (when (> (length org-html-extension) 0) ".")
		     (or (plist-get ext-plist :html-extension)
			 org-html-extension
			 "html")))
	 (file (org-export-output-file-name extension subtreep))
	 (org-export-coding-system org-html-coding-system))
    (org-export-to-file 'guitar-html file
      async subtreep visible-only body-only ext-plist)))

(provide 'guitar)
