;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(require 'ox-publish)
(setq org-publish-project-alist
      '(

	("bjcp-notes"
	 :base-directory "~/org/bjcp/"
	 :base-extension "org"
	 :publishing-directory "~/bjcp-epub/reader1/"
	 :recursive t
	 :publishing-function org-epub-publish-to-epub
	 :headline-levels 4             ; Just the default for this project.
	 :auto-preamble t
	 )

	("bjcp" :components ("bjcp-notes" "org-static"))
	))

(setq org-html-doctype-alist (cons (cons "xhtml-epub" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">") org-html-doctype-alist))

(custom-set-variables
 '(org-html-head-include-scripts t)
 '(org-html-head-include-default-style t))

(require 'cl-lib)
(require 'ox-html)

(unless (assoc "xhtml-epub" org-html-doctype-alist)
  (add-to-list 'org-html-doctype-alist
	       (cons "xhtml-epub" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">")))

(org-export-define-derived-backend 'epub 'html
  :translate-alist
  '((template . org-epub-template))
  )


(defun org-epub-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   (when (and (not (org-html-html5-p info)) (org-html-xhtml-p info))
     (let* ((xml-declaration (plist-get info :html-xml-declaration))
	    (decl (or (and (stringp xml-declaration) xml-declaration)
		      (cdr (assoc (plist-get info :html-extension)
				  xml-declaration))
		      (cdr (assoc "html" xml-declaration))
		      "")))
       (when (not (or (not decl) (string= "" decl)))
	 (format "%s\n"
		 (format decl
			 (or (and org-html-coding-system
				  (fboundp 'coding-system-get)
				  (coding-system-get org-html-coding-system 'mime-charset))
			     "iso-8859-1"))))))
   "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">"
   "\n"
   (concat "<html"
	   (format
	    " xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\""
	    (plist-get info :language) (plist-get info :language))
	   ">\n")
   
   "<head>\n"
   (org-html--build-meta-info info)
   "</head>\n"
   "<body>\n"
   ;; Preamble.
   (org-html--build-pre/postamble 'preamble info)
   ;; Document contents.
;   (let ((div (assq 'content (plist-get info :html-divs))))
					;     (format "<%s id=\"%s\">\n" (nth 1 div) (nth 2 div)))
   "<div id=\"content\">"
   contents
   "</div>"
;   (format "</%s>\n" (nth 1 (assq 'content (plist-get info :html-divs))))
   ;; Postamble.
   (org-html--build-pre/postamble 'postamble info)
   ;; Closing document.
   "</body>\n</html>"))

(defun org-epub-publish-to-epub (plist filename pub-dir)
  "Publish an org file to epub.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'epub filename
		      (concat "." (or (plist-get plist :html-extension)
				      org-html-extension
				      "html"))
		      plist pub-dir))

(require 'org-element)
(require 'cl)

(set-buffer (current-buffer))

(insert (with-output-to-string (princ "foobar")))foobar
(let ((*toclevel* 2))
  (insert (generate-toc "judge-procedures-manual.html" "judge-procedures-manual.org")))

(defun generate-tocs ()
  (interactive)
  (save-excursion
    (with-current-buffer (get-buffer-create "*epub-toc*")
      (insert (generate-toc-for-buffers '("judge-procedures-manual.org"))))))

(defun generate-toc-for-buffers (buflis)
  (let ((*toclevel* 2))
    (concat
     (mapcar (lambda (buf)
	       (with-current-buffer buf
		 (generate-toc)))
	     buflis))))

(defvar *toclevel* 2)

(defun gen-filename (filename)
  (replace-regexp-in-string "\\.org" ".html" filename))

(defun gen-descriptor (filename)
  (replace-regexp-in-string "^.+/" ""
			    (replace-regexp-in-string "\\.org" ""
						      (replace-regexp-in-string "\\.html" "" filename))))

(setq org-epub-projects
      '(("reader-one" .
	 ((base-dir . "/home/mark/org/bjcp/")
	  (target-dir . "/home/mark/bjcp-epub/reader1/")
	  (file-list . ("judge-procedures-manual.org" "master-level-sheet.org"))
	  (author . "Beer Judge Certification Program")
	  (publisher . "Mark Meyer")
	  (rights . "t.b.d.")
	  (toc-depth . 2)
	  (uid . "http://ofosos.org/bjcp/jpm.epub")
	  (title . "Beer Judge Prep - Reader 1")
	  (language . "en")
	  (subject . "non-fiction beer")
	  (description . "Beer Judging Prep Class, Reader for Session One")
	  (date . "2017-01-01")))))

(gen-filename (car (rest (assoc 'file-list (assoc "reader-one" org-epub-projects)))))

(require 'ox-publish)
(require 'ox-html)

(template-toc-ncx "test" 3 "foobar" "test")

(defun template-toc-ncx (uid toc-depth title toc-nav)
  (concat
   "<?xml version=\"1.0\"?>
<!DOCTYPE ncx PUBLIC \"-//NISO//DTD ncx 2005-1//EN\" 
   \"http://www.daisy.org/z3986/2005/ncx-2005-1.dtd\">

<ncx xmlns=\"http://www.daisy.org/z3986/2005/ncx/\" version=\"2005-1\">

   <head>
      <meta name=\"dtb:uid\" content=\""
   uid
   "\"/>
      <meta name=\"dtb:depth\" content=\""
   (format "%d" toc-depth)
   "\"/>
      <meta name=\"dtb:totalPageCount\" content=\"0\"/>
      <meta name=\"dtb:maxPageNumber\" content=\"0\"/>
   </head>

   <docTitle>
      <text>"
   title
   "</text>
   </docTitle>

   <navMap>"
   toc-nav
   "</navMap>
</ncx>"))

(defun template-content-opf (title language uid subject description creator publisher date rights manifest spine)
  (concat
   "<?xml version=\"1.0\"?>

<package xmlns=\"http://www.idpf.org/2007/opf\" unique-identifier=\"dcidid\" 
   version=\"2.0\">

   <metadata xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
      xmlns:dcterms=\"http://purl.org/dc/terms/\"
      xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
      xmlns:opf=\"http://www.idpf.org/2007/opf\">
      <dc:title>" title "</dc:title>
      <dc:language xsi:type=\"dcterms:RFC3066\">" language "</dc:language>
      <dc:identifier id=\"dcidid\" opf:scheme=\"URI\">"
      uid
         "</dc:identifier>
      <dc:subject>" subject 
         "</dc:subject>
      <dc:description>" description

         "</dc:description>
      <dc:creator>" creator "</dc:creator>
      <dc:publisher>" publisher "</dc:publisher>
      <dc:date xsi:type=\"dcterms:W3CDTF\">" date "</dc:date>
      <dc:rights>" rights "</dc:rights>
   </metadata>

   <manifest>
      <item id=\"ncx\"      href=\"toc.ncx\"
         media-type=\"application/x-dtbncx+xml\" />"
      manifest
      
   "</manifest>

   <spine toc=\"ncx\">"
   spine

   "</spine>
</package>"))

(defun gen-manifest (files)
  (mapconcat
   (lambda (file)
     (concat "<item id=\"" (car file) "\"      href=\"" (rest file) "\"
            media-type=\"application/xhtml+xml\" />\n"))
   files ""))

(defun gen-spine (files)
  (mapconcat
   (lambda (file)
     (concat "<itemref idref=\"" (car file) "\" />\n"))
   files ""))

(gen-spine '(("foo" . "foo.html") ("bar" . "bar.html")))

(defun template-container ()
  "<?xml version=\"1.0\"?>
<container version=\"1.0\" xmlns=\"urn:oasis:names:tc:opendocument:xmlns:container\">
   <rootfiles>
      <rootfile full-path=\"content.opf\"
      media-type=\"application/oebps-package+xml\"/>
   </rootfiles>
</container>")

(defun template-mimetype ()
  "application/epub+zip")


(defun generate-project (name)
  (let* ((project (rest (assoc name org-epub-projects)))
	 (generated '())
	 (files (rest (assoc 'file-list project)))
	 (uid (rest (assoc 'uid project)))
	 (toc-depth (or (rest (assoc 'toc-depth project)) 2))
	 (title (rest (assoc 'title project)))
	 (language (rest (assoc 'language project)))
	 (subject (rest (assoc 'subject project)))
	 (description (rest (assoc 'description project)))
	 (creator (rest (assoc 'creator project)))
	 (publisher (rest (assoc 'publisher project)))
	 (date (rest (assoc 'date project)))
	 (rights (rest (assoc 'rights project)))
	 (base-dir (rest (assoc 'base-dir project)))
	 (target-dir (rest (assoc 'target-dir project)))
	 (project '("foobar" :base-directory "~/org/bjcp/"
				    :base-extension "org"
				    :publishing-directory "~/bjcp-epub/reader1/"
				    :recursive t
				    :publishing-function org-epub-publish-to-epub
				    :headline-levels 4             ; Just the default for this project.
				    :auto-preamble t))
	 (toc-nav (mapconcat (lambda (file)
			       (org-epub-publish-to-epub project (concat base-dir file) target-dir)
			       (setq generated (cons (cons (gen-descriptor file) (concat (gen-descriptor file) ".html")) generated))
				 (generate-toc (concat (gen-descriptor file) ".html")
					       (find-file (concat base-dir file))))
			     files "")))
    (with-current-buffer (find-file (concat target-dir "toc.ncx"))
      (insert (template-toc-ncx uid toc-depth title toc-nav))
      (save-buffer))
    (with-current-buffer (find-file (concat target-dir "content.opf"))
      (insert (template-content-opf title language uid subject description creator publisher date rights
				    (gen-manifest generated)
				    (gen-spine generated)))
      (save-buffer))
    (with-current-buffer (find-file (concat target-dir "META-INF/container.xml"))
      (insert (gen-container))
      (make-directory (concat target-dir "META-INF"))
      (save-buffer))
    (with-current-buffer (find-file (concat target-dir "mimetype"))
      (insert (template-mimetype))
      (save-buffer))))

(org-publish-initialize-cache "epub-project")
(generate-project "reader-one")

(defun generate-toc (target-file &optional buffer)
  (interactive "p" "b")
  (let ((buffer (or buffer (current-buffer)))
	(toc-id-prefix target-file)
	(toc-id 0))
    (with-current-buffer buffer
      (with-output-to-string
	(let ((stack '())
	      (current-level 0))
	  (org-element-map (org-element-parse-buffer 'headline nil) 'headline
	    (lambda (headline)
	      (incf toc-id)
	      (cond
	       ((< current-level (org-element-property :level headline))
		(push 1 stack))
	       ((> current-level (org-element-property :level headline))
		(princ "</navPoint>")
		(while (> current-level (org-element-property :level headline))
		  (pop stack)
		  (incf (car stack))
		  (decf current-level)
		  (princ "</navPoint>")))
	       ((eq current-level (org-element-property :level headline))
		(incf (car stack))
		(princ "</navPoint>")))
	      (setq current-level (org-element-property :level headline))
	      (unless (> current-level *toclevel*)
		(princ
		 (concat (format "<navPoint class=\"h%d\" id=\"%s-%d\">\n" current-level toc-id-prefix toc-id)
			 (format "<navLabel><text>%s</text></navLabel>\n" (org-element-property :title headline))
			 (format "<content src=\"%s#%s\"/>" target-file
				 (apply 'concat "sec" (mapcar (lambda (num) (format "-%d" num)) (reverse stack)))))))))
	  (while stack
	    (pop stack)
	    (princ "</navPoint>")))))))

