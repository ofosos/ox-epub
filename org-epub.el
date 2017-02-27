;;; org-epub.el --- Export org mode projects to EPUB

;; Copyright (c) 2017 - Mark Meyer

;; Author: Mark Meyer <mark@ofosos.org>
;; URL: http://github.com/ofosos/org-epub
;; Version: 0.1.0

;; Code goes here

(require 'cl)
(require 'cl-lib)
(require 'ox-publish)
(require 'ox-html)
(require 'org-element)

(org-export-define-derived-backend 'epub 'html
  :translate-alist
  '((template . org-epub-template))
  )

(defvar *org-epub-current-file* nil)
(defvar *org-epub-contents-alist* '())

(defun org-epub-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let* ((headlines-raw (org-export-collect-headlines info 2))
	 (headlines (mapcar (lambda (headline)
			      (let ((plist (car (cdr headline))))
				(list
				 (plist-get plist :raw-value)
				 *org-epub-current-file*
				 (plist-get plist :level)
				 (org-export-get-reference headline info)))) headlines-raw)))
    (setf (alist-get (intern *org-epub-current-file*) *org-epub-contents-alist*) headlines))
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
  (setq *org-epub-current-file* filename)
  (org-publish-org-to 'epub filename
		      (concat "." (or (plist-get plist :html-extension)
				      org-html-extension
				      "html"))
		      plist pub-dir))

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

(defun org-epub-publish-finish (plist)
  (let* ((generated '())
	 (project (cons "foo" plist))
	 (files (org-publish-get-base-files project))
	 (uid (org-publish-property :uid project))
	 (toc-depth (or (org-publish-property :toc-depth project) 2))
	 (title (org-publish-property :title project))
	 (language (org-publish-property :language project))
	 (subject (org-publish-property :subject project))
	 (description (org-publish-property :description project))
	 (creator (org-publish-property :creator project))
	 (publisher (org-publish-property :publisher project))
	 (date (org-publish-property :epub-date project))
	 (rights (org-publish-property :rights project))
	 (base-dir (org-publish-property :base-directory project))
	 (target-dir (org-publish-property :publishing-directory project))
	 (toc-nav (generate-toc (apply 'append (mapcar 'cdr *org-epub-contents-alist*)) base-dir))
	 (generated (mapcar (lambda (file)
			      (cons (file-name-base file)
				    (concat (unless (seq-empty-p (file-relative-name file base-dir))
					      (file-relative-name
					       (file-name-directory file) base-dir))
					    (file-name-base file) ".html")))
			    files)))
    (with-current-buffer (find-file (concat target-dir "toc.ncx"))
      (erase-buffer)
      (insert (template-toc-ncx uid toc-depth title toc-nav))
      (save-buffer 0)
      (kill-buffer))
    (with-current-buffer (find-file (concat target-dir "content.opf"))
      (erase-buffer)
      (insert (template-content-opf title language uid subject description creator publisher date rights
				    (gen-manifest generated)
     				    (gen-spine generated)))
      (save-buffer 0)
      (kill-buffer))
    (with-current-buffer (find-file (concat target-dir "META-INF/container.xml"))
      (erase-buffer)
      (insert (template-container))
      (unless (file-exists-p (concat target-dir "META-INF"))
	(make-directory (concat target-dir "META-INF")))
      (save-buffer 0)
      (kill-buffer))
    (with-current-buffer (find-file (concat target-dir "mimetype"))
      (erase-buffer)
      (insert (template-mimetype))
      (save-buffer 0)
      (kill-buffer))))

(defun generate-toc (headlines base-dir)
  (let ((toc-id 0)
	(current-level 0))
    (with-output-to-string
      (mapcar
       (lambda (headline)
	 (let* ((title (nth 0 headline))
		(base (file-name-base (nth 1 headline)))
		(rel-target (file-relative-name (nth 1 headline) base-dir))
		(target (concat (file-name-directory rel-target) base ".html"))
		(level (nth 2 headline))
		(ref (nth 3 headline)))
	   (incf toc-id)
	   (cond
	    ((< current-level level)
	     (incf current-level))
	    ((> current-level level)
	     (princ "</navPoint>")
	     (while (> current-level level)
	       (decf current-level)
	       (princ "</navPoint>")))
	    ((eq current-level level)
	     (princ "</navPoint>")))
	   (princ
	    (concat (format "<navPoint class=\"h%d\" id=\"%s-%d\">\n" current-level base toc-id)
		    (format "<navLabel><text>%s</text></navLabel>\n" (org-html-encode-plain-text title))
		    (format "<content src=\"%s#%s\"/>" target ref)))))
       headlines)
      (while (> current-level 0)
	(princ "</navPoint>")
	(decf current-level)))))

;;; org-epub.el ends here
