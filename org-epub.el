;;; org-epub.el --- Export org mode projects to EPUB -*- lexical-binding: t; -*-

;; Copyright (c) 2017 - Mark Meyer

;; Author: Mark Meyer <mark@ofosos.org>
;; Maintainer: Mark Meyer <mark@ofosos.org>

;; URL: http://github.com/ofosos/org-epub
;; Keywords: hypermedia

;; Version: 0.1.0

;; Package-Requires: ((emacs "25") (org "9"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is an addition to the standard org-mode exporters.  The package
;; extends the (X)HTML exporter to produce EPUB files.  It eliminates
;; all inline CSS and JavaScript to accomplish this.  This exporter
;; will also tie the XHTML DTD to XHTML 1.1, a concrete DTD specifier
;; that was not supported by ox-html previously.

;; The main part is the generation of the table of contents in machine
;; readable form, as well as the spine, which defines the order in
;; which files are presented.  A lesser part is the inclusion of
;; various metadata properties, among them authorship and rights.

;;; Code:

(require 'cl-lib)
(require 'ox-publish)
(require 'ox-html)
(require 'org-element)

(org-export-define-derived-backend 'epub 'html
  :translate-alist
  '((template . org-epub-template))
  )

(defvar org-epub-current-file* nil
  "The current file we're exporting.")

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
				 (org-export-get-reference headline info)))) headlines-raw))
	 (cache (org-publish-cache-get "org-epub-headlines")))
    (setf (alist-get (intern *org-epub-current-file*) cache) headlines)
    (org-publish-cache-set "org-epub-headlines" cache))
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

(defun org-epub-template-toc-ncx (uid toc-depth title toc-nav)
  "Create the toc.ncx file.

UID is the uid/url of the file.  TOC-DEPTH is the depth of the toc
that should be shown to the readers.  TITLE is the title of the
ebook and TOC-NAV being the raw contents enclosed in navMap."
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

(defun org-epub-template-content-opf (title language uid subject description creator publisher date rights manifest spine cover)
  "Create the content.opf file.

The following metadata is included in the content.opf: TITLE is
the title of the ebook, LANGUAGE is the language, UID is the
uid/url of the ebook, SUBJECT is the theme of the book in
keywords, DESCRIPTION is a longer free form description, CREATOR
is the author, PUBLISHER identifies the publisher, DATE is the
date of publication, RIGHTS signifies the copyrights.

The following arguments are XML strings: MANIFEST is the content
inside the manifest tags, this should include all user generated
html files but not things like the cover page, SPINE is an XML
string with the list of html files in reading order.

Finally COVER is the cover image filename."
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
      <dc:rights>" rights "</dc:rights>"
      (when cover
	"<meta name=\"cover\" content=\"cover-image\"/>")
      "
   </metadata>

   <manifest>\n"
      (when cover
	(concat "<item id=\"cover\" href=\"cover.html\" media-type=\"application/xhtml+xml\"/>
         <item id=\"cover-image\" href=\"" cover "\" media-type=\"image/jpeg\"/>"))
      "<item id=\"ncx\"      href=\"toc.ncx\"
         media-type=\"application/x-dtbncx+xml\" />"
      manifest
      
   "</manifest>

   <spine toc=\"ncx\">
     <itemref idref=\"cover\" linear=\"no\" />"
   spine

   "</spine>

 <guide>
 <reference type=\"cover\" href=\"cover.html\" />
 </guide>

</package>"))

(defun org-epub-gen-manifest (files)
  "Generate the manifest XML string.

FILES is the list of files to be included in the manifest."
  (mapconcat
   (lambda (file)
     (concat "<item id=\"" (car file) "\"      href=\"" (rest file) "\"
            media-type=\"application/xhtml+xml\" />\n"))
   files ""))

(defun org-epub-gen-spine (files)
  "Generate the spine XML string.

FILES is the list of files to be included in the spine, these
must be in reading order."
  (mapconcat
   (lambda (file)
     (concat "<itemref idref=\"" (car file) "\" />\n"))
   files ""))

(defun org-epub-template-container ()
  "Generate the container.xml file, the root of any EPUB."
  "<?xml version=\"1.0\"?>
<container version=\"1.0\" xmlns=\"urn:oasis:names:tc:opendocument:xmlns:container\">
   <rootfiles>
      <rootfile full-path=\"content.opf\"
      media-type=\"application/oebps-package+xml\"/>
   </rootfiles>
</container>")

(defun org-epub-template-cover (cover-file width height)
  "Generate a HTML template for the cover page.

COVER-FILE is the filename of a jpeg file, while WIDTH and HEIGHT are
properties of the image."
   (concat "<?xml version=\"1.0\" encoding=\"utf-8\"?>
 <!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
 
 <html xmlns=\"http://www.w3.org/1999/xhtml\">
 <head>
 <title></title>
 </head>
 
 <body>
 <svg version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"
  width=\"100%\" height=\"100%\" viewBox=\"0 0 573 800\" preserveAspectRatio=\"xMidYMid meet\">
 <image xlink:href=\"" cover-file "\" height=\"" (format "%d" height) "\" width=\"" (format "%d" width) "\" />
 </svg>
 </body>
 </html>"))

(defun org-epub-template-mimetype ()
  "Generate the mimetype file for the epub."
  "application/epub+zip")

(defun org-epub-publish-finish (plist)
  "Finish the generation of the EPUB.

This function is usually called, when all HTML files have
finished exporting.  PLIST is the project property list."
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
	 (epub (org-publish-property :epub-file project))
	 (cover (org-publish-property :epub-cover project))
	 (cover-height (org-publish-property :epub-cover-height project))
	 (cover-width (org-publish-property :epub-cover-width project))
	 (target-dir (org-publish-property :publishing-directory project))
	 (toc-nav (org-epub-generate-toc (apply 'append (mapcar 'cdr (org-publish-cache-get "org-epub-headlines"))) base-dir))
	 (generated (mapcar (lambda (file)
			      (cons (file-name-base file)
				    (concat (unless (seq-empty-p (file-relative-name file base-dir))
					      (file-relative-name
					       (file-name-directory file) base-dir))
					    (file-name-base file) ".html")))
			    files)))
    (with-current-buffer (find-file (concat target-dir "toc.ncx"))
      (erase-buffer)
      (insert (org-epub-template-toc-ncx uid toc-depth title toc-nav))
      (save-buffer 0)
      (kill-buffer))
    (when cover
      (with-current-buffer (find-file (concat target-dir "cover.html"))
	(erase-buffer)
	(insert (org-epub-template-cover cover cover-width cover-height))
	(save-buffer 0)
	(kill-buffer)))
    (with-current-buffer (find-file (concat target-dir "content.opf"))
      (erase-buffer)
      (insert (org-epub-template-content-opf title language uid subject description creator publisher date rights
				    (org-epub-gen-manifest generated)
     				    (org-epub-gen-spine generated) cover))
      (save-buffer 0)
      (kill-buffer))
    (with-current-buffer (find-file (concat target-dir "META-INF/container.xml"))
      (erase-buffer)
      (insert (org-epub-template-container))
      (unless (file-exists-p (concat target-dir "META-INF"))
	(make-directory (concat target-dir "META-INF")))
      (save-buffer 0)
      (kill-buffer))
    (with-current-buffer (find-file (concat target-dir "mimetype"))
      (erase-buffer)
      (insert (org-epub-template-mimetype))
      (save-buffer 0)
      (kill-buffer))
    (org-epub-zip-it-up epub files base-dir target-dir cover)))

(defun org-epub-zip-it-up (epub-file files base-dir target-dir cover)
  "Create the .epub file by zipping up the contents.

EPUB-FILE is the target filename, FILES is the list of source
files to process, BASE-DIR is the base dir of the source files
while TARGET-DIR is the directory where exported HTML files
live.  COVER is the filename of the cover image, which may be
nil."
  (let ((default-directory target-dir)
	(meta-files '("META-INF/container.xml" "content.opf" "toc.ncx")))
    (call-process "zip" nil '(:file "zip.log") nil
		  "-Xu0"
		  epub-file
		  "mimetype")
    (apply 'call-process "zip" nil '(:file "zip.log") nil
	   "-Xu9"
	   epub-file
	   (append meta-files (when cover (list cover "cover.html"))
		   (mapcar (lambda (file)
			     (replace-regexp-in-string "\\.org" ".html"
						       (file-relative-name file base-dir))) files)))))

(defun org-epub-generate-toc (headlines base-dir)
  "Generate the toc/navMap entries for the toc.ncx file.

HEADLINES are the headlines to include in the toc, while BASE-DIR
is the base dir where the source files for the project live."
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
	   (cl-incf toc-id)
	   (cond
	    ((< current-level level)
	     (cl-incf current-level))
	    ((> current-level level)
	     (princ "</navPoint>")
	     (while (> current-level level)
	       (cl-decf current-level)
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
	(cl-decf current-level)))))

(provide 'org-epub)

;;; org-epub.el ends here
