;;; ox-epub.el --- Export org mode projects to EPUB -*- lexical-binding: t; -*-

;; Copyright (c) 2017 - Mark Meyer

;; Author: Mark Meyer <mark@ofosos.org>
;; Maintainer: Mark Meyer <mark@ofosos.org>

;; URL: http://github.com/ofosos/org-epub
;; Keywords: hypermedia

;; Version: 0.1.0

;; Package-Requires: ((emacs "24.3") (org "9"))

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
  :options-alist
  '((:epub-uid "UID" nil nil t)
    (:epub-subject "Subject" nil nil t)
    (:epub-description "Description" nil nil t)
    (:epub-publisher "Publisher" nil nil t)
    (:epub-rights "License" nil nil t))
    
  :translate-alist
  '((template . org-epub-template))
  :menu-entry
  '(?E "Export to Epub"
       ((?e "As Epub file" org-epub-export-to-epub))))

(defvar org-epub-current-file nil
  "The current file we're exporting.")

(defvar org-epub-zip-dir nil
  "The temporary directory to export to")

(defun org-epub-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let* ((headlines-raw (org-export-collect-headlines info 2))
	 (headlines (mapcar (lambda (headline)
				       (let ((plist (car (cdr headline))))
					 (list
					  (plist-get plist :raw-value)
					  org-epub-current-file
					  (plist-get plist :level)
					  (org-export-get-reference headline info)))) headlines-raw)))
    ;; options: uid (:epub-uid), title (:title), language (:language),
    ;; subject (:epub-subject), description (:epub-description), creator
    ;; (:creator), publisher, date (:date), rights (:epub-rights)
    (let ((uid (plist-get info :epub-uid))
	  (title (plist-get info :title))
	  (language (plist-get info :language))
	  (subject (plist-get info :epub-subject))
	  (description (plist-get info :epub-description))
	  (creator (plist-get info :creator))
	  (publisher (plist-get info :epub-publisher))
	  (date (plist-get info :date))
	  (rights (plist-get info :epub-rights)))
      ;; maybe set toc-depth "2" to some dynamic value
      (with-current-buffer (find-file (concat org-epub-zip-dir "toc.ncx"))
	(erase-buffer)
	(insert
	 (org-epub-template-toc-ncx uid 2 title (org-epub-generate-toc-single headlines "body.html")))
	(save-buffer 0)
	(kill-buffer))
      ;; insert cover export
      (with-current-buffer (find-file (concat org-epub-zip-dir "content.opf"))
	(erase-buffer)
	(insert (org-epub-template-content-opf title language uid subject description creator publisher date rights
					       (org-epub-gen-manifest '(("body-html" . "body.html")))
					       (org-epub-gen-spine '(("body-html" . "body.html"))) nil)) ;; FIXME cover
	(save-buffer 0)
	(kill-buffer))))
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
     ;; Document title.
     (when (plist-get info :with-title)
       (let ((title (plist-get info :title))
	     (subtitle (plist-get info :subtitle))
	     (html5-fancy (org-html--html5-fancy-p info)))
	 (when title
	 (format
	  (if html5-fancy
	      "<header>\n<h1 class=\"title\">%s</h1>\n%s</header>"
	    "<h1 class=\"title\">%s%s</h1>\n")
	  (org-export-data title info)
	  (if subtitle
	      (format
	       (if html5-fancy
		   "<p class=\"subtitle\">%s</p>\n"
		 "\n<br>\n<span class=\"subtitle\">%s</span>\n")
	       (org-export-data subtitle info))
	    "")))))
     contents
     "</div>"
					;   (format "</%s>\n" (nth 1 (assq 'content (plist-get info :html-divs))))
     ;; Postamble.
     (org-html--build-pre/postamble 'postamble info)
     ;; Closing document.
     "</body>\n</html>"))

;; see ox-odt

;;compare org-export-options-alist
;;;###autoload
(defun org-epub-export-to-epub (&optional async subtreep visible-only ext-plist)
  "Export the current buffer to an EPUB file.

ASYNC defines wether this process should run in the background,
SUBTREEP supports narrowing of the document, VISIBLE-ONLY allows
you to export only visible parts of the document, EXT-PLIST is
the property list for the export process."
  (interactive)
  (let* ((outfile (org-export-output-file-name ".epub" subtreep))
	 (out-file-type (file-name-extension outfile))
	 (org-epub-zip-dir (file-name-as-directory
			    (make-temp-file (format "%s-" out-file-type) t)))
	 (body (org-export-as 'epub subtreep visible-only nil ext-plist)))
    (with-current-buffer (find-file (concat org-epub-zip-dir "META-INF/container.xml"))
      (erase-buffer)
      (insert (org-epub-template-container))
      (unless (file-exists-p (concat org-epub-zip-dir "META-INF"))
	(make-directory (concat org-epub-zip-dir "META-INF")))
      (save-buffer 0)
      (kill-buffer))
    (with-current-buffer (find-file (concat org-epub-zip-dir "mimetype"))
      (erase-buffer)
      (insert (org-epub-template-mimetype))
      (save-buffer 0)
      (kill-buffer))
    (with-current-buffer (find-file (concat org-epub-zip-dir "body.html"))
      (erase-buffer)
      (insert body)
      (save-buffer 0)
      (kill-buffer))
    (org-epub-zip-it-up outfile '("body.html") org-epub-zip-dir nil)
    (message "Generated %s" outfile)
    (expand-file-name outfile)))

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
     (concat "<item id=\"" (car file) "\"      href=\"" (cdr file) "\"
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

(defun org-epub-zip-it-up (epub-file files target-dir cover)
  "Create the .epub file by zipping up the contents.

EPUB-FILE is the target filename, FILES is the list of source
files to process, while TARGET-DIR is the directory where
exported HTML files live.  COVER is the filename of the cover
image, which may be nil."
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
		   files)))
  (copy-file (concat target-dir epub-file) default-directory t))

(defun org-epub-generate-toc-single (headlines filename)
  "Generate a single file TOC.

HEADLINES is a list containing the abbreviated headline
information. The name of the target file is given by FILENAME."
  (let ((toc-id 0)
	(current-level 0))
    (with-output-to-string
      (mapc
       (lambda (headline)
	 (let* ((title (nth 0 headline))
		(base (file-name-base (nth 1 headline)))
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
		    (format "<content src=\"%s#%s\"/>" filename ref)))))
       headlines)
      (while (> current-level 0)
	(princ "</navPoint>")
	(cl-decf current-level)))))

(provide 'ox-epub)

;;; ox-epub.el ends here
