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
    (:epub-rights "License" nil nil t)
    (:epub-style "EPUBSTYLE" nil nil t)
    (:epub-cover "EPUBCOVER" nil nil t))
    
  :translate-alist
  '((template . org-epub-template)
    (link . org-epub-link))
  :menu-entry
  '(?E "Export to Epub"
       ((?e "As Epub file" org-epub-export-to-epub)
	(?O "As Epub file and open"
	    (lambda (a s v b)
	      (if a (org-epub-export-to-epub t s v)
		(org-open-file (org-epub-export-to-epub nil s v) 'system)))))))

(defvar org-epub-zip-dir nil
  "The temporary directory to export to")

(defvar org-epub-image-counter 0
  "Counter for the exported images")

(defvar org-epub-image-list nil
  "List of images that need to be included in the EPUB")

(defvar org-epub-style-default "
  .title  { text-align: center;
             margin-bottom: .2em; }
  .subtitle { text-align: center;
              font-size: medium;
              font-weight: bold;
              margin-top:0; }
  .todo   { font-family: monospace; color: red; }
  .done   { font-family: monospace; color: green; }
  .priority { font-family: monospace; color: orange; }
  .tag    { background-color: #eee; font-family: monospace;
            padding: 2px; font-size: 80%; font-weight: normal; }
  .timestamp { color: #bebebe; }
  .timestamp-kwd { color: #5f9ea0; }
  .org-right  { margin-left: auto; margin-right: 0px;  text-align: right; }
  .org-left   { margin-left: 0px;  margin-right: auto; text-align: left; }
  .org-center { margin-left: auto; margin-right: auto; text-align: center; }
  .underline { text-decoration: underline; }
  #postamble p, #preamble p { font-size: 90%; margin: .2em; }
  p.verse { margin-left: 3%; }
  pre {
    border: 1px solid #ccc;
    box-shadow: 3px 3px 3px #eee;
    padding: 8pt;
    font-family: monospace;
    overflow: auto;
    margin: 1.2em;
  }
  pre.src {
    position: relative;
    overflow: visible;
    padding-top: 1.2em;
  }
  pre.src:hover:before { display: inline;}
  /* Languages per Org manual */
  pre.src-asymptote:before { content: 'Asymptote'; }
  pre.src-awk:before { content: 'Awk'; }
  pre.src-C:before { content: 'C'; }
  /* pre.src-C++ doesn't work in CSS */
  pre.src-clojure:before { content: 'Clojure'; }
  pre.src-css:before { content: 'CSS'; }
  pre.src-D:before { content: 'D'; }
  pre.src-ditaa:before { content: 'ditaa'; }
  pre.src-dot:before { content: 'Graphviz'; }
  pre.src-calc:before { content: 'Emacs Calc'; }
  pre.src-emacs-lisp:before { content: 'Emacs Lisp'; }
  pre.src-fortran:before { content: 'Fortran'; }
  pre.src-gnuplot:before { content: 'gnuplot'; }
  pre.src-haskell:before { content: 'Haskell'; }
  pre.src-java:before { content: 'Java'; }
  pre.src-js:before { content: 'Javascript'; }
  pre.src-latex:before { content: 'LaTeX'; }
  pre.src-ledger:before { content: 'Ledger'; }
  pre.src-lisp:before { content: 'Lisp'; }
  pre.src-lilypond:before { content: 'Lilypond'; }
  pre.src-lua:before { content: 'Lua'; }
  pre.src-matlab:before { content: 'MATLAB'; }
  pre.src-mscgen:before { content: 'Mscgen'; }
  pre.src-ocaml:before { content: 'Objective Caml'; }
  pre.src-octave:before { content: 'Octave'; }
  pre.src-org:before { content: 'Org mode'; }
  pre.src-oz:before { content: 'OZ'; }
  pre.src-plantuml:before { content: 'Plantuml'; }
  pre.src-processing:before { content: 'Processing.js'; }
  pre.src-python:before { content: 'Python'; }
  pre.src-R:before { content: 'R'; }
  pre.src-ruby:before { content: 'Ruby'; }
  pre.src-sass:before { content: 'Sass'; }
  pre.src-scheme:before { content: 'Scheme'; }
  pre.src-screen:before { content: 'Gnu Screen'; }
  pre.src-sed:before { content: 'Sed'; }
  pre.src-sh:before { content: 'shell'; }
  pre.src-sql:before { content: 'SQL'; }
  pre.src-sqlite:before { content: 'SQLite'; }
  /* additional languages in org.el's org-babel-load-languages alist */
  pre.src-forth:before { content: 'Forth'; }
  pre.src-io:before { content: 'IO'; }
  pre.src-J:before { content: 'J'; }
  pre.src-makefile:before { content: 'Makefile'; }
  pre.src-maxima:before { content: 'Maxima'; }
  pre.src-perl:before { content: 'Perl'; }
  pre.src-picolisp:before { content: 'Pico Lisp'; }
  pre.src-scala:before { content: 'Scala'; }
  pre.src-shell:before { content: 'Shell Script'; }
  pre.src-ebnf2ps:before { content: 'ebfn2ps'; }
  /* additional language identifiers per \"defun org-babel-execute\"
       in ob-*.el */
  pre.src-cpp:before  { content: 'C++'; }
  pre.src-abc:before  { content: 'ABC'; }
  pre.src-coq:before  { content: 'Coq'; }
  pre.src-groovy:before  { content: 'Groovy'; }
  /* additional language identifiers from org-babel-shell-names in
     ob-shell.el: ob-shell is the only babel language using a lambda to put
     the execution function name together. */
  pre.src-bash:before  { content: 'bash'; }
  pre.src-csh:before  { content: 'csh'; }
  pre.src-ash:before  { content: 'ash'; }
  pre.src-dash:before  { content: 'dash'; }
  pre.src-ksh:before  { content: 'ksh'; }
  pre.src-mksh:before  { content: 'mksh'; }
  pre.src-posh:before  { content: 'posh'; }
  /* Additional Emacs modes also supported by the LaTeX listings package */
  pre.src-ada:before { content: 'Ada'; }
  pre.src-asm:before { content: 'Assembler'; }
  pre.src-caml:before { content: 'Caml'; }
  pre.src-delphi:before { content: 'Delphi'; }
  pre.src-html:before { content: 'HTML'; }
  pre.src-idl:before { content: 'IDL'; }
  pre.src-mercury:before { content: 'Mercury'; }
  pre.src-metapost:before { content: 'MetaPost'; }
  pre.src-modula-2:before { content: 'Modula-2'; }
  pre.src-pascal:before { content: 'Pascal'; }
  pre.src-ps:before { content: 'PostScript'; }
  pre.src-prolog:before { content: 'Prolog'; }
  pre.src-simula:before { content: 'Simula'; }
  pre.src-tcl:before { content: 'tcl'; }
  pre.src-tex:before { content: 'TeX'; }
  pre.src-plain-tex:before { content: 'Plain TeX'; }
  pre.src-verilog:before { content: 'Verilog'; }
  pre.src-vhdl:before { content: 'VHDL'; }
  pre.src-xml:before { content: 'XML'; }
  pre.src-nxml:before { content: 'XML'; }
  /* add a generic configuration mode; LaTeX export needs an additional
     (add-to-list 'org-latex-listings-langs '(conf \" \")) in .emacs */
  pre.src-conf:before { content: 'Configuration File'; }

  table { border-collapse:collapse; }
  caption.t-above { caption-side: top; }
  caption.t-bottom { caption-side: bottom; }
  td, th { vertical-align:top;  }
  th.org-right  { text-align: center;  }
  th.org-left   { text-align: center;   }
  th.org-center { text-align: center; }
  td.org-right  { text-align: right;  }
  td.org-left   { text-align: left;   }
  td.org-center { text-align: center; }
  dt { font-weight: bold; }
  .footpara { display: inline; }
  .footdef  { margin-bottom: 1em; }
  .figure { padding: 1em; }
  .figure p { text-align: center; }
  .inlinetask {
    padding: 10px;
    border: 2px solid gray;
    margin: 10px;
    background: #ffffcc;
  }
  #org-div-home-and-up
   { text-align: right; font-size: 70%; white-space: nowrap; }
  textarea { overflow-x: auto; }
  .linenr { font-size: smaller }
  .code-highlighted { background-color: #ffff00; }
  .org-info-js_info-navigation { border-style: none; }
  #org-info-js_console-label
    { font-size: 10px; font-weight: bold; white-space: nowrap; }
  .org-info-js_search-highlight
    { background-color: #ffff00; color: #000000; font-weight: bold; }
  .org-svg { width: 90%; }

"
  "Default style declarations for org epub")

(defvar org-epub-cover nil
  "EPUB cover html")
(defvar org-epub-cover-img nil
  "EPUB cover img")

(defvar org-epub-manifest nil
  "EPUB export manifest")

(defun org-epub-manifest-entry (filename type mimetype &optional source)
  (let ((lis (list :filename filename :type type :mimetype mimetype)))
    (when source
      (plist-put lis :source source))
    lis))

(defun org-epub-html-p (manifest-entry)
  (eq (plist-get manifest-entry :type) 'html))

(defun org-epub-style-p (manifest-entry)
  (eq (plist-get manifest-entry :type) 'style))

(defun org-epub-cover-p (manifest-entry)
  (eq (plist-get manifest-entry :type) 'cover))

(defun org-epub-coverimg-p (manifest-entry)
  (eq (plist-get manifest-entry :type) 'coverimg))

(defun org-epub-img-p (manifest-entry)
  (eq (plist-get manifest-entry :type) 'img))

(defun org-epub-manifest-all (pred)
  (remove-if-not pred org-epub-manifest))

(cl-defun org-epub-manifest-first (pred)
  (let ((val))
    (dolist (el org-epub-manifest val)
      (when (funcall pred el)
	(return-from org-epub-manifest-first el)))))
    

(defun org-epub-link (link desc info)
  (when (and (not desc) (org-export-inline-image-p link (plist-get info :html-inline-image-rules)))
    (org-epub-include-image link))
  (org-html-link link desc info))

(defun org-epub-include-image (link)
  (let* ((path (org-element-property :path link))
	 (number (cl-incf org-epub-image-counter))
	 (new-path (concat "./images/" (format "%i.%s" number (file-name-extension path)))))
    (when (eq number 1)
      (make-directory (concat org-epub-zip-dir "images/") t))
    (push new-path org-epub-image-list)
    (copy-file (concat default-directory path) (concat org-epub-zip-dir new-path) t)
    (org-element-put-property link :path new-path)))

(defun org-epub-nameify (str)
  (replace-regexp-in-string "^[-]*" "" (replace-regexp-in-string "[/\\.]" "-" str)))

(defun org-epub-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let* ((headlines-raw (org-export-collect-headlines info 2))
	 (headlines (mapcar (lambda (headline)
			      (list
			       (org-element-property :raw-value headline)
			       (org-element-property :level headline)
			       (org-export-get-reference headline info))) headlines-raw)))
    ;; options: uid (:epub-uid), title (:title), language (:language),
    ;; subject (:epub-subject), description (:epub-description), creator
    ;; (:creator), publisher, date (:date), rights (:epub-rights)
    (let ((uid (org-export-data (plist-get info :epub-uid) info))
	  (title (org-export-data (plist-get info :title) info))
	  (language (org-export-data (plist-get info :language) info))
	  (subject (org-export-data (plist-get info :epub-subject) info))
	  (description (org-export-data (plist-get info :epub-description) info))
	  (author (org-export-data (plist-get info :author) info))
	  (publisher (org-export-data (plist-get info :epub-publisher) info))
	  (date (org-export-data (plist-get info :date) info))
	  (rights (org-export-data (plist-get info :epub-rights) info)))
      ;; gen style.css
      (when (plist-get info :html-head-include-default-style)
	(with-current-buffer (find-file (concat org-epub-zip-dir "style.css"))
	  (insert org-epub-style-default)
	  (save-buffer 0)
	  (kill-buffer))))
      ;; maybe set toc-depth "2" to some dynamic value
      (with-current-buffer (find-file (concat org-epub-zip-dir "toc.ncx"))
	(erase-buffer)
	(insert
	 (org-epub-template-toc-ncx uid 2 title (org-epub-generate-toc-single headlines "body.html")))
	(save-buffer 0)
	(kill-buffer))
      (when (plist-get info :epub-cover)
	(let* ((cover-path (plist-get info :epub-cover))
	       (cover-type (file-name-extension cover-path))
	       (cover-img (create-image (expand-file-name cover-path)))
	       (cover-width (car (image-size cover-img t)))
	       (cover-height (cdr (image-size cover-img t)))
	       (cover-name (concat "cover." cover-type)))
	  (message cover-path)
	  (copy-file cover-path (concat org-epub-zip-dir cover-name) t)
	  (with-current-buffer (find-file (concat org-epub-zip-dir "cover.html"))
	    (erase-buffer)
	    (insert
	     (org-epub-template-cover cover-name cover-width cover-height))
	    (save-buffer 0)
	    (kill-buffer)
	    (setf org-epub-cover "cover.html")
	    (setf org-epub-cover-img cover-name))))
      (with-current-buffer (find-file (concat org-epub-zip-dir "content.opf"))
	(erase-buffer)
	(insert (org-epub-template-content-opf title language uid subject description author publisher date rights
					       (org-epub-gen-manifest (append
								       '(("body-html" "body.html" "application/xhtml+xml")
									 ("style-css" "style.css" "text/css"))
								       (mapcar
									(lambda (el)
									  (list (org-epub-nameify el)
										el
										(concat "image/" (file-name-extension el))))
									org-epub-image-list)))
					       (org-epub-gen-spine '(("body-html" . "body.html")))))
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
     (when (plist-get info :html-head-include-default-style)
       "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\"/>\n")
     (when (plist-get info :epub-style)
       (mapconcat
	#'(lambda (str)
	    (concat "<link rel=\"stylesheet\" type=\"text/css\" href=\"" str "\"/>\n"))
	(org-split-string (or (plist-get info :epub-style) " "))))
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

(defmacro org-epub--export-wrapper (outfile &rest body)
  `(let* ((outfile ,outfile)
	  (out-file-type (file-name-extension outfile))
	  (org-epub-image-counter 0)
	  (org-epub-image-list nil)
	  (org-epub-cover nil)
	  (org-epub-cover-img nil)
	  (org-epub-zip-dir (file-name-as-directory
			     (make-temp-file (format "%s-" out-file-type) t)))
	  (body ,@body))
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
     (org-epub-zip-it-up outfile '("body.html" "style.css") org-epub-zip-dir)
     (delete-directory org-epub-zip-dir t)
     (message "Generated %s" outfile)
     (expand-file-name outfile)))

;;compare org-export-options-alist
;;;###autoload
(defun org-epub-export-to-epub (&optional async subtreep visible-only ext-plist)
  "Export the current buffer to an EPUB file.

ASYNC defines wether this process should run in the background,
SUBTREEP supports narrowing of the document, VISIBLE-ONLY allows
you to export only visible parts of the document, EXT-PLIST is
the property list for the export process."
  (interactive)
  (let* ((outfile (org-export-output-file-name ".epub" subtreep)))
    (if async
	(org-export-async-start (lambda (f) (org-export-add-to-stack f 'odt))
	  (org-epub--export-wrapper
	   outfile
	   (org-export-as 'epub subtreep visible-only nil ext-plist)))
      (org-epub--export-wrapper
       outfile
       (org-export-as 'epub subtreep visible-only nil ext-plist)))))

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

(defun org-epub-template-content-opf (title language uid subject description creator publisher date rights manifest spine)
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
string with the list of html files in reading order."
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
      (when org-epub-cover
	"<meta name=\"cover\" content=\"cover-image\"/>")
      "
   </metadata>

   <manifest>\n"
      (when org-epub-cover
	(concat "<item id=\"cover\" href=\"cover.html\" media-type=\"application/xhtml+xml\"/>
         <item id=\"cover-image\" href=\"" org-epub-cover-img "\" media-type=\"" (concat "image/" (file-name-extension org-epub-cover-img)) "\"/>"))
      "<item id=\"ncx\"      href=\"toc.ncx\"
         media-type=\"application/x-dtbncx+xml\" />"
      manifest
      
   "</manifest>

   <spine toc=\"ncx\">"
   (when org-epub-cover
     "<itemref idref=\"cover\" linear=\"no\" />")
   
   spine

   "</spine>

 <guide>"
   (when org-epub-cover
     " <reference type=\"cover\" href=\"" org-epub-cover "\" />")
   "
 </guide>

</package>"))

(defun org-epub-gen-manifest (files)
  "Generate the manifest XML string.

FILES is the list of files to be included in the manifest."
  (mapconcat
   (lambda (file)
     (concat "<item id=\"" (car file) "\"      href=\"" (second file) "\"
            media-type=\"" (third file) "\" />\n"))
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

(defun org-epub-zip-it-up (epub-file files target-dir)
  "Create the .epub file by zipping up the contents.

EPUB-FILE is the target filename, FILES is the list of source
files to process, while TARGET-DIR is the directory where
exported HTML files live.  "
  (let ((default-directory target-dir)
	(meta-files '("META-INF/container.xml" "content.opf" "toc.ncx")))
    (call-process "zip" nil '(:file "zip.log") nil
		  "-Xu0"
		  epub-file
		  "mimetype")
    (apply 'call-process "zip" nil '(:file "zip.log") nil
	   "-Xu9"
	   epub-file
	   (append meta-files files org-epub-image-list
		   (when org-epub-cover (list org-epub-cover org-epub-cover-img)))))
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
		(level (nth 1 headline))
		(ref (nth 2 headline)))
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
	    (concat (format "<navPoint class=\"h%d\" id=\"%s-%d\">\n" current-level filename toc-id)
		    (format "<navLabel><text>%s</text></navLabel>\n" (org-html-encode-plain-text title))
		    (format "<content src=\"%s#%s\"/>" filename ref)))))
       headlines)
      (while (> current-level 0)
	(princ "</navPoint>")
	(cl-decf current-level)))))

(provide 'ox-epub)

;;; ox-epub.el ends here
