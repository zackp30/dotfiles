;;; ox-cv.el --- CV moderncv Back-End for Org Export Engine

;; Copyright (C) 2007-2012  Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <n.goaziou AT gmail DOT com>
;;         Alan Schmitt <alan.schmitt AT polytechnique DOT org>
;;         Viktor Rosenfeld <listuser36 AT gmail DOT com>
;;         Rasmus Pank Roulund <emacs AT pank DOT eu>
;;         Myles English <myles AT rockhead DOT biz>
;; Keywords: org, wp, tex

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This library implements a CV moderncv back-end, derived from the
;; LaTeX one.
;;
;; Depending on the desired output format, three commands are provided
;; for export: `org-cv-export-as-latex' (temporary buffer),
;; `org-cv-export-to-latex' ("tex" file) and
;; `org-cv-export-to-pdf' ("pdf" file).
;;
;; On top of buffer keywords supported by `latex' back-end (see
;; `org-latex-options-alist'), this back-end introduces the following
;; keywords:
;;   - "FIRSTNAME" (see `org-cv-firstname'),
;;   - "FAMILYNAME" (see `org-cv-familyname')

;; `org-cv-special-tags-in-cv', namely "to" and "from".
;; LaTeX line breaks are not necessary if using these headings.  If
;; both a headline and a keyword specify a to or from address the
;; value is determined in accordance with
;; `org-cv-prefer-special-headings'.
;;
;; A number of OPTIONS settings can be set to change which contents is
;; exported.
;;   - email (see `org-cv-use-email')
;;
;; The following variables works differently from the main LaTeX class
;;   - "AUTHOR": default to user-full-name but may be disabled.  (see org-cv-author),
;;   - "EMAIL": same as AUTHOR, (see org-cv-email),
;;
;; Headlines are in general ignored.  However, headlines with special
;; tags can be used for specified contents.
;;
;; You will need to add an appropriate association in
;; `org-latex-classes' in order to use the CV class.
;; The easiest way to do this is by adding
;;
;;   (eval-after-load "ox-cv"
;;   '(org-cv-plug-into-ox))
;;
;; to your init file. This will add a very sparse moderncv class and
;; set it as the default `org-cv-latex-default-class'.  You can also
;; add you own letter class.  For instace:
;;
;; (require 'ox-cv)  
;; (add-to-list 'org-latex-classes
;;             '("mymoderncv"
;;               "\\documentclass\{mymoderncv\}
;; \\moderncvtheme[grey]{classic}  
;; \[NO-DEFAULT-PACKAGES]  
;; \[NO-PACKAGES]
;; \[EXTRA]"))

;; (add-to-list 'org-export-before-parsing-hook
;;              'ox-cv-export-parse-employment)
;;
;; Then, in your Org document, be sure to require the proper class
;; with :
;;
;;    #+LATEX_CLASS: mymoderncv
;;
;; Or by setting `org-cv-default-class'.

;;; Code:

(require 'ox-latex)


;;; User-Configurable Variables

(defgroup org-export-cv nil
  "Options for exporting to CV moderncv class in LaTeX export."
  :tag "Org CV"
  :group 'org-export)

(defcustom org-cv-author 'user-full-name
  "The sender's name.

This variable defaults to calling the function `user-full-name'
which just returns the current `user-full-name'.  Alternatively a
string, nil or a function may be given. Functions must return a
string."
  :group 'org-export-cv
  :type '(radio (function-item user-full-name)
		(string)
		(function)
		(const :tag "Do not export author" nil)))

(defun org-cv-firstname-fun ()
  "The sender's first name."
  (nth 0 (split-string (user-full-name) " ")))

(defcustom org-cv-firstname 'org-cv-firstname-fun
  "The sender's first name.

This variable defaults to calling the function `user-full-name'
which just returns the current `user-full-name'.  Alternatively a
string, nil or a function may be given. Functions must return a
string."
  :group 'org-export-cv
  :type '(radio (function-item org-cv-firstname-fun)
		(string)
		(function)
		(const :tag "Do not export firstname" nil)))

(defcustom org-cv-familyname '(nth 1 (split-string (user-full-name) " "))
  "The sender's first name.

This variable defaults to calling the function `user-full-name'
which just returns the current `user-full-name'.  Alternatively a
string, nil or a function may be given. Functions must return a
string."
  :group 'org-export-cv
  :type '(radio (function-item (nth 1 (split-string (user-full-name) " ")))
		(string)
		(function)
		(const :tag "Do not export familyname" nil)))

(defcustom org-cv-email 'org-cv-email
  "The sender's email address.

This variable defaults to the value `org-cv-email' which
returns `user-mail-address'.  Alternatively a string, nil or a
function may be given.  Functions must return a string."
  :group 'org-export-cv
  :type '(radio (function-item org-cv-email)
		(string)
		(function)
		(const :tag "Do not export email" nil)))

(defcustom org-cv-use-email t
  "Print sender's email address."
  :group 'org-export-cv
  :type 'boolean)

(defcustom org-cv-default-class nil
  "Default class for `org-cv'.  Must be a member of
  `org-latex-classes'"
  :group 'org-export-cv
  :type 'string)

(defconst org-cv-special-tags-in-cv '(education projects)
  "header tags related to the cv itself")

(defvar org-cv-special-contents nil
  "holds special content temporarily.")

(defcustom org-cv-title-command "\\makecvtitle"
  "The command used to insert the title just after \\begin{document}.
If this string contains the formatting specification \"%s\" then
it will be used as a formatting string, passing the title as an
argument."
  :group 'org-export-cv
  :type 'string)



;;; Define Back-End

(org-export-define-derived-backend 'cv 'latex
  :options-alist
  '((:latex-class "LATEX_CLASS" nil (if org-cv-default-class
					org-cv-default-class
					org-latex-default-class) t)
    (:author "AUTHOR" nil (org-cv--get-custom org-cv-author) t)
    (:firstname "FIRSTNAME" nil (org-cv--get-custom org-cv-firstname) t)
    (:familyname "FAMILYNAME" nil (org-cv--get-custom org-cv-familyname) t)
    (:email "EMAIL" nil (org-cv--get-custom org-cv-email) t)
    (:with-email nil "email" org-cv-use-email)
    (:special-tags nil nil (append
			    org-cv-special-tags-in-cv)))
  :translate-alist '((export-block . org-cv-export-block)
		     (export-snippet . org-cv-export-snippet)
		     ;;(headline . org-cv-headline)
		     (keyword . org-cv-keyword)
		     (template . org-cv-template)
		     (inline-src-block . nil) ;;org-cv-blank)
		     (src-block . nil)) ;;org-cv-blank)
  :menu-entry
  '(?m "Export with mymoderncv"
       ((?L "As LaTeX buffer" org-cv-export-as-latex)
	(?l "As LaTeX file" org-cv-export-to-latex)
	(?p "As PDF file" org-cv-export-to-pdf)
	(?o "As PDF file and open"
	    (lambda (a s v b)
	      (if a (org-cv-export-to-pdf t s v b)
		(org-open-file (org-cv-export-to-pdf nil s v b))))))))



;;; Initialize class function

(defun org-cv-plug-into-ox ()
  "Add a sparse `default-cv' to `org-latex-classes' and set
`org-cv-default-class' to `default-cv'"
  (let ((class "default-cv"))
    (eval-after-load "ox-latex"
      '(unless (member ,class 'org-latex-classes)
	 (add-to-list 'org-latex-classes
		      `(,class
			"\\documentclass[11pt]{moderncv}") ())
	 (setq org-cv-default-class class)))))

;;; Helper functions

(defun org-cv-email ()
  "Return the current `user-mail-address'"
  user-mail-address)

;; The following is taken from/inspired by ox-grof.el
;; Thanks, Luis!

(defun org-cv--get-tagged-contents (key)
  "Get tagged content from `org-cv-special-contents'"
  (cdr (assoc (org-cv--get-custom key)
	      org-cv-special-contents)))

(defun org-cv--get-custom (value)
  "Determines whether a value is nil, a string or a
function (a symbol).  If it is a function it it evaluates it."
  (when value
    (cond ((stringp value) value)
	  ((functionp value) (funcall value))
	  ((symbolp value) (symbol-name value))
	  (t value))))


(defun org-cv--prepare-special-contents-as-macro (a-list &optional keep-newlines no-tag)
  "Finds all the components of `org-cv-special-contents'
corresponding to members of the `a-list' and return them as a
string to be formatted.  The function is used for inserting
content of special headings such as PS.

If keep-newlines is t newlines will not be removed.  If no-tag is
is t the content in `org-cv-special-contents' will not
be wrapped in a macro named whatever the members of a-list are called.
"
  (let (output)
    (dolist (ac* a-list output)
      (let*
	  ((ac (org-cv--get-custom ac*))
	   (x (org-cv--get-tagged-contents ac)))
	(when x
	  (setq output
		(concat
		 output "\n"
		 ;; sometimes LaTeX complains about newlines
		 ;; at the end or beginning of macros.  Remove them.
		 (org-cv--format-string-as-macro
		  (if keep-newlines x (org-cv--remove-offending-new-lines x))
		  (unless no-tag  ac)))))))))

(defun org-cv--format-string-as-macro (string &optional macro)
  "If a macro is given format as string as  \"\\macro{string}\" else as \"string\""
  (if macro
      (format "\\%s{%s}" macro string)
    (format "%s" string)))

(defun org-cv--remove-offending-new-lines (string)
  "Remove new lines in the begging and end of `string'"
  (replace-regexp-in-string "\\`[ \n\t]+\\|[\n\t ]*\\'" "" string))

;;; Transcode Functions

;;;; Export Block

(defun org-cv-export-block (export-block contents info)
  "Transcode an EXPORT-BLOCK element into CV code.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (when (member (org-element-property :type export-block) '("CV" "LATEX"))
    (org-remove-indentation (org-element-property :value export-block))))

;;;; Export Snippet

(defun org-cv-export-snippet (export-snippet contents info)
  "Transcode an EXPORT-SNIPPET object into CV Scrlttr2 code.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (when (memq (org-export-snippet-backend export-snippet) '(latex cv))
    (org-element-property :value export-snippet)))

;;;; Keyword

(defun org-cv-keyword (keyword contents info)
  "Transcode a KEYWORD element into CV Scrlttr2 code.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((key (org-element-property :key keyword))
	(value (org-element-property :value keyword)))
    ;; Handle specifically BEAMER and TOC (headlines only) keywords.
    ;; Otherwise, fallback to `latex' back-end.
    (if (equal key "CV") value
      (org-export-with-backend 'latex keyword contents info))))


;; Headline
(defun ox-cv-timestamp-to-shortdate (date_str)
  "e.g. <2002-08-12 Mon> => Aug 2012"
  (let* ((abbreviate 't)
	 (dte (org-parse-time-string date_str))
	 (month (nth 4 dte))
	 (year (nth 5 dte)));;'(02 07 2015)))
    (concat (calendar-month-name month abbreviate)
	    " "
	    (number-to-string year))))

(defun ox-cv-export-parse-employment (backend)
  "Parses a section containing headlines that represent positions of
 employment"
  (save-excursion
    (save-restriction
      (dolist
	  (hl (org-element-map (org-element-parse-buffer 'headline)
		  'headline
		'identity))
	(when (member "employment" (org-element-property :tags hl))
	  (goto-char (org-element-property :begin hl))
	  (org-narrow-to-subtree)
	  ;; TODO: remove first headline
	  (let ((jobs (org-element-parse-buffer))
		employ)
	    (delete-region (point-min) (point-max))
	    (insert "* Employment\n")
	    (dolist
		(job (org-element-map jobs 'headline 'identity))
	      ;;TODO
	      (if (org-element-property :POSITION job)  ;; its a job section
	      (let (
		    para
		    role
		    (employ
			 (format "\\cventry{%s -- %s}{%s}{%s}{%s}{}{}\n"
				 (ox-cv-timestamp-to-shortdate
				  (or (org-element-property :DATE_FROM job)
				      (org-element-property :DATE_FROM_EST job)))
				 (ox-cv-timestamp-to-shortdate
				  (or (org-element-property :DATE_TO job)
				      (org-element-property :DATE_TO_EST job)))
				 (org-element-property :POSITION job)
				 (org-element-property :EMPLOYER job)
				 (org-element-property :LOCATION job))
		    ))
		;; Role and responsibilities
		(dolist
		    (para (org-element-contents
			   (org-element-map job 'paragraph 'identity)))
		  (setq role (substring-no-properties
			      (car (org-element-contents para)))))
		;;  Replace employment section with parsed section
		(insert employ)
		(widen)
		) ;; let
	      ) ;; if
	      )))))))

(defun org-tbl-to-cv-entries (tbl)
  "Converts a table like this into \\cventry:
#+name:education_tbl
| from |   to | qual             | uni                | country | grade| thesis      |
|------+------+------------------+--------------------+---------+------+-------------|
| 2008 | 2013 | PhD              | Uni of Life        | UK      |      | Thesis title: ``Things''      |
| 2000 | 2001 | MSc Common Sense | Uni of Hard Knocks | UK      |      | Thesis title: ``Obviousity''  |
"
  (     (setq result "")
	(setq ljobs (nth 0 (list tbl)))
	;;     (pop ljobs)
	;;   (pop ljobs)
	(dolist (elt ljobs result)
       (progn
        (setq date_from (nth 0 elt))
         (setq date_to (nth 1 elt))
        (setq position (nth 2 elt))
        (setq employer (nth 3 elt))
        (setq city (nth 4 elt))
(setq date_from (cond ((numberp date_from) (number-to-string date_from))
                      ((stringp date_from) date_from)))
(setq date_to (cond ((numberp date_to) (number-to-string date_to))
                    ((stringp date_to) date_to)))
        (setq result (concat
         result
         "\\cventry{"
         date_from
         " -- "
         date_to "}{"
         position  "}{"
         employer "}{"
city  "}{}{}\n")))
     )
    (print result)
))

(defun org-cv-headline (headline contents info)
  "Transcode a HEADLINE element from Org to LaTeX.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information.

Note that if a headline is tagged with a tag from
`org-cv-special-tags-in-cv' it will not be exported, but
stored in `org-cv-special-contents' and included at the
appropriate place."
  (unless (let ((tag (car (org-export-get-tags headline info))))
	    (and tag
		 (member-ignore-case
		  tag (mapcar #'symbol-name (plist-get info :special-tags)))
		 ;; Store association for later use and bail out.
		 (push (cons tag contents) org-cv-special-contents)))
    
    ;; In any case, insert contents in cv's body.
    contents))

;;;; Template

(defun org-cv-template (contents info)
  "Return complete document string after LaTeX conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((title (org-export-data (plist-get info :title) info)))
    (concat
     ;; Time-stamp.
     (and (plist-get info :time-stamp-file)
	  (format-time-string "%% Created %Y-%m-%d %a %H:%M\n"))
     ;; Document class and packages.
     (let* ((class (plist-get info :latex-class))
	    (class-options (plist-get info :latex-class-options))
	    (header (nth 1 (assoc class org-latex-classes)))
	    (document-class-string
	     (and (stringp header)
		  (if (not class-options) header
		    (replace-regexp-in-string
		     "^[ \t]*\\\\documentclass\\(\\(\\[[^]]*\\]\\)?\\)"
		     class-options header t nil 1)))))
       (if (not document-class-string)
	   (user-error "Unknown LaTeX class `%s'" class)
	 (org-latex-guess-babel-language
	  (org-latex-guess-inputenc
	   (org-element-normalize-string
	    (org-splice-latex-header
	     document-class-string
	     org-latex-default-packages-alist
	     org-latex-packages-alist nil
	     (concat (org-element-normalize-string
		      (plist-get info :latex-header))
		     (plist-get info :latex-header-extra)))))
	  info)))
     ;; Possibly limit depth for headline numbering.
     (let ((sec-num (plist-get info :section-numbers)))
       (when (integerp sec-num)
	 (format "\\setcounter{secnumdepth}{%d}\n" sec-num)))
     ;; Firstname and Familyname
     (let ((author (let ((auth (plist-get info :author)))
			  (and auth (org-export-data auth info))))
	   (firstname (org-export-data (plist-get info :firstname) info))
	   (familyname (org-export-data (plist-get info :familyname) info)))
       (format "\\firstname{%s}\\familyname{%s}\n" firstname familyname))
     ;; Author.
     (let ((author (and (plist-get info :with-author)
			(let ((auth (plist-get info :author)))
			  (and auth (org-export-data auth info)))))
	   (email (and (plist-get info :with-email)
		       (org-export-data (plist-get info :email) info))))
       (cond ((and author email (not (string= "" email)))
	      (format "\\author{%s\\thanks{%s}}\n" author email)) ;; FIXME: not needed for moderncv
	     ((or author email) (format "\\author{%s}\n" (or author email))))
       (cond ((and email (not (string= "" email)))
	      (format "\\email{%s}\n" email))))
     ;; Date.
     (let ((date (and (plist-get info :with-date) (org-export-get-date info))))
       (format "\\date{%s}\n" (org-export-data date info)))
     ;; Title
     (format "\\title{%s}\n" title)
     ;; Hyperref options.
;;     (when (plist-get info :latex-hyperref-p)
;;       (format "\\hypersetup{\n  pdfkeywords={%s},\n  pdfsubject={%s},\n  pdfcreator={%s}}\n"
;;	       (or (plist-get info :keywords) "")
;;	       (or (plist-get info :description) "")
;;	       (if (not (plist-get info :with-creator)) ""
;;		 (plist-get info :creator))))
     ;; Document start.
     "\\begin{document}\n\n"
     ;; Title command.
     (org-element-normalize-string
      (cond ((string= "" title) nil)
	    ((not (stringp org-cv-title-command)) nil)
	    ((string-match "\\(?:[^%]\\|^\\)%s"
			   org-cv-title-command)
	     (format org-cv-title-command title))
	    (t org-cv-title-command)))
     ;; Document's body.
     contents
     ;; Creator.
     (let ((creator-info (plist-get info :with-creator)))
       (cond
	((not creator-info) "")
	((eq creator-info 'comment)
	 (format "%% %s\n" (plist-get info :creator)))
	(t (concat (plist-get info :creator) "\n"))))
     ;; Document end.
     "\\end{document}")))



;;; Commands

;;;###autoload
(defun org-cv-export-as-latex
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a CV letter.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{letter}\" and \"\\end{letter}\".

EXT-PLIST, when provided, is a proeprty list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org CV Export*\".  It
will be displayed if `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (let (org-cv-special-contents)
  (if async
      (org-export-async-start
	  (lambda (output)
	    (with-current-buffer (get-buffer-create "*Org CV Export*")
	      (erase-buffer)
	      (insert output)
	      (goto-char (point-min))
	      (LaTeX-mode)
	      (org-export-add-to-stack (current-buffer) 'cv)))
	`(org-export-as 'cv ,subtreep ,visible-only ,body-only
			',ext-plist))
    (let ((outbuf (org-export-to-buffer
		   'cv "*Org CV Export*"
		   subtreep visible-only body-only ext-plist)))
      (with-current-buffer outbuf (LaTeX-mode))
      (when org-export-show-temporary-export-buffer
	(switch-to-buffer-other-window outbuf))))))

;;;###autoload
(defun org-cv-export-to-latex
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a CV Scrlttr2 letter (tex).

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
between \"\\begin{letter}\" and \"\\end{letter}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

When optional argument PUB-DIR is set, use it as the publishing
directory.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep))
	org-cv-special-contents)
    (if async
	(org-export-async-start
	    (lambda (f) (org-export-add-to-stack f 'cv))
	  `(expand-file-name
	    (org-export-to-file
	     'cv ,outfile ,subtreep ,visible-only ,body-only
	     ',ext-plist)))
      (org-export-to-file
       'cv outfile subtreep visible-only body-only ext-plist))))

;;;###autoload
(defun  org-cv-export-to-pdf
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a CV (pdf).

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
between \"\\begin{letter}\" and \"\\end{letter}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return PDF file's name."
  (interactive)
  (if async
      (let ((outfile (org-export-output-file-name ".tex" subtreep)))
	(org-export-async-start
	    (lambda (f) (org-export-add-to-stack f 'cv))
	  `(expand-file-name
	    (org-latex-compile
	     (org-export-to-file
	      'cv ,outfile ,subtreep ,visible-only ,body-only
	      ',ext-plist)))))
    (org-latex-compile
     (org-cv-export-to-latex
      nil subtreep visible-only body-only ext-plist))))


(provide 'ox-cv)
;;; ox-cv.el ends here
