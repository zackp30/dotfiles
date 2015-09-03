;;; bbdbV3-wl.el --- 
;; 
;; Filename: bbdbV3-wl.el
;; Description: 
;; Author: Christian Nelson Giménez
;; Maintainer: 
;; Created: vie oct  7 11:28:06 2011 (-0300)
;; Version: 
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; I decide to make a bbdb-wl interface from scratch.
;; This decition cames because BBDB V3.x changes a lot from his last. 
;; Almost everything for e-mails was implemented, and every function 
;; has changed their names or even does not exist anymore.
;;
;; So, there's another version for this BBDB and Wanderlust 2011.
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:


;; This is from bbdb-wl for BBDB V2.x
(eval-and-compile
  (add-hook 'wl-message-redisplay-hook 'bbdb-wl-get-update-record)
;;   (add-hook 'wl-summary-exit-hook 'bbdb-wl-hide-bbdb-buffer)
;;   (add-hook 'wl-message-window-deleted-hook 'bbdb-wl-hide-bbdb-buffer)
;;   (add-hook 'wl-exit-hook 'bbdb-wl-exit)
;;   (add-hook 'wl-save-hook '(bbdb-save t))
;;   (add-hook 'wl-summary-toggle-disp-off-hook 'bbdb-wl-hide-bbdb-buffer)
;;   (add-hook 'wl-summary-toggle-disp-folder-on-hook 'bbdb-wl-hide-bbdb-buffer)
;;   (add-hook 'wl-summary-toggle-disp-folder-off-hook 'bbdb-wl-hide-bbdb-buffer)
;;   (add-hook 'wl-summary-toggle-disp-folder-message-resumed-hook
;;             'bbdb-wl-show-bbdb-buffer)
;;   (add-hook 'wl-summary-mode-hook
;;             (function
;;              (lambda ()
;;                (define-key (current-local-map) ":" 'bbdb-wl-show-sender)
;;                (define-key (current-local-map) ";" 'bbdb-wl-edit-notes))))
;;   (add-hook 'wl-summary-exit-hook 'bbdb-flush-all-caches)
;;   (add-hook 'wl-summary-exec-hook 'bbdb-flush-all-caches)
;;   (add-hook 'wl-mail-setup-hook
;;             (function
;;              (lambda ()
;; ;;;            (local-set-key "\M-\t" 'bbdb-complete-name)
;;                (define-key (current-local-map) "\M-\t" 'bbdb-complete-name))))
  (define-key mime-view-mode-default-map ":" 'bbdb-wl-get-update-record)
  )

(require 'bbdb)
(require 'bbdb-com)
(require 'wl)

(defun bbdb-wl-get-sender-data ()
  "Get all the data located in the header about the sender."
  (save-excursion
    (goto-char 0)
    (search-forward-regexp "^From:[[:blank:]]*" nil t) 
    (search-forward-regexp "[^[:blank:]].*$" nil t)
    (match-string-no-properties 0)
    )
  )

(defun bbdb-wl-get-sender-name ()
  "Return the sender name(just the name)"
  ;; Search for sender mail...
  (let ((usrmail (bbdb-wl-get-sender-data)))
    ;; Extract real name
    (wl-address-header-extract-realname usrmail)
    )
  )

(defun bbdb-wl-get-sender-address ()
  "Return the sender petname(just that)"
  (let ((usrmail (bbdb-wl-get-sender-data)))
    (wl-address-header-extract-address usrmail)
    )
  )

(defun bbdb-wl-find-and-show (name)
  "Look for the name in the BBDB and show it! 
Return the records of the BBDB found.
If no records are found, just return nil.

If name is an empty string, to avoid showing all the people in BBDB this function will return nil."
  (if (string= name "")      
      (progn
	(bbdb-display-records nil) ;; Display nothing, erase last display.
	nil
	)
    (progn
      (setq records (bbdb-search (bbdb-records) name))
      (bbdb-display-records records)
      records
      )
    )
  )

(defun bbdb-wl-get-update-record ()
  "Function ideally of `wl-message-redisplay-hook'.
Find all data from the sender and reciever(the 'get' part) and query to update if necessary(the 'update' part)."  
  (interactive)

  ;; Find names!
  (let ((sender-name (bbdb-wl-get-sender-name)))
    ;; is it in BBDB?    
    (setq records (bbdb-wl-find-and-show sender-name))
    (if records
	;; yes, exists...	
	;; should we update it?
	(bbdb-wl-check-update record)
      ;;Nop, doesn't exists...
      (progn
	(bbdb-wl-query-create-sender)
	(bbdb-wl-find-and-show sender-name)
	)
      )
    )
  )

(defun bbdb-wl-check-update (record)
  "Check if this BBDB record and the information in the mail are different.
If they are, query the user if she/he want to update the BBDB record.
If she/he wants, update it.
If not, well, do nothing!"
  ;; TODO
  )

(defun bbdb-wl-query-create-sender ()
  "Query the user if we can create the sender. If we can, create the sender with its data.
If we cannot... just ignore this mail."
  (when (y-or-n-p (format "Record %s doesn't exists. Create it?" (bbdb-wl-get-sender-name)))
    ;; Answers "y"... create it.
    (bbdb-wl-create-sender)
    )	       
  )

(defun bbdb-wl-create-sender ()
  "Create the sender BBDB record and add it to BBDB."
  ;; Avoid duplication!
  ;;(let ((bbdb-no-duplicates t))
    (if
	(bbdb-create-internal (bbdb-wl-get-sender-name) ;; name
			      nil ;; affix
			      nil ;; aka
			      nil ;; organizations
			      (cons (bbdb-wl-get-sender-address) nil);; mail
			      nil ;; phones
			      nil ;; addresses 
			      nil ;; notes
			      )
	(message "%s" "Record Added: Done")
      (message "%s" "Record not added... Possibly it is already in the database")
      )
	
  ;;  )
  )


					; ******************************
					; ** Addressbook

(defun bbdb-wl-find-name (name)
  "Find this name in the BBDB. 
If it does exists return the record.
If nothing founds, return nil."
  (bbdb-search (bbdb-records) name)
  )

(defun bbdb-wl-sinchronize-addressbook ()
  "Sinchronize BBDB with Wanderlust's Addressbook."
  (interactive)
  ;; We need that the address buffer has been visited...
  (let ((buff (get-buffer "Address")))
    (when buff      
      ;; it has been visited...
	(with-current-buffer buff
	  ;; For each item in the addressbook add it into the BBDB(if don't exists)
	  (dolist (person wl-addrmgr-list)
	    (unless (bbdb-wl-find-name (nth 2 person))
	      ;; It doesn't exists... create it
	      (message "%s %s %s %s" "Adding into BBDB: " (nth 0 person) (nth 1 person) (nth 2 person))
	      (bbdb-create-internal (nth 2 person) ;; name
				    nil ;; affix
				    (cons (nth 1 person) nil) ;; aka
				    nil ;; organizations
				    (cons (nth 0 person) nil);; mail
				    nil ;; phones
				    nil ;; addresses 
				    nil ;; notes
				    )
	      )
	    )
	  )
	)
    )
  )


(provide 'bbdbV3-wl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bbdbV3-wl.el ends here
