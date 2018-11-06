;;; orgmode-pack.el --- org-mode configuration (todo with org, slide presentation, etc...)

;;; Commentary:

;;; Code:

(require 'ac-math)
(require 'ert)
(require 'smartscan)
(require 'whitespace)
(require 'dash-functional)

(require 'org)
(add-hook 'org-mode-hook (lambda ()
			   (smartscan-mode-turn-on)
			   (column-number-mode)))
(add-hook 'org-mode-hook (lambda ()
			   (when (require 'whitespace nil t)
			     (whitespace-turn-off))))
(custom-set-variables '(org-directory "~/org")
		      '(org-agenda-files (directory-files org-directory 'absolute-names ".org$" 'nosort))
		      '(org-startup-indented t)
		      '(org-log-done 'time)
		      '(org-default-notes-file (concat org-directory "/notes.org"))
		      '(org-export-with-toc t)
		      '(org-export-headline-levels 4)
		      ;; metadata tags for the task at end
		      '(org-tag-alist '(("howto"       . ?h)
					("tech"        . ?t)
					("emacs"       . ?e)
					("orgmode"     . ?o)
					("faq"         . ?F)
					("linux"       . ?l)
					("dev"         . ?d)
					("clojure"     . ?c)
					("elisp"       . ?E)
					("common-lisp" . ?C)
					("haskell"     . ?H)
					("scala"       . ?s)
					("devops"      . ?d)
					("TOC"         . ?T))) ;; for org-toc
		      ;; keywords sequence for org-mode
		      '(org-todo-keywords
			'((sequence "TODO(t)" "IN-PROGRESS(i)" "PENDING(p)" "|"  "DONE(d)" "FAILED(f)" "DELEGATED(e)" "CANCELLED(c)")))
		      ;; modifying the color for the different keywords
		      '(org-todo-keyword-faces
			'(("TODO"        . (:foreground "firebrick2" :weight bold))
			  ("IN-PROGRESS" . (:foreground "olivedrab" :weight bold))
			  ("PENDING"     . (:foreground "sienna" :weight bold))
			  ("DONE"        . (:foreground "forestgreen" :weight bold))
			  ("DELEGATED"   . (:foreground "dimgrey" :weight bold))
			  ("FAILED"      . (:foreground "steelblue" :weight bold))
			  ("CANCELLED"   . shadow)))
		      '(org-fontify-done-headline t))

(defun orgmode-pack-mode-log (&rest msg-data)
  "Log MSG-DATA with specific pack prefix."
  (apply #'message (format "Orgmode Pack - %s" (car msg-data)) (cdr msg-data)))

(defun orgmode-pack-org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states) ;; turn off logging
    (org-todo (if (= n-not-done 0) 'done 'previousset))))

(add-hook 'org-after-todo-statistics-hook 'orgmode-pack-org-summary-todo)

;; babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((haskell    . t)
   (emacs-lisp . t)
   (sh         . t)
   (clojure    . t)
   (java       . t)
   (ruby       . t)
   (perl       . t)
   (python     . t)
   (R          . t)
   (ditaa      . t)
   (lilypond   . t)))

  (custom-set-faces
   '(org-done ((t (:foreground "PaleGreen"
                               :weight normal
                               :strike-through t))))
   '(org-headline-done
     ((((class color) (min-colors 16) (background dark))
       (:foreground "LightSalmon" :strike-through t)))))

(defun orgmode-pack-update-parent-cookie ()
  "Update Org-mode statistics."
  (when (equal major-mode 'org-mode)
    (save-excursion
      (ignore-errors
	(org-back-to-heading)
	(org-update-parent-todo-statistics)))))

(defadvice org-kill-line (after fix-cookies activate)
  "Add advice around the org-kill-line method."
  (orgmode-pack-update-parent-cookie))

(defadvice kill-whole-line (after fix-cookies activate)
  "Same for `kill-whole-line`.
AFTER killing whole line, update the org-mode's current statistics.
FIX-COOKIES.
ACTIVATE."
  (orgmode-pack-update-parent-cookie))

;;;;;;;;; Math setup

;; there is trouble with the standard install so I use directly emacs-live's native api
(require 'ac-math)

;; adding the auto-complete mode to org
(add-to-list 'ac-modes 'org-mode)

(defun ac-latex-mode-setup ()
  "Add ac-sources to default ac-sources."
  (setq ac-sources
	(append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
		ac-sources)))

(add-hook 'org-mode-hook 'ac-latex-mode-setup)

(add-hook 'org-mode-hook
	  (lambda ()
	    (global-unset-key (kbd "C-c o"))
	    (global-unset-key (kbd "C-c t"))
	    (global-unset-key (kbd "C-c o c"))
	    (global-unset-key (kbd "C-c o l"))
	    (global-unset-key (kbd "C-c o a"))
	    (global-unset-key (kbd "C-c o t"))

	    (define-key org-mode-map (kbd "C-c o c") 'org-capture)
	    (define-key org-mode-map (kbd "C-c o l") 'org-store-link)
	    (define-key org-mode-map (kbd "C-c o a") 'org-agenda)
	    (define-key org-mode-map (kbd "C-c o t") 'org-todo)
	    (define-key org-mode-map (kbd "C-c o b") 'org-iswitchb)

	    ;; org-mode
	    (define-key org-mode-map (kbd "C-M-f") 'org-metadown)
	    (define-key org-mode-map (kbd "C-M-b") 'org-metaup)
	    (define-key org-mode-map (kbd "C-M-l") 'org-shiftright)
	    (define-key org-mode-map (kbd "C-M-j") 'org-shiftleft)
	    (define-key org-mode-map (kbd "C-M-i") 'org-shiftup)
	    (define-key org-mode-map (kbd "C-M-k") 'org-shiftdown)))

(add-to-list 'auto-mode-alist '("\.org$"  . org-mode))
(add-to-list 'auto-mode-alist '("\.todo$" . org-mode))
(add-to-list 'auto-mode-alist '("\.note$" . org-mode))

(defcustom orgmode-pack-yt-html-iframe-width 440 "Width for the youtube's iframe.")
(defcustom orgmode-pack-yt-html-iframe-height 335 "Height for the youtube's iframe.")
(defconst orgmode-pack-yt-html-iframe-format
  "<iframe width=\"%s\" height=\"%s\" src=\"https://www.youtube.com/embed/%s\" frameborder=\"0\" allowfullscreen>%s</iframe>")
(defconst orgmode-pack-yt-latex-iframe-format
  "\href{%s}{%s}")

(org-add-link-type "yt"
		   (lambda (handle)
		     (browse-url (concat "https://www.youtube.com/embed/" handle)))
		   (lambda (path desc backend)
		     (cl-case backend
		       (html (format orgmode-pack-yt-html-iframe-format
				     orgmode-pack-yt-html-iframe-width
				     orgmode-pack-yt-html-iframe-height
				     path
				     (or desc "")))
		       (latex (format orgmode-pack-yt-latex-iframe-format
				      path (or desc "video"))))))

(require 'org-contacts)
(require 'org-protocol)

(require 'org-capture)
(add-to-list 'org-capture-templates
	     '("c" "Contacts" entry (file "~/.contacts/org-contacts.org")
	       "* %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:END:"))

(add-to-list 'org-capture-templates
	     '("x" "org-protocol" entry (file "~/org/todo.org")
	       "* TODO Review %c\n%U\n%i\n" :immediate-finish))

(defun org-read-entry-property-name ()
  "Read a property name from the current entry."
  (let ((completion-ignore-case t)
        (default-prop (or (and (org-at-property-p)
                               (org-match-string-no-properties 2))
                          org-last-set-property)))
    (org-completing-read
     (format "Property [%s]: " (if default-prop default-prop ""))
     (org-entry-properties nil nil)
     nil nil nil nil default-prop)))

(defun my-org-region-to-property (&optional property)
  "Transform a region to an org property.
PROPERTY can be provided as universal argument."
  (interactive)
  ;; if no region is defined, do nothing
  (if (use-region-p)
      ;; if a region string is found, ask for a property and set property to
      ;; the string in the region
      (let ((val (replace-regexp-in-string
                  "\\`[ \t\n]*" ""
                  (replace-regexp-in-string "[ \t\n]*\\'" ""
                                            (substring (buffer-string)
                                                       (- (region-beginning) 1)
                                                       (region-end)))))
            ;; if none was stated by user, read property from user
            (prop (or property
                      (org-read-entry-property-name))))

        ;; set property
        (org-set-property prop val))))

(provide 'orgmode-pack)
;;; orgmode-pack ends here
