;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This script is for exporting numbered citations in html with unsorted numbered bibliography
;; see: http://kitchingroup.cheme.cmu.edu/blog/2015/12/03/Exporting-numbered-citations-in-html-with-unsorted-numbered-bibliography/


(setq org-ref-bibliography-entry-format
      '(("article" . "<li><a name=\"\%k\"></a>%a, %t, <i>%j</i>, <b>%v(%n)</b>, %p (%y). <a href=\"%U\">link</a>. <a href=\"http://dx.doi.org/%D\">doi</a>.</li>")
	("book" . "<li><a name=\"\%k\"></a>%a, %t, %u (%y).</li>")))

(defun org-ref-nil-html-processor () nil) ;added by Qike
(defun org-ref-nil-latex-processor () nil) ;added by Qike
(defun org-ref-unsrt-latex-processor () nil)
(defun org-ref-unsrt-html-processor ()
  "Citation processor function for the unsrt style with html output."
  (let (links
	unique-keys numbered-keys
	replacements
	bibliography-link
	bibliographystyle-link
	bibliography)
    ;; step 1 - get the citation links
    (setq links (loop for link in (org-element-map
				      (org-element-parse-buffer) 'link 'identity)
		      if (-contains?
			  org-ref-cite-types
			  (org-element-property :type link))
		      collect link))

    ;; list of unique numbered keys. '((key number))
    (setq unique-keys (loop for i from 1
			    for key in (org-ref-get-bibtex-keys)
			    collect (list key (number-to-string i))))


    ;; (start end replacement-text)
    (setq replacements
	  (loop for link in links
		collect
		(let ((path (org-element-property :path link)))
		  (loop for (key number) in unique-keys
			do
			(setq
			 path
			 (replace-regexp-in-string
			  key (format "<a href=\"#%s\">%s</a>" key number)
			  path)))
		  (list (org-element-property :begin link)
			(org-element-property :end link)
			(format "@@html:<sup>%s</sup>@@" path)))))

    ;; construct the bibliography string
    (setq bibliography
	  (concat "#+BEGIN_EXPORT HTML	
<h1>Bibliography</h1><ol>"
		  (mapconcat
		   'identity
		   (loop for (key number) in unique-keys
			 collect
			 (let* ((result (org-ref-get-bibtex-key-and-file key))
				(bibfile (cdr result))
				(entry (save-excursion
					 (with-temp-buffer
					   (insert-file-contents bibfile)
					   (bibtex-set-dialect
					    (parsebib-find-bibtex-dialect) t)
					   (bibtex-search-entry key)
					   (bibtex-parse-entry t)))))
			   ;; remove escaped & in the strings
			   (replace-regexp-in-string "\\\\&" "&"
					   (org-ref-reftex-format-citation
					    entry
					    (cdr (assoc (cdr (assoc "=type=" entry))
							org-ref-bibliography-entry-format))))))
		   "")
		  "</ol>
#+END_EXPORT")) ;changed by Qike

    ;; now, we need to replace each citation. We do that in reverse order so the
    ;; positions do not change.
    (loop for (start end replacement) in (reverse replacements)
	  do
	  (setf (buffer-substring start end) replacement))

    ;; Eliminate bibliography style links
    (loop for link in (org-element-map
			  (org-element-parse-buffer) 'link 'identity)
	  if (string= "bibliographystyle"
		      (org-element-property :type link))
	  do
	  (setf (buffer-substring (org-element-property :begin link)
				  (org-element-property :end link))
		""))

    ;; replace the bibliography link with the bibliography text
    (setq bibliography-link (loop for link in (org-element-map
						  (org-element-parse-buffer) 'link 'identity)
				  if (string= "bibliography"
					      (org-element-property :type link))
				  collect link))
    (if (> (length bibliography-link) 1)
	(error "Only one bibliography link allowed"))

    (setq bibliography-link (car bibliography-link))
    (setf (buffer-substring (org-element-property :begin bibliography-link)
			    (org-element-property :end bibliography-link))
	  bibliography)))


(defun org-ref-citation-processor (backend)
  "Figure out what to call and call it"
  (let (bibliographystyle)
    (setq
     bibliographystyle
     (org-element-property
      :path (car
	     (loop for link in
		   (org-element-map
		       (org-element-parse-buffer) 'link 'identity)
		   if (string= "bibliographystyle"
			       (org-element-property :type link))
		   collect link))))
    (funcall (intern (format "org-ref-%s-%s-processor" bibliographystyle backend)))))

(add-hook 'org-export-before-parsing-hook 'org-ref-citation-processor)

;; * following doesn't work
;; (defun org-ref-nil-html-processor () nil) ;added by Qike, see my comment form the above link 
;; (defun org-ref-nil-latex-processor () nil) ;added by Qike
;; (defun org-ref-unsrt-latex-processor () nil)
;; ;; *** org-ref-unsrt-html-processor 
;; (defun org-ref-unsrt-html-processor ()
;;   "Citation processor function for the unsrt style with html output."
;;   (let (links
;; 	unique-keys numbered-keys
;; 	replacements
;; 	bibliography-link
;; 	bibliographystyle-link
;; 	bibliography)
;;     ;; step 1 - get the citation links
;;     (setq links (loop for link in (org-element-map
;; 				      (org-element-parse-buffer) 'link 'identity)
;; 		      if (-contains?
;; 			  org-ref-cite-types
;; 			  (org-element-property :type link))
;; 		      collect link))

;;     ;; list of unique numbered keys. '((key number))
;;     (setq unique-keys (loop for i from 1
;; 			    for key in (org-ref-get-bibtex-keys)
;; 			    collect (list key (number-to-string i))))


;;     ;; (start end replacement-text)
;;     (setq replacements
;; 	  (loop for link in links
;; 		collect
;; 		(let ((path (org-element-property :path link)))
;; 		  (loop for (key number) in unique-keys
;; 			do
;; 			(setq
;; 			 path
;; 			 (replace-regexp-in-string
;; 			  key (format "<a href=\"#%s\">%s</a>" key number)
;; 			  path)))
;; 		  (list (org-element-property :begin link)
;; 			(org-element-property :end link)
;; 			(format "@@html:<sup>%s</sup>@@" path)))))

;;     ;; construct the bibliography string
;;     (setq bibliography
;; 	  (concat "#+BEGIN_EXPORT HTML	
;;   <h1>Bibliography</h1><ol>"
;; 		  (mapconcat
;; 		   'identity
;; 		   (loop for (key number) in unique-keys
;; 			 collect
;; 			 (let* ((result (org-ref-get-bibtex-key-and-file key))
;; 				(bibfile (cdr result))
;; 				(entry (save-excursion
;; 					 (with-temp-buffer
;; 					   (insert-file-contents bibfile)
;; 					   (bibtex-set-dialect
;; 					    (parsebib-find-bibtex-dialect) t)
;; 					   (bibtex-search-entry key)
;; 					   (bibtex-parse-entry t)))))
;; 			   ;; remove escaped & in the strings
;; 			   (replace-regexp-in-string "\\\\&" "&"
;; 						     (org-ref-reftex-format-citation
;; 						      entry
;; 						      (cdr (assoc (cdr (assoc "=type=" entry))
;; 								  org-ref-bibliography-entry-format))))))
;; 		   "")
;; 		  "</ol>
;;   #+END_EXPORT")) ;changed by Qike

;;     ;; now, we need to replace each citation. We do that in reverse order so the
;;     ;; positions do not change.
;;     (loop for (start end replacement) in (reverse replacements)
;; 	  do
;; 	  (setf (buffer-substring start end) replacement))

;;     ;; Eliminate bibliography style links
;;     (loop for link in (org-element-map
;; 			  (org-element-parse-buffer) 'link 'identity)
;; 	  if (string= "bibliographystyle"
;; 		      (org-element-property :type link))
;; 	  do
;; 	  (setf (buffer-substring (org-element-property :begin link)
;; 				  (org-element-property :end link))
;; 		""))

;;     ;; replace the bibliography link with the bibliography text
;;     (setq bibliography-link (loop for link in (org-element-map
;; 						  (org-element-parse-buffer) 'link 'identity)
;; 				  if (string= "bibliography"
;; 					      (org-element-property :type link))
;; 				  collect link))
;;     (if (> (length bibliography-link) 1)
;; 	(error "Only one bibliography link allowed"))

;;     (setq bibliography-link (car bibliography-link))
;;     (setf (buffer-substring (org-element-property :begin bibliography-link)
;; 			    (org-element-property :end bibliography-link))
;; 	  bibliography)))


;; (defun org-ref-citation-processor (backend)
;;   "Figure out what to call and call it"
;;   (let (bibliographystyle)
;;     (setq
;;      bibliographystyle
;;      (org-element-property
;;       :path (car
;; 	     (loop for link in
;; 		   (org-element-map
;; 		       (org-element-parse-buffer) 'link 'identity)
;; 		   if (string= "bibliographystyle"
;; 			       (org-element-property :type link))
;; 		   collect link))))
;;     (funcall (intern (format "org-ref-%s-%s-processor" bibliographystyle backend)))))
;; (add-hook 'org-export-before-parsing-hook 'org-ref-citation-processor)

(provide 'org-export-html-with-numbered-bibliography)

