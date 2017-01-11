;; * Outlines in LaTeX code

(defconst lTeX-font-lock-keywords
  '(("^% \\(\\* .*\\)$" 1 'org-level-1 prepend)
    ("^% \\(\\*\\* .*\\)$" 1 'org-level-2 prepend)
    ("^%  \\(\\*\\*\\* .*\\)$" 1 'org-level-3 prepend)
    ("^%  \\(\\*\\*\\*\\* .*\\)$" 1 'org-level-4 prepend)
    ("^%  \\(\\*\\*\\*\\*\\* .*\\)$" 1 'org-level-5 prepend)
    ;; (lTeX-outline-comment-highlight 1 'default prepend)
    ("`\\([^\n']+\\)'" 1 font-lock-constant-face prepend)))

(font-lock-add-keywords 'TeX-mode lTeX-font-lock-keywords)

;; ** haven't got this working yet
;; This chunk is for highlighing the comment
;; (defun lTeX-outline-comment-highlight (limit)
;;   (while (re-search-forward "^%  \\(?:[^*]\\|$\\)" limit t)
;;     (let* ((pt (point))
;; 	   (success (save-excursion
;; 		      (and (re-search-backward "^%  \\*" nil t)
;; 			   (null (re-search-forward "^[^% ]" pt t))))))
;;       (when success
;; 	(set-match-data (list (line-beginning-position) (line-end-position)
;; 			      (point) (line-end-position)))
;; 	(end-of-line)
;; 	t))))

(defun TeX-outline-setup ()
  "Setup outline and orgstruct mode for TeX code.
This enables you to use tab to open and close outlines."
  (setq-local outline-regexp "%  ?\\*+\\|\\`")
  (setq-local orgstruct-heading-prefix-regexp "%  ?\\*+\\|\\`")
  (outline-minor-mode)
  (orgstruct-mode)
  (outline-show-branches))
  
(add-hook 'TeX-mode-hook
	  #'TeX-outline-setup)

(provide 'outline-TeX-code)
