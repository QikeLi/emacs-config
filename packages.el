;;; packages.el --- Install and configure scimax packages
;;; Commentary:
;;
;; This is a starter kit for scimax. This package provides a
;; customized setup for emacs that we use daily for scientific
;; programming and publication.
;;
;; see https://github.com/jwiegley/use-package for details on use-package


;;; Code:

(setq use-package-always-ensure t)


;; * org-mode
;; load this first before anything else to avoid mixed installations
(use-package org-plus-contrib
  :mode ("\\.org\\'" . org-mode)
  :init
  ;; Use the current window for C-c ' source editing
  (setq org-src-window-setup 'current-window
	org-support-shift-select t)

  ;; I like to press enter to follow a link. mouse clicks also work.
  (setq org-return-follows-link t)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; This block is Qike's customization
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; set capture templates
  (setq org-capture-templates
  	'(("t" "Tasks" entry (file+headline "~/Dropbox/orgFiles/qikeMain.org" "Tasks")
  	   "* TODO %^{Brief Description} %^g\n%?\nAdded: %U" :prepend t)
  	  ("d" "Dailies" entry (file+headline "~/Dropbox/orgFiles/qikeMain.org" "Dailies")
  	   "* TODO %^{Brief Description} %^g\n%?\nAdded: %U" :prepend t)
  	  ("c" "Calendar" entry (file+headline "~/Dropbox/orgFiles/qikeMain.org" "Calendar")
  	   "* APPT %?\n%i\n:PROPERTIES:\n:REPEAT_TO_STATE: APPT\n:END:\n" :prepend t)
  	  ("p" "Projects" entry (file+headline "~/Dropbox/orgFiles/qikeMain.org" "Projects")
  	   "* %^{Brief Description} %^g\n%?\nAdded: %U" :prepend t)
  	  ;; ("w" "JSM" entry (file+headline "~/Dropbox/orgFiles/qikeMain.org" "Present simulation study @ JSM 2016")
  	  ;;  "* TODO %^{Brief Description} %^g\n%?\nAdded: %U" :prepend t)
  	  ;; ("y" "CTC" entry (file+headline "~/Dropbox/orgFiles/qikeMain.org" "Respond to ISMB CTC Review")
  	  ;;  "* TODO %^{Brief Description} %^g\n%?\nAdded: %U" :prepend t)
  	  ("o" "OUTREACH" entry (file+headline "~/Dropbox/orgFiles/qikeMain.org" "Produce outreach manuscript")
  	   "* TODO %^{Brief Description} %^g\n%?\nAdded: %U" :prepend t)
  	  ;; ("l" "LussierLab" entry (file+headline "~/Dropbox/orgFiles/qikeMain.org" "LussierLab")
  	  ;;  "* TODO %^{Brief Description} %^g\n%?\nAdded: %U" :prepend t)
  	  ("s" "Someday" entry (file "~/Dropbox/orgFiles/someday.org")
  	   "* TOFILE %^{Brief Description} %^g\n%?\nAdded: %U" :prepend t)
  	  ;; ("h" "HTG" entry (file+headline "~/Dropbox/orgFiles/qikeMain.org" "HTG")
  	  ;;  "* TODO %^{Brief Description} %^g\n%?\nAdded: %U" :prepend t)
  	  ("r" "Tickler Scheduled Tasks" entry (file+headline "~/Dropbox/orgFiles/qikeMain.org" "Tickler Scheduled Tasks")
  	   "* TODO %^{Brief Description} %^g\n%?\nAdded: %U" :prepend t)
  	  ;; ("f" "Financial" entry (file+headline "~/Dropbox/orgFiles/qikeMain.org" "Financial")
  	  ;;  "* TODO %^{Brief Description} %^g\n%?\nAdded: %U" :prepend t)
  	  ;; ("d" "PHD" entry (file+headline "~/Dropbox/orgFiles/qikeMain.org"s "PHD")
  	  ;;  "* TODO %^{Brief Description} %^g\n%?\nAdded: %U" :prepend t)
  	  ("n" "Notes" entry
  	   (file+headline "~/Dropbox/orgFiles/qikeMain.org" "Temporary Notes")
  	   "* %^{Brief Description} %^g\n%?\nAdded: %U" :prepend t)
  	  ("j" "Journal" entry (file+datetree "~/Dropbox/orgFiles/journal.org")
  	   "* %?\nEntered on %U\n  %i\n  %a\n" :prepend t)
  	  ("h" "Habits" entry (file+headline "~/Dropbox/orgFiles/qikeMain.org" "Habits")
  	   "* TODO %^{Brief Description} %^g\n%?\n:PROPERTIES:\n:STYLE:    habit\n:END:\nAdded: %U" :prepend t)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; setup Babel
  ;; active Babel languages
  (require 'ob-R)			;note 
  (custom-set-variables
   ;; specifies R language to be loaded
   '(org-babel-load-languages (quote (
  				      (shell . t)
  				      (latex . t)
  				      (python . t)
  				      (emacs-lisp . t)
  				      (R . t))))
   ;; Do not prompt for code evaluation
   '(org-confirm-babel-evaluate nil))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; clock-in when task is in STARTED state; clock-out when task is in WAITING state.
  (defun sacha/org-clock-in-if-starting ()
    "Clock in when the task is marked STARTED."
    (when (and (string= org-state "STARTED")
  	       (not (string= org-last-state org-state)))
      (org-clock-in)))

  (add-hook 'org-after-todo-state-change-hook
  	    'sacha/org-clock-in-if-starting)

  (defadvice org-clock-in (after sacha activate)
    "Set this task's status to 'STARTED'."
    (org-todo "STARTED"))

  (defun sacha/org-clock-out-if-waiting ()
    "Clock in when the task is marked STARTED."
    (when (and (string= org-state "WAITING")
  	       (not (string= org-last-state org-state)))
      (org-clock-out)))

  (add-hook 'org-after-todo-state-change-hook
  	    'sacha/org-clock-out-if-waiting)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; code block templates
  (eval-after-load 'org
    '(progn
       (add-to-list 'org-structure-template-alist
  		    '("g" "#+BEGIN_SRC R :exports both :results graphics :file ./fig_1?.pdf :width 6.75 :session\n\n#+END_SRC" "<src lang=\"?\">\n\n</src>"))
       (add-to-list 'org-structure-template-alist
  		    '("r" "#+BEGIN_SRC R :exports both :results output :session\n\n#+END_SRC" "<src lang=\"?\">\n\n</src>"))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;TODO States
  (setq org-todo-keywords
  	'((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "APPT(a)" "|" "DONE(d)" "CANCELLED(c)" "DEFERRED(f)")))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Preserve top level headings when archiving to a file
  ;; copied from: http://orgmode.org/worg/org-hacks.html
  (defun my-org-inherited-no-file-tags ()
    (let ((tags (org-entry-get nil "ALLTAGS" 'selective))
  	  (ltags (org-entry-get nil "TAGS")))
      (mapc (lambda (tag)
  	      (setq tags
  		    (replace-regexp-in-string (concat tag ":") "" tags)))
  	    (append org-file-tags (when ltags (split-string ltags ":" t))))
      (if (string= ":" tags) nil tags)))

  (defadvice org-archive-subtree (around my-org-archive-subtree-low-level activate)
    (let ((tags (my-org-inherited-no-file-tags))
  	  (org-archive-location
  	   (if (save-excursion (org-back-to-heading)
  			       (> (org-outline-level) 1))
  	       (concat (car (split-string org-archive-location "::"))
  		       "::* "
  		       (car (org-get-outline-path)))
  	     org-archive-location)))
      ad-do-it
      (with-current-buffer (find-file-noselect (org-extract-archive-file))
  	(save-excursion
  	  (while (org-up-heading-safe))
  	  (org-set-tags-to tags)))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; specify the org files to be scanned for agenda
  (setq org-agenda-files
  	(delq nil
  	      (mapcar (lambda (x) (and (file-exists-p x) x))
  		      '("~/Dropbox/orgFiles/qikeMain.org"
  			"~/Dropbox/orgFiles/journal.org"))))
  (add-to-list 'auto-mode-alist '("\\.txt$" . org-mode))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; only show only in agenda view
  (setq org-agenda-span (quote day))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; add org-habit module
  ;; (add-to-list 'org-modules 'org-habit t)
  (defun qike/habit-after-load-org ()	;delay adding org-habit to org-modules after org is loaded
    (add-to-list 'org-modules 'org-habit))
  (eval-after-load "org" '(qike/habit-after-load-org))
  (require 'org-habit)
  (require 'package)
  ;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  ;; setup for org-habit
  (setq org-todo-repeat-to-state nil)  
  (setq org-habit-graph-column 55)
  (setq org-habit-show-habits-only-for-today nil)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; set org-agenda sorting strategy
  (setq org-agenda-sorting-strategy
  	(quote
  	 ((agenda time-up habit-down priority-down category-keep)
  	  (todo priority-down category-keep)
  	  (tags priority-down category-keep)
  	  (search category-keep))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
  ;; fix a problem caused by ergoemacs, that is, when move a subtree down, only the heading get moved
  ;; After using scimax, this problem doesn't seem occur anymore. Now the key for org-move-subtree-down is "M-down", and for org-move-subtree-up is <M-up>
  ;; (add-hook 'org-mode-hook
  ;; 	    (lambda ()
  ;; 	      (local-set-key (kbd "<M-S-down>") 'org-move-subtree-down)
  ;; 	      (local-set-key  (kbd "<M-S-up>") 'org-move-subtree-up)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; set the max level of heading I can go to for refiling
  ;; see:https://www.reddit.com/r/emacs/comments/4366f9/how_do_orgrefiletargets_work/
  (setq org-refile-targets (quote (("qikeMain.org" :maxlevel . 6))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  :bind
  (("C-c l" . org-store-link)
   ("C-c L" . org-insert-link-global)
   ("C-c o" . org-open-at-point-global)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("s-<SPC>" . org-mark-ring-goto)
   ("H-." . org-time-stamp-inactive)))


;; * Other packages

(use-package aggressive-indent
  :config (aggressive-indent-global-mode 1))

(use-package auto-complete
  :diminish auto-complete-mode
  :config (ac-config-default))

(use-package avy)


(use-package tex
  :ensure auctex
  :config (setenv "PATH" (concat (getenv "PATH") ":/usr/local/texlive/2015/bin/x86_64-darwin"))
  (setq exec-path (append exec-path '(":/usr/local/texlive/2015/bin/x86_64-darwin"))))


;; Make cursor more visible when you move a long distance
(use-package beacon
  :config
  (beacon-mode 1))


(use-package bookmark
  :init
  (setq bookmark-default-file (expand-file-name "user/bookmarks" scimax-dir)
	bookmark-save-flag 1))


(use-package bookmark+
  ;; I am not currently using this, and it loads a bunch of files on startup.
  :disabled t)


;; Potential for commandline scripts using emacs
(use-package commander
  :disabled t)


(use-package swiper
  :bind
  ("C-s" . counsel-grep-or-swiper)
  :diminish ivy-mode
  :config
  (ivy-mode))

(use-package counsel
  :init
  (require 'ivy)
  (setq projectile-completion-system 'ivy)
  (setq ivy-use-virtual-buffers t)
  (define-prefix-command 'counsel-prefix-map)
  (global-set-key (kbd "H-c") 'counsel-prefix-map)

  ;; default pattern ignores order.
  (setf (cdr (assoc t ivy-re-builders-alist))
	'ivy--regex-ignore-order)
  :bind
  (("M-x" . counsel-M-x)
   ("C-x b" . ivy-switch-buffer)
   ("C-x C-f" . counsel-find-file)
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-h i" . counsel-info-lookup-symbol)
   ("H-c r" . ivy-resume)
   ("H-c l" . counsel-load-library)
   ("H-c g" . counsel-git-grep)
   ("H-c a" . counsel-ag)
   ("H-c p" . counsel-pt))
  :diminish ""
  :config
  (progn
    (counsel-mode)

    (define-key ivy-minibuffer-map (kbd "M-<SPC>") 'ivy-dispatching-done)

    ;; C-RET call and go to next
    (define-key ivy-minibuffer-map (kbd "C-<return>")
      (lambda ()
	"Apply action and move to next/previous candidate."
	(interactive)
	(ivy-call)
	(ivy-next-line)))

    ;; M-RET calls action on all candidates to end.
    (define-key ivy-minibuffer-map (kbd "M-<return>")
      (lambda ()
	"Apply default action to all candidates."
	(interactive)
	(ivy-beginning-of-buffer)
	(loop for i from 0 to (- ivy--length 1)
	      do
	      (ivy-call)
	      (ivy-next-line)
	      (ivy--exhibit))
	(exit-minibuffer)))

    ;; s-RET to quit
    (define-key ivy-minibuffer-map (kbd "s-<return>")
      (lambda ()
	"Exit with no action."
	(interactive)
	(ivy-exit-with-action
	 (lambda (x) nil))))

    (define-key ivy-minibuffer-map (kbd "?")
      (lambda ()
	(interactive)
	(describe-keymap ivy-minibuffer-map)))

    (define-key ivy-minibuffer-map (kbd "<left>") 'ivy-backward-delete-char)
    (define-key ivy-minibuffer-map (kbd "C-d") 'ivy-backward-delete-char)))

;; Provides functions for working on lists
(use-package dash)
(use-package dash-functional)

(use-package elfeed)

;; Python editing mode
(use-package elpy
  :config
  (elpy-enable))

;; Provides functions for working with files
(use-package f)

;; https://github.com/amperser/proselint
;; pip install proselint
(use-package flycheck
  ;; Jun 28 - I like this idea, but sometimes this is too slow.
  :config 
  (add-hook 'text-mode-hook #'flycheck-mode)
  (add-hook 'org-mode-hook #'flycheck-mode)
  (define-key flycheck-mode-map (kbd "s-;") 'flycheck-previous-error))


;; https://manuel-uberti.github.io/emacs/2016/06/06/spellchecksetup/
(use-package flyspell-correct-ivy
  :ensure t
  :init
  (if (file-directory-p (expand-file-name "emacs-win" scimax-dir))
      (progn
	;; spell-checking on windows
	(setq ispell-program-name
	      (expand-file-name
	       "emacs-win/bin/hunspell"
	       scimax-dir))

	(setq ispell-dictionary "english")

	(setq ispell-local-dictionary-alist
	      `(("english"
		 "[[:alpha:]]"
		 "[^[:alpha:]]"
		 "[']"
		 t
		 ("-d" "en_US" "-p" ,(expand-file-name
				      "emacs-win/share/hunspell/en_US"
				      scimax-dir))
		 nil
		 utf-8))))
    (setq ispell-program-name (executable-find "hunspell")
	  ispell-dictionary "en_US"
	  flyspell-correct-interface 'flyspell-correct-ivy))
  (add-hook 'flyspell-incorrect-hook
	    (lambda (beg end sym)
	      (message "%s misspelled. Type %s to fix it."
		       (buffer-substring beg end)
		       (substitute-command-keys
			"\\[flyspell-correct-previous-word-generic]"))
	      ;; return nil so word is still highlighted.
	      nil))
  (add-hook 'text-mode-hook
	    (lambda ()
	      (flyspell-mode)
	      (flycheck-mode)))

  (add-hook 'org-mode-hook
	    (lambda ()
	      (flyspell-mode +1)
	      (flycheck-mode +1)))
  
  :after flyspell
  :config
  (progn
    (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous-word-generic)))


(use-package flx)

(use-package git-messenger
  :bind ("C-x v o" . git-messenger:popup-message))

(use-package helm
  :init (setq helm-command-prefix-key "C-c h")
  :bind
  ("<f7>" . helm-recentf)
  ;; ("M-x" . helm-M-x)
  ;; ("M-y" . helm-show-kill-ring)
  ;; ("C-x b" . helm-mini)
  ;; ("C-x C-f" . helm-find-files)
  ;; ("C-h C-f" . helm-apropos)
  :config
  (add-hook 'helm-find-files-before-init-hook
	    (lambda ()
	      (helm-add-action-to-source
	       "Insert path"
	       (lambda (target)
		 (insert (file-relative-name target)))
	       helm-source-find-files)

	      (helm-add-action-to-source
	       "Insert absolute path"
	       (lambda (target)
		 (insert (expand-file-name target)))
	       helm-source-find-files)

	      (helm-add-action-to-source
	       "Attach file to email"
	       (lambda (candidate)
		 (mml-attach-file candidate))
	       helm-source-find-files)

	      (helm-add-action-to-source
	       "Make directory"
	       (lambda (target)
		 (make-directory target))
	       helm-source-find-files))))


(use-package helm-bibtex)

(use-package helm-projectile)

(use-package help-fns+)

;; Functions for working with hash tables
(use-package ht)

(use-package htmlize)

(use-package hy-mode)

(use-package hydra
  :init
  (setq hydra-is-helpful t)

  :config
  (require 'hydra-ox))

(use-package ivy-hydra)

(use-package jedi)

(use-package jedi-direx)

;; Superior lisp editing
(use-package lispy
  :config
  (dolist (hook '(emacs-lisp-mode-hook
		  hy-mode-hook))
    (add-hook hook
	      (lambda ()
		(lispy-mode)
		(eldoc-mode)))))

(use-package magit
  :init (setq magit-completing-read-function 'ivy-completing-read)
  :bind
  ("<f5>" . magit-status)
  ("C-c v t" . magit-status))

;; Templating system
;; https://github.com/Wilfred/mustache.el
(use-package mustache)

;; this is a git submodule
(use-package ob-ipython
  :ensure nil
  :load-path (lambda () (expand-file-name "ob-ipython" scimax-dir))
  :init
  (add-to-list 'load-path
	       (expand-file-name "ob-ipython" scimax-dir)))

(use-package ov)

;; this is a git submodule
(use-package org-ref
  :ensure nil
  :load-path (lambda () (expand-file-name "org-ref " scimax-dir))
  :init
  (add-to-list 'load-path
	       (expand-file-name "org-ref" scimax-dir))
  (setq bibtex-autokey-year-length 4
	bibtex-autokey-name-year-separator "-"
	bibtex-autokey-year-title-separator "-"
	bibtex-autokey-titleword-separator "-"
	bibtex-autokey-titlewords 2
	bibtex-autokey-titlewords-stretch 1
	bibtex-autokey-titleword-length 5)
  (global-set-key (kbd "H-b") 'org-ref-bibtex-hydra/body))


;; https://github.com/bbatsov/projectile
(use-package projectile
  :init (setq projectile-cache-file
	      (expand-file-name "user/projectile.cache" scimax-dir)
	      projectile-known-projects-file
	      (expand-file-name "user/projectile-bookmarks.eld" scimax-dir))
  :bind
  ("C-c pp" . projectile-switch-project)
  ("C-c pb" . projectile-switch-to-buffer)
  ("C-c pf" . projectile-find-file)
  ("C-c pg" . projectile-grep)
  ("C-c pk" . projectile-kill-buffers)
  ;; nothing good in the modeline to keep.
  :diminish ""
  :config
  (define-key projectile-mode-map (kbd "H-p") 'projectile-command-map)
  (projectile-global-mode))

(use-package pydoc)

(use-package rainbow-mode)

(use-package recentf
  :config
  (setq recentf-exclude
        '("COMMIT_MSG" "COMMIT_EDITMSG" "github.*txt$"
          ".*png$" "\\*message\\*" "auto-save-list\\*"))
  (setq recentf-max-saved-items 60))


;; Functions for working with strings
(use-package s)

(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'light)
  (sml/setup))


;; keep recent commands available in M-x
(use-package smex)

(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode))


;; * Scimax packages
(use-package scimax
  :ensure nil
  :load-path scimax-dir
  :init (require 'scimax))

(use-package scimax-mode
  :ensure nil
  :load-path scimax-dir
  :init (require 'scimax-mode)
  :config (scimax-mode))

(use-package scimax-org
  :ensure nil
  :load-path scimax-dir
  :bind
  ("s--" . org-subscript-region-or-point)
  ("s-=" . org-superscript-region-or-point)
  ("s-i" . org-italics-region-or-point)
  ("s-b" . org-bold-region-or-point)
  ("s-v" . org-verbatim-region-or-point)
  ("s-c" . org-code-region-or-point)
  ("s-u" . org-underline-region-or-point)
  ("s-+" . org-strikethrough-region-or-point)
  ("s-4" . org-latex-math-region-or-point)
  ("s-e" . ivy-insert-org-entity)
  :init
  (require 'scimax-org))

(use-package ox-clip
  :ensure nil
  :load-path scimax-dir
  :bind ("H-k" . ox-clip-formatted-copy))

(use-package scimax-email
  :ensure nil
  :load-path scimax-dir)

(use-package scimax-notebook
  :ensure nil
  :load-path scimax-dir)

(use-package scimax-utils
  :ensure nil
  :load-path scimax-dir
  :bind ( "<f9>" . hotspots)
  :config
  (setq scimax-user-hotspot-commands
	'(("Mail" . (lambda ()
		      (browse-url "https://www.google.com/gmail")))
	  ("Calendar" . (lambda ()
			  (browse-url "https://www.google.com/calendar/render")))
	  ("Contacts" . ivy-contacts)
	  ("RSS" . elfeed)
	  ("Twitter" . twit)
	  ("Agenda" . (lambda () (org-agenda "" "w"))) 
	  ("CV" . (lambda ()
		    (org-open-file
		     "/Users/jkitchin/Dropbox/CMU/CV and bios/kitchin_cv.docx" '(16)))))))

(use-package ox-manuscript
  :ensure nil
  :load-path (lambda () (expand-file-name "ox-manuscript" scimax-dir)))

(use-package org-show
  :ensure nil
  :load-path (lambda () (expand-file-name "org-show" scimax-dir)))

(use-package techela
  :ensure nil
  :load-path (lambda () (expand-file-name "techela" scimax-dir)))

(use-package words
  :ensure nil
  :load-path scimax-dir
  :bind ("H-w" . words-hydra/body))

(use-package ov-highlighter
  :ensure nil
  :load-path scimax-dir
  :bind ("H-h" . ov-highlighter/body)
  :init (require 'ov-highlighter))

(use-package ore
  :ensure nil
  :load-path scimax-dir
  :bind ("H-o" . ore))

(use-package org-editmarks
  :ensure nil
  :load-path scimax-dir)

(use-package scimax-ivy
  :ensure nil
  :load-path scimax-dir)

(use-package kitchingroup
  :ensure nil
  :load-path scimax-dir)


;; * User packages

;; We load one file: user.el

(when (and
       scimax-load-user-dir
       (file-exists-p (expand-file-name "user.el" user-dir)))
  (load (expand-file-name "user.el" user-dir)))

;; * The end
(provide 'packages)

;;; packages.el ends here
