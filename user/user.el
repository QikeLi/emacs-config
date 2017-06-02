;; * ergoemacs-mode
(use-package ergoemacs-mode
  ;; :load-path "~/Dropbox/scimax/elpa/ergoemacs-mode-20170112.1108"
  ;; :diminish undo-tree-mode
  ;; :demand			;override package deferral
  :ensure t
  :bind (("S-SPC" . "SPC")	    ;shift space should be a space	 
	 ("C-SPC" . set-mark-command)	;change Ctrl+Space to set mark since Alt+Space is reserved by Mac OSX for spotlight
	 ("M-<" . beginning-of-buffer)
	 ("M->" . end-of-buffer)
	 ("M-S-z" . undo-tree-redo)
	 ;; resolve the conflicts with swiper package
	 ("M-x" . ergoemacs-cut-line-or-region)
	 ("C-s" . save-buffer)
	 ;;	 ("C-f" . swiper)
	 ;;	 ("M-a" . counsel-M-x)
	 ;;	 ("C-o". counsel-find-file)
	 )
  :init
  ;; set keys for Apple keyboard, for emacs in OS X
  (setq mac-command-modifier 'meta) ;; Otherwise it would be Alt on Mac (I want Cmd which was a default for stable version of ergoemacs-mode)
  ;;(setq ergoemacs-use-mac-command-as-meta 1)
  (setq mac-option-modifier 'super) ; make opt key do Super
  ;; make my the menu key on Microsoft sculpt keyboad do hyper
  ;; (define-key key-translation-map (kbd "C-p") 'event-apply-hyper-modifier)
  ;; (setq mac-control-modifier 'control)	; make Control key do Control
  ;; (setq ns-function-modifier 'hyper)	; make Fn key do Hyper
  ;; (define-key key-translation-map (kbd "<f13>") (kbd "<menu>")) ;; <f13> is assigned to CAPSLOCK
  (setq ergoemacs-theme nil) ;; Uses Standard Ergoemacs keyboard theme
  (setq ergoemacs-keyboard-layout "us") ; Assumes QWERTY keyboard layout  
  (ergoemacs-mode 1)
  :config
  ;; enable backward-praragraph and ergoemacs-beginning-or-end-of-buffer in orgstruct-mode
  (progn
    (define-key orgstruct-mode-map (kbd "M-U") 'backward-paragraph )
    (define-key orgstruct-mode-map (kbd "M-n") 'ergoemacs-beginning-or-end-of-buffer))
  ;; resolve the coflicts with swiper
  (ergoemacs-require 'swiper))
  ;; ;; ;;;;;;;;;
  ;; :init

  ;; ;;;;;;;;;;;;;;;;;;
  ;; ;; Matthew Fidler' suggestion. See his email to me
  ;; (setq ivy-use-virtual-buffers t)
  ;; (global-set-key [remap isearch-forward] 'swiper)
  ;; (defun my-reftex-hook ()
  ;;   (define-key swiper "C-s" nil)
  ;;   (define-key reftex-mode-map "\C-c/" nil)
  ;;   (define-key reftex-mode-map "\C-c/" nil)
  ;;   )
  ;; (add-hook 'reftex-mode-hook 'my-reftex-hook)
  ;; ;; (global-set-key (kbd "C-c C-r") 'ivy-resume)
  ;; ;; (global-set-key (kbd "<f6>") 'ivy-resume)
  ;; (global-set-key [remap execute-extended-command] 'counsel-M-x)
  ;; (global-set-key [remap find-file] 'counsel-find-file)
  ;; (global-set-key [remap describe-function] 'counsel-describe-function)
  ;; (global-set-key [remap describe-variable] 'counsel-describe-variable)
  ;; (global-set-key [remap find-library] 'counsel-find-library)
  ;; (global-set-key [remap info-lookup-symbol] 'counsel-info-lookup-symbol)
  ;; ;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  ;; ;; (global-set-key (kbd "C-c g") 'counsel-git)
  ;; (global-set-key [remap vc-git-grep] 'counsel-git-grep)
  ;; ;; (global-set-key (kbd "C-c k") 'counsel-ag)
  ;; (global-set-key [remap locate] 'counsel-locate)
  ;; ;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  ;; ;; (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
  ;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 ;; (ergoemacs-require 'swiper)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ;; prevent show "symbol's value as variable is void: icicle-ido-like-mode" when describing a key-bindin,
  ;; ;; this may be a temporary fix
  ;; (setq icicle-ido-like-mode t) 	; prevent show "symbol's value as variable is void: icicle-ido-like-mode" when describing a key-bindin,
  ;; (setq icicle-mode t)		; this may be a temporary fix
  ;; (setq multiple-cursors-mode nil)		; this may be a temporary fix
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;
  
  ;;;;;;;;;;;; script in this block is experiment
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; (setq ergoemacs-mode-line nil)  ;; Does not work by just setting (diminish 'ergoemacs-mode)
  ;; (setq ergoemacs-handle-ctl-c-or-ctl-x 'only-C-c-and-C-x)  ;; this should fix problem of applying org-mode keybindings on regions
  ;; (setq ergoemacs-command-loop-blink-character nil)  ;; looks better
  ;; (setq ergoemacs-echo-function nil)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
  ;; :config
  
;; (ergoemacs-theme-component extra () 
;;   "Extra keys" 
;;   :layout "us"
;;   :bind (("<menu> 1" . split-window-right)
;; 	   ("M-g" . my-delete-line)
;; 	   ("M-S-g" . my-backward-delete-line)
;; 	   ("M-e" . my-backward-delete-word)
;; 	   ("M-r" . my-delete-word)
;; 	   ("<menu> c" . company-complete)))
;; (ergoemacs-require 'extra);; ;; 

;; * ESS
;; Adapted with one minor change from Felipe Salazar at
;; http://www.emacswiki.org/emacs/EmacsSpeaksStatistics
(use-package ess
  :commands R
  :bind (("C-M-c" . ess-readline)
	 ("C-M-t" . ess-readnextline))
  :init
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
  (setq exec-path (append exec-path '("/usr/local/bin")))
  :config
  (setq ess-ask-for-ess-directory nil)
  (setq ess-local-process-name "R")
  (setq ansi-color-for-comint-mode 'filter)
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-scroll-to-bottom-on-output t)
  (setq comint-move-point-for-output t)
  (defun my-ess-start-R ()
    (interactive)
    (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
	(progn
	  (delete-other-windows)
	  (setq w1 (selected-window))
	  (setq w1name (buffer-name))
	  (setq w2 (split-window w1 nil t))
	  (R)
	  (set-window-buffer w2 "*R*")
	  (set-window-buffer w1 w1name))))
  (defun my-ess-eval ()
    (interactive)
    (my-ess-start-R)
    (if (and transient-mark-mode mark-active)
	(call-interactively 'ess-eval-region)
      (call-interactively 'ess-eval-line-and-step)))
  (add-hook 'ess-mode-hook
	    '(lambda()
	       (local-set-key [(shift return)] 'my-ess-eval)))
  ;; (add-hook 'ess-mode-hook 'allout-mode)
  (add-hook 'inferior-ess-mode-hook
	    '(lambda()
	       (local-set-key [C-up] 'comint-previous-input)
	       (local-set-key [C-down] 'comint-next-input)))
  (add-hook 'Rnw-mode-hook
	    '(lambda()
	       (local-set-key [(shift return)] 'my-ess-eval)))
  (require 'ess-site)  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; command history in R prompt
  ;; added 16 Mar 2016, 11:07am
  ;; thanks to Heather Turner
  ;; http://stackoverflow.com/questions/27307757/ess-retrieving-command-history-from-commands-entered-in-essr-inferior-mode-or
  (defun ess-readline ()
    "Move to previous command entered from script *or* R-process and copy 
   to prompt for execution or editing"
    (interactive)
    ;; See how many times function was called
    (if (eq last-command 'ess-readline)
	(setq ess-readline-count (1+ ess-readline-count))
      (setq ess-readline-count 1))
    ;; Move to prompt and delete current input
    (comint-goto-process-mark)
    (end-of-buffer nil) ;; tweak here
    (comint-kill-input)
    ;; Copy n'th command in history where n = ess-readline-count
    (comint-previous-prompt ess-readline-count)
    (comint-copy-old-input)
    ;; Below is needed to update counter for sequential calls
    (setq this-command 'ess-readline)
    )
  
  (defun ess-readnextline ()
    "Move to next command after the one currently copied to prompt and copy 
   to prompt for execution or editing"
    (interactive)
    ;; Move to prompt and delete current input
    (comint-goto-process-mark)
    (end-of-buffer nil)
    (comint-kill-input)
    ;; Copy (n - 1)'th command in history where n = ess-readline-count
    (setq ess-readline-count (max 0 (1- ess-readline-count)))
    (when (> ess-readline-count 0)
      (comint-previous-prompt ess-readline-count)
      (comint-copy-old-input))
    ;; Update counter for sequential calls
    (setq this-command 'ess-readline)
    )

  ;; Smart assign/underscore management
  (setq ess-smart-S-assign-key (kbd ";"))
  (ess-toggle-S-assign nil) ; note: this line was used to prevent underscore being assgin-key
  (ess-toggle-underscore nil) ; leave underscore key alone!
  )



;; * integrate gmail with emacs
(use-package edit-server
  :init
  (require 'edit-server)
  (edit-server-start)
  (setq edit-server-new-frame nil))
(use-package gmail-message-mode)

;; * conver pdf figures to image files when export org files
;; a package to convert pdf figures in your .org file to .png, .jpg, etc., so that the
;; figures can be displayed properly after exported.
;; referece:
;; https://github.com/kaushalmodi/.emacs.d/issues/22   ;this is an issue I raised under the name: modi/org-include-img-from-pdf function
;; https://github.com/kaushalmodi/.emacs.d/tree/master/elisp/org-include-img-from-pdf
(use-package org-include-img-from-pdf
  :ensure nil
  :load-path "~/Dropbox/scimax/user/user-packages"
  :config
  (progn
    (with-eval-after-load 'ox
      ;; (add-hook 'org-export-before-processing-hook #'modi/org-include-img-from-pdf)))) ;Modi's original code
      (add-hook 'org-export-babel-evaluate-hook #'modi/org-include-img-from-pdf)))) ;Qike's modifiication
;; * Outlines in R code
;; This minor mode is written by Qike. I modified the code in Kitchin's scimax.el
(use-package outline-R-code
  :ensure nil
  :load-path "~/Dropbox/scimax/user/user-packages")

;; * Outlines in LaTeX code
;; This minor mode is written by Qike. I modified the code in Kitchin's scimax.el
(use-package outline-TeX-code
  :ensure nil
  :load-path "~/Dropbox/scimax/user/user-packages") 


;; * org-mime
(use-package org-mime
  :load-path "~/Dropbox/scimax"
  :ensure nil)
;; * org-export-html-with-numbered-bibliography
;; turn this off if exporting to LaTeX(Beamer) is having problems 
;; (use-package org-export-html-with-numbered-bibliography
;;   :ensure nil
;;   :load-path "~/Dropbox/scimax/user/user-packages")

;; * cdlatex
;; this is a minor mode to speed up typing math symbols
(use-package cdlatex
  :ensure nil
  :init
  :diminish cdlatex-mode
  :config
  (progn
    ;; (add-hook 'org-mode-hook 'org-cdlatex-mode) ; with org mode
    (add-hook 'org-mode-hook 'turn-on-org-cdlatex) ; with org mode
    (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)     ; with AUCTeX LaTeX mode
    (add-hook 'latex-mode-hook 'turn-on-cdlatex)))     ; with Emacs latex mode
;; * Define two keybindings for org-ref
;; Define two keybinding for org-ref-helm-insert-label-link and org-ref-helm-insert-ref-link

(use-package org-ref-insert-kbd-mode
  :ensure nil
  :load-path "~/Dropbox/scimax/user/user-packages"
  :diminish org-ref-insert-kbd-mode)

;; * window-numbering
;; (use-package window-numbering
;;   :ensure t)


;; * polymode, which enables Rmarkdown
;; (use-package polymode
;;   :ensure t
;;   ;; :load-path "~/Dropbox/scimax/user/user-packages/polymode/"
;;   :load-path "~/Dropbox/scimax/user/user-packages/polymode/modes/poly-R.el"
;;   :mode
;;       ("\\.Snw" . poly-noweb+r-mode)
;;       ("\\.Rnw" . poly-noweb+r-mode)
;;       ("\\.Rmd" . poly-markdown+r-mode)
;;   :init (require ’poly-R)
;;   (require ’poly-markdown))

(use-package polymode		; ESS with polymode
  :ensure f	                        ; https://github.com/vitoshka/polymode
  :load-path "~/Dropbox/scimax/user/user-packages/polymode"
  :load-path "~/Dropbox/scimax/user/user-packages/polymode/modes"
  :init

    ;; (setq load-path 		; Append the directory to emacs path
    ;;   (append '("~/.emacs.d/polymode"
    ;;   "~/.emacs.d/polymode/modes") load-path))
  (require 'poly-R)		; Load necessary modes
  (require 'poly-markdown)
  (require 'poly-noweb)
  :mode
  ("\\.Snw" . poly-noweb+r-mode)
  ("\\.Rnw" . poly-noweb+r-mode)
  ("\\.Rmd" . poly-markdown+r-mode))

    

;; * whitespace mode
(use-package whitespace
  :init
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face lines-tail))
  (add-hook 'prog-mode-hook 'whitespace-mode)
  (add-hook 'ess-mode-hook 'whitespace-mode)
  (setq whitespace-mode t)  )
;; * code block background color in org-mode
;; * reassign the background colors of the code blocks
(defface org-block-emacs-lisp
  `((t (:background "gray22")))
  "Face for elisp src blocks")

(defface org-block-R
  `((t (:background "gray22")))
  "Face for elisp src blocks")

(defface org-block-python
  `((t (:background "gray22")))
  "Face for python blocks")

(defface org-block-ipython
  `((t (:background "gray22")))
  "Face for python blocks") 

(defface org-block-jupyter-hy
  `((t (:background "gray22")))
  "Face for hylang blocks")

(defface org-block-sh
  `((t (:background "gray22")))
  "Face for python blocks")
;; * change color of script highlights in Latex mode
;; Notes:
;;   The following is originally defined in font-latex.el
(defface font-latex-script-char-face
  (let ((font (cond ((assq :inherit custom-face-attributes)
		     '(:inherit underline))
		    (t '(:underline t)))))
    `((((class grayscale) (background light))
       (:foreground "DarkGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "gray" ,@font))
      (((class color) (background light))
       (:foreground "salmon"))
      (((class color) (background dark))
       (:foreground "tomato1"))
      (t (,@font))))
  "Face used for the script chars ^ and _."
  :group 'font-latex-highlighting-faces)
;; * contacts
;; (use-package contacts
;;   :ensure nil
;;   :load-path scimax-dir)
(use-package org-contacts
  :ensure nil
  :init
  (defcustom org-contacts-files (list "~/Dropbox/orgFiles/qike-contacts/qike-contacts.org")
    "List of Org files to use as contacts source.
When set to nil, all your Org files will be used."
    :type '(repeat file)
    :group 'org-contacts))
;; * Biometrics Journal template
;; ** <<biometrics>>
(add-to-list 'org-latex-classes '("biometrics-article"
				  "\\documentclass[useAMS,referee]{biom}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
				  ;; ("\\abstract{%s}" . "\\abstract*{%s}")
				  ("\\section{%s}" . "\\section*{%s}")
				  ("\\subsection{%s}" . "\\subsection*{%s}")
				  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
				  ("\\paragraph{%s}" . "\\paragraph*{%s}")
				  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; * open with pacakge
(use-package openwith
  :config
  (openwith-mode t)
  (setq openwith-associations '(("\\.avi\\'" "/Applications/mpv.app/Contents/MacOS/mpv" (file))
  			       ("\\.mkv\\'" "/Applications/mpv.app/Contents/MacOS/mpv" (file))
  			       ("\\.mp4\\'" "/Applications/VLC.app/Contents/MacOS/VLC" (file))
  			       ("\\.pdf\\'" "open" (file))
			       ("\\.docx\\'" "open" (file))
			       ("\\.html\\'" "open" (file)))))

;; * org-gcal
;; this setup sync the gcal.org file with my google calendar
;; this setup follows the blog post that can be found here:http://cestlaz.github.io/posts/using-emacs-26-gcal/#.WREgs4nyvWd
(setq package-check-signature nil)
(use-package org-gcal
  :ensure t
  :config
  (setq org-gcal-client-id "89437206869-tvpubapoa49nh4ahoh8do3otu80sdihf.apps.googleusercontent.com"
	org-gcal-client-secret "W63P3SAcpMU2wL-1nMZ4siQz"
	org-gcal-file-alist '(("liqike@gmail.com" .  "~/Dropbox/orgFiles/gcal.org"))))
(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))
;; * Miscellaneous
;; ** set some variables
;; *** Turn on (flyspell-mode)
(flyspell-mode)				;; Note: force to turn this on when start up emacs since otherwise have trouble to load dictionary. see the issue I raised at scimax: https://github.com/jkitchin/scimax/issues/32
;; *** Turn on agenda reminder
(org-agenda-to-appt)
;; *** Everyday at 12:05am run org-agenda-to-appt(useful in case you keep Emacs always on)
(run-at-time "12:05am" (* 24 3600) 'org-agenda-to-appt)
;; *** prvent start a new frame when open a file from Mac Finder
(setq ns-pop-up-frames nil)
;; m*** ove files to ~/.Trash when delete
(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.Trash")
;; *** Turn off alarm sounds
(setq ring-bell-function 'ignore)
;; ** a function to open file at cursor
(defun xah-open-file-at-cursor ()
  "Open the file path under cursor.
If there is text selection, uses the text selection for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:‹n›” that indicates line number. If so, jump to that line number.
If path does not have a file extension, automatically try with “.el” for elisp files.
This command is similar to `find-file-at-point' but without prompting for confirmation.

URL `http://ergoemacs.org/emacs/emacs_open_file_path_fast.html'"
  (interactive)
  (let ((-path (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (let (p0 p1 p2)
                   (setq p0 (point))
                   ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
                   (skip-chars-backward "^  \"\t\n`'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\`")
                   (setq p1 (point))
                   (goto-char p0)
                   (skip-chars-forward "^  \"\t\n`'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\'")
                   (setq p2 (point))
                   (goto-char p0)
                   (buffer-substring-no-properties p1 p2)))))
    (if (string-match-p "\\`https?://" -path)
        (browse-url -path)
      (progn ; not starting “http://”
        (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\'" -path)
            (progn
              (let (
                    (-fpath (match-string 1 -path))
                    (-line-num (string-to-number (match-string 2 -path))))
                (if (file-exists-p -fpath)
                    (progn
                      (find-file -fpath)
                      (goto-char 1)
                      (forward-line (1- -line-num)))
                  (progn
                    (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" -fpath))
                      (find-file -fpath))))))
          (progn
            (if (file-exists-p -path)
                (find-file -path)
              (if (file-exists-p (concat -path ".el"))
                  (find-file (concat -path ".el"))
                (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" -path))
                  (find-file -path ))))))))))


;; ** a function and keybinding to swap window postions
;; swap two windows
(defun swap-window-positions ()         ; Stephen Gildea
  "*Swap the positions of this window and the next one."
  (interactive)
  (let ((other-window (next-window (selected-window) 'no-minibuf)))
    (let ((other-window-buffer (window-buffer other-window))
	  (other-window-hscroll (window-hscroll other-window))
	  (other-window-point (window-point other-window))
	  (other-window-start (window-start other-window)))
      (set-window-buffer other-window (current-buffer))
      (set-window-hscroll other-window (window-hscroll (selected-window)))
      (set-window-point other-window (point))
      (set-window-start other-window (window-start (selected-window)))
      (set-window-buffer (selected-window) other-window-buffer)
      (set-window-hscroll (selected-window) other-window-hscroll)
      (set-window-point (selected-window) other-window-point)
      (set-window-start (selected-window) other-window-start))
    (select-window other-window)))
(global-set-key (kbd "s-p") 'swap-window-positions)

;; ** a function to display PDF images in org-mode

;; ;; Execute the `modi/org-include-img-from-pdf' function just before saving the file
;; (add-hook 'before-save-hook #'modi/org-include-img-from-pdf)
;; ;; Execute the `modi/org-include-img-from-pdf' function before processing the
;; ;; file for export
;; (add-hook 'org-export-before-processing-hook #'modi/org-include-img-from-pdf)

;; (defun modi/org-include-img-from-pdf (&rest ignore)
;;   "Convert the pdf files to image files.

;; Only looks at #HEADER: lines that have \":convertfrompdf t\".
;; This function does nothing if not in org-mode, so you can safely
;; add it to `before-save-hook'."
;;   (interactive)
;;   (when (derived-mode-p 'org-mode)
;;     (save-excursion
;;       (goto-char (point-min))
;;       (while (search-forward-regexp
;;               "^\\s-*#\\+HEADER:.*\\s-:convertfrompdf\\s-+t"
;;               nil 'noerror)
;;         (let* (filenoext imgext imgfile pdffile cmd)
;;           ;; Keep on going on to the next line till it finds a line with
;;           ;; `[[FILE]]'
;;           (while (progn
;;                    (forward-line 1)
;;                    (not (looking-at "\\[\\[\\(.*\\)\\.\\(.*\\)\\]\\]"))))
;;           (when (looking-at "\\[\\[\\(.*\\)\\.\\(.*\\)\\]\\]")
;;             (setq filenoext (match-string-no-properties 1))
;;             (setq imgext (match-string-no-properties 2))
;;             (setq imgfile (expand-file-name (concat filenoext "." imgext)))
;;             (setq pdffile (expand-file-name (concat filenoext "." "pdf")))
;;             (setq cmd (concat "convert -density 96 -quality 85 "
;;                               pdffile " " imgfile))
;;             (when (file-newer-than-file-p pdffile imgfile)
;;               ;; This block is executed only if pdffile is newer than imgfile
;;               ;; or if imgfile does not exist
;;               ;; Source: https://www.gnu.org/software/emacs/manual/html_node/elisp/Testing-Accessibility.html
;;               (message "%s" cmd)
;;               (shell-command cmd))))))))
;; ** email
;; I copied this configuration from this website: https://www.emacswiki.org/emacs/GnusGmail
(setq gnus-select-method
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")  ; it could also be imap.googlemail.com if that's your server.
	       (nnimap-server-port "imaps")
	       (nnimap-stream ssl)))

(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-smtp-user "liqike"	   ;my email user name
      user-mail-address "liqike@gmail.com" ;my email address
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;; to prevent line breaks in emails
;; (defun no-auto-fill ()
;;   "Turn off auto-fill-mode."
;;   (auto-fill-mode -1))
;; (add-hook 'mu4e-compose-mode-hook #'no-auto-fill)


(setq auto-fill-mode '(not mu4e-compose-mode))
