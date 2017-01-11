;; * ergoemacs-mode
(use-package ergoemacs-mode
  ;; :load-path "elpa/ergoemacs-mode/"
  ;; :diminish undo-tree-mode
  :demand			;override package deferral
  :bind (("S-SPC" . "SPC") ;shift space should be a space	 
	 ("C-SPC" . set-mark-command)	;change Ctrl+Space to set mark since Alt+Space is reserved by Mac OSX for spotlight
	 ("M-<" . beginning-of-buffer)
	 ("M->" . end-of-buffer)
	 ("M-S-z" . undo-tree-redo)
	 ;; resolve the conflicts with swiper package
	 ("M-x" . ergoemacs-cut-line-or-region)
	 ("C-s" . save-buffer)
	 ("C-f" . swiper)
	 ("M-a" . counsel-M-x)
	 ("C-o". counsel-find-file))
  :init
  ;; set keys for Apple keyboard, for emacs in OS X
  (setq mac-command-modifier 'meta)  ;; Otherwise it would be Alt on Mac (I want Cmd which was a default for stable version of ergoemacs-mode)
  (setq mac-option-modifier 'super) ; make opt key do Super
  (setq mac-control-modifier 'control) ; make Control key do Control
  (setq ns-function-modifier 'hyper)  ; make Fn key do Hyper
  ;; (define-key key-translation-map (kbd "<f13>") (kbd "<menu>")) ;; <f13> is assigned to CAPSLOCK
  (setq ergoemacs-theme nil) ;; Uses Standard Ergoemacs keyboard theme
  (setq ergoemacs-keyboard-layout "us") ; Assumes QWERTY keyboard layout
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; prevent show "symbol's value as variable is void: icicle-ido-like-mode" when describing a key-bindin,
  ;; this may be a temporary fix
  (setq icicle-ido-like-mode t) 	; prevent show "symbol's value as variable is void: icicle-ido-like-mode" when describing a key-bindin,
  (setq icicle-mode t)		; this may be a temporary fix
  (setq multiple-cursors-mode t)		; this may be a temporary fix
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;;;;;;;;;;;; script in this block is experiment
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; (setq ergoemacs-mode-line nil)  ;; Does not work by just setting (diminish 'ergoemacs-mode)
  ;; (setq ergoemacs-handle-ctl-c-or-ctl-x 'only-C-c-and-C-x)  ;; this should fix problem of applying org-mode keybindings on regions
  ;; (setq ergoemacs-command-loop-blink-character nil)  ;; looks better
  ;; (setq ergoemacs-echo-function nil)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
  ;; :config
  (ergoemacs-mode 1))
;; (ergoemacs-theme-component extra () 
;;   "Extra keys" 
;;   :layout "us"
;;   :bind (("<menu> 1" . split-window-right)
;; 	   ("M-g" . my-delete-line)
;; 	   ("M-S-g" . my-backward-delete-line)
;; 	   ("M-e" . my-backward-delete-word)
;; 	   ("M-r" . my-delete-word)
;; 	   ("<menu> c" . company-complete)))
;; (ergoemacs-require 'extra)

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
  ;; (ess-toggle-S-assign nil) ; note: this line was used to prevent underscore being assgin-key
  ;; (ess-toggle-S-assign nil); note: this line was used to prevent underscore being assgin-key
  ;; (ess-toggle-underscore nil) ; leave underscore key alone!
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
      (add-hook 'org-export-before-processing-hook #'modi/org-include-img-from-pdf))))

;; * Miscellaneous
;; ** set some variables
;; Turn on (flyspell-mode)
(flyspell-mode)				;; Note: force to turn this on when start up emacs since otherwise have trouble to load dictionary. see the issue I raised at scimax: https://github.com/jkitchin/scimax/issues/32
;; Turn on agenda reminder
(org-agenda-to-appt)
;; Everyday at 12:05am run org-agenda-to-appt(useful in case you keep Emacs always on)
(run-at-time "12:05am" (* 24 3600) 'org-agenda-to-appt)
;; prvent start a new frame when open a file from Mac Finder
(setq ns-pop-up-frames nil)
;; move files to ~/.Trash when delete
(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.Trash")

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
(global-set-key (kbd "C-c C-x p") 'swap-window-positions)

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
