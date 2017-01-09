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


