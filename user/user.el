;; set up for ergoemacs-mode
(use-package ergoemacs-mode
  ;; :load-path "elpa/ergoemacs-mode/"
  ;; :diminish undo-tree-mode
  :bind (("S-SPC" . "SPC") ;shift space should be a space	 
	 ("C-SPC" . set-mark-command)	;change Ctrl+Space to set mark since Alt+Space is reserved by Mac OSX for spotlight
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
  (setq ergoemacs-keyboard-layout "us") ;; Assumes QWERTY keyboard layout
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

