;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with ^X ^F and enter text in its buffer.

(defvar org-ref-insert-kbd-mode-map (make-sparse-keymap)
  "Keymap while org-ref-insert-kbd-mode is active.")

;;;###autoload
(define-minor-mode org-ref-insert-kbd-mode
  "A temporary minor mode to be activated only specific to a buffer."
  nil
  :lighter " Temp"
  org-ref-insert-kbd-mode-map)

(define-minor-mode org-ref-insert-kbd-mode
  "Minor mode to simulate buffer local keybindings."
  :init-value nil)

(define-key org-ref-insert-kbd-mode-map (kbd "C-c (") 'org-ref-helm-insert-label-link)
(define-key org-ref-insert-kbd-mode-map (kbd "C-c )") 'org-ref-helm-insert-ref-link)

(provide 'org-ref-insert-kbd-mode)

