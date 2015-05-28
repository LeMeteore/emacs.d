;; -*- coding: utf-8 -*-

;; because, it's too funny
;; this code should be put somewhere else
;; (add-hook 'mail-setup-hook 'spook)
(defadvice compose-mail (after insert-spook activate)
  (end-of-buffer)(spook)(backward-paragraph))


;; linum mode only when programming
(add-hook 'prog-mode-hook 'linum-mode)

;; before save hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'whitespace-cleanup)

; after save hooks
(add-hook 'after-save-hook 'auto-recompile-elisp-file)

;; aggressive indent
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

;; mark window as main window
;; (add-hook 'prog-mode-hook 'mark-this-window-as-main)
;; (add-hook 'text-mode-hook 'mark-this-window-as-main)


;; cancel minibuffer
(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; haskell
(add-hook 'haskell-mode-hook 'hi2-mode)

;; jedi
(add-hook 'python-mode-hook 'jedi:setup)
;; optional
(setq jedi:complete-on-dot t)
