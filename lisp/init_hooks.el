;; -*- coding: utf-8 -*-

; linum mode only when programming
(add-hook 'prog-mode-hook 'linum-mode)

;; before save hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'whitespace-cleanup)

; after save hooks
(add-hook 'after-save-hook 'auto-recompile-elisp-file)

;; aggressive indent
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
