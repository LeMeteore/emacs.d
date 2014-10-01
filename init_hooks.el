;; -*- coding: utf-8 -*-

; linum mode only when programming
(add-hook 'prog-mode-hook 'linum-mode)

;; before save hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'whitespace-cleanup)

; after save hooks
;(add-hook 'after-save-hook 'byte-compile-current-buffer)
(add-hook 'after-save-hook (when (equal major-mode 'Emacs-Lisp) 'byte-compile-current-buffer))
