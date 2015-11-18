;; -*- coding: utf-8 -*-

;; because, it's too funny
;; this code should be put somewhere else
;; (add-hook 'mail-setup-hook 'spook)
(defadvice compose-mail (after insert-spook activate)
  (end-of-buffer)(spook)(backward-paragraph))


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
;; (add-hook 'python-mode-hook 'jedi:setup)

;; optional
;; (setq jedi:complete-on-dot t)

;; if the file is large, open it in fundamental mode
;; (add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)

(unless window-system
  (add-hook 'linum-before-numbering-hook
            (lambda ()
              (setq-local linum-format-fmt
                          (let ((w (length (number-to-string
                                            (count-lines (point-min) (point-max))))))
                            (concat "%" (number-to-string w) "d"))))))

(defun linum-format-func-nw (line)
  (concat
   (propertize (format linum-format-fmt line) 'face 'linum)
   (propertize " " 'face 'linum)))


(unless window-system
  (setq linum-format 'linum-format-func-nw))

;; (if (equal window-system "x")
;;     (setq linum-format 'linum-format-func)
;;   (setq linum-format 'linum-format-func-nw))

;;
;; (setq linum-format 'linum-format-func)

;; (when window-system
;;   (setq linum-format 'linum-format-func-nw))

;; linum mode only when pythoning
(add-hook 'python-mode-hook 'linum-mode)

;; linum mode only when programming
(add-hook 'prog-mode-hook 'linum-mode)

;; ????
(add-hook 'isearch-update-post-hook
          (lambda ()
            (when (> (length isearch-string) 0)
              ;;or whatever
              (recenter)))
          nil t)

;; rustlang configuration
(add-hook 'rust-mode-hook 'my-rust-lang-mode-config)
(add-hook 'rust-mode-hook
          (lambda ()
            (define-key rust-mode-map (kbd "<f6>") 'my-rust-save-compile-and-run)))

;; c programming configuration
;; (add-hook 'c-mode-hook 'my-c-lang-mode-config)
(add-hook 'c-mode-hook
          (lambda ()
            (define-key c-mode-map (kbd "<f5>") 'my-c-save-compile)))
(add-hook 'c-mode-hook
          (lambda ()
            (define-key c-mode-map (kbd "<f6>") 'my-c-run)))

(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map (kbd "<f6>") 'my-python-run)))


;; js and json stuff

(defun js-custom-settings ()
  (setq tab-width 2))

(add-hook 'js-mode-hook 'js-custom-settings)
