;; -*- coding: utf-8 -*-

;; magit status, please use the whole screen
(setq magit-post-display-buffer-hook
      #'(lambda ()
          (when (derived-mode-p 'magit-status-mode)
            (delete-other-windows))))

;; because, it's too funny
;; this code should be put somewhere else
;; (add-hook 'mail-setup-hook 'spook)
(defadvice compose-mail (after insert-spook activate)
  (goto-char (point-max))(spook)(backward-paragraph))


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

;; golang
(add-hook 'go-mode-hook
          'my-setup-go-env)
(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook  'gofmt-before-save)))
(add-hook 'go-mode-hook
          (lambda ()
            (define-key go-mode-map (kbd "<f6>") 'my-go-run)))
(add-hook 'go-mode-hook
          (lambda ()
            (define-key go-mode-map (kbd "C-c C-r") 'go-remove-unused-imports)))
(add-hook 'go-mode-hook
          (lambda ()
            (define-key go-mode-map (kbd "C-c C-g") 'go-goto-imports)))
(add-hook 'go-mode-hook
          (lambda ()
            (define-key go-mode-map (kbd "C-c C-f") 'gofmt)))
(add-hook 'go-mode-hook
          (lambda ()
            (define-key go-mode-map (kbd "C-c C-k") 'godoc)))

;; (defun my-go-mode-before-save-hook ()
;;   (when (eq major-mode 'go-mode)
;;     (gofmt-before-save)
;;     (message "It's never too early to start saving (go code)!")))
;; (add-hook 'before-save-hook #'my-go-mode-before-save-hook)

;; python
(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map (kbd "<f6>") 'py-execute-buffer)))
(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map (kbd "<f7>") 'my-python-add-breakpoint)))
(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map (kbd "\C-c\ \C-x") 'my-close-and-kill-next-pane)))

;; a function to prettify symbol
(defun my-add-pretty ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(
          ("lambda" . 955) ; λ
          ("->" . 8594)    ; →
          ("=>" . 8658)    ; ⇒
          ("map" . 8614)   ; ↦
          ("import ipdb; ipdb.set_trace()" . 9632) ; ■
          )))
(add-hook 'python-mode-hook   (global-prettify-symbols-mode 1))
(add-hook 'python-mode-hook 'my-add-pretty)


;; js and json stuff
(defun js-custom-settings ()
  (setq tab-width 2))
(add-hook 'js-mode-hook 'js-custom-settings)

;; shell function to increase font size
(defun shell-hook ()
  (text-scale-increase 1.1))
(add-hook 'term-mode-hook 'shell-hook)

;;
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)
