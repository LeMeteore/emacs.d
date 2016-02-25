;; -*- coding: utf-8 -*-

;; emacs customization for command aliases and keybindings

(progn
  ;; set arrow keys in isearch. left/right is backward/forward, up/down is history. press Return to exit
  ;; (define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat )
  ;; (define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance )
  (define-key isearch-mode-map (kbd "<left>") 'isearch-repeat-backward) ; single key, useful
  (define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward) ; single key, useful
  )

(setq select-enable-clipboard t)

;; create new empty buffer
(global-set-key (kbd "C-S-n") 'my-empty-buffer)

;; next/previous user buffer
(global-set-key (kbd "<f8>") 'my-previous-user-buffer)
(global-set-key (kbd "<f9>") 'my-next-user-buffer)



;; split vertically| & horizontally--
(global-set-key (kbd "M-|") 'split-window-horizontally)
(global-set-key (kbd "M--") 'split-window-vertically)

;; describe function, mode, key, apropos command
(global-set-key (kbd "<f2> F") 'describe-function)
(global-set-key (kbd "<f2> M") 'describe-mode)
(global-set-key (kbd "<f2> K") 'describe-key)
(global-set-key (kbd "<f2> V") 'describe-variable)
(global-set-key (kbd "<f2> B") 'describe-bindings)
(global-set-key (kbd "<f2> I") 'info)
(global-set-key (kbd "<f2> a") 'apropos-command)
(global-set-key (kbd "<f2> w") 'where-is)
(global-set-key (kbd "<f2> c") 'comment-or-uncomment-region)
(global-set-key (kbd "<f2> ;") 'endless/comment-line)
(global-set-key (kbd "<f2> k") 'my-copy-line-or-region)
(global-set-key (kbd "<f2> g") 'magit-status) ;; not sure
(global-set-key (kbd "<f2> p") 'package-list-packages)
(global-set-key (kbd "<f2> q") 'kill-emacs)
(global-set-key (kbd "<f2> d") 'my-date)
(global-set-key (kbd "<f2> t") 'my-term)
(global-set-key (kbd "<f2> j") 'dired-jump)

(global-set-key (kbd "<f2> z") 'switch-to-minibuffer-window)

(global-set-key (kbd "<f2> r") (lambda () (interactive) (revert-buffer nil t)))


;; navigation by sexp
(global-set-key (kbd "<f2> <up>") 'backward-sexp)
(global-set-key (kbd "<f2> <down>") 'forward-sexp)

(global-set-key (kbd "C-x f") 'find-file-in-project)

;; shortening of often used commands
(defalias 'list-buffers 'ibuffer)
(defalias 'psql 'sql-postgres)
(defalias 'wsc 'whitespace-cleanup)
(defalias 'fd 'find-dired)
(defalias 'ff 'find-file)
(defalias 'pff 'projectile-find-file)
(defalias 'gf 'grep-find)
(defalias 'rb 'revert-buffer)
(defalias 'g 'grep)
(defalias 'mgs 'magit-status)
(defalias 'detach 'delete-frame) ;; C-x 5 0
(defalias 'redo 'undo-tree-redo)
;; I should normally stick to standard first & rest
(defalias 'head 'car)
(defalias 'tail 'cdr)


;; (defalias 'ipl ('describe-variable package-activated-list))
(global-set-key (kbd "<f2> <left>")  'windmove-left)
(global-set-key (kbd "<f2> <right>") 'windmove-right)
(global-set-key (kbd "<f2> <up>")    'windmove-up)
(global-set-key (kbd "<f2> <down>")  'windmove-down)

;; (key-chord-define-global "1u"     'undo)
;; (key-chord-define-global "''"     "`'\C-b")
;; (key-chord-define-global "1r"     'redo)
;; (key-chord-define-global "2f"     'find-file)
;; (key-chord-define-global "1d"     (lambda () (interactive) (dired "~/.emacs.d/")))
;; (key-chord-define-global "1q"     'ddg)
;; (key-chord-define-global "1s"     'save-buffer)
;; (key-chord-define-global "1f"     'my-open-file-fast)
;; (key-chord-define-global "1k"     'kill-buffer)
;; (key-chord-define-global "1b"     'ido-switch-buffer)

;; or some other keybinding...
;;(global-set-key (kbd "C-x F") 'djcb-find-file-as-root)

;; multiple info buffers keybindings
(global-set-key "\C-hr" 'my-emacs-info)
(global-set-key "\C-hj" 'my-elisp-info)
(global-set-key "\C-ho" 'my-org-info)

;; ;; quickly open some files
;; (global-set-key (kbd "<f7> h") (lambda () (interactive) (find-file "~/.emacs.d/lisp/init_hooks.el")))
;; (global-set-key (kbd "<f7> p") (lambda () (interactive) (find-file "~/.emacs.d/lisp/init_load_packages.el")))
;; (global-set-key (kbd "<f6> k") (lambda () (interactive) (find-file "~/.emacs.d/lisp/init_key_bindings.el")))

;; git
;; (global-set-key (kbd "<f6> a") 'git-add-current-buffer)


;; from emax redux
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))
;;(global-set-key (kbd "C-<TAB>") (lambda () (interactive) (other-window -1)))

;; use keypad as extra function key, from ergoemacs; not working within emacs-nox
(global-set-key (kbd "<kp-0>") 'delete-window)
(global-set-key (kbd "<kp-1>") 'delete-other-windows)
(global-set-key (kbd "<kp-2>") 'split-window-vertically)
(global-set-key (kbd "<kp-3>") 'xah-open-file-at-cursor)

;; recent files
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; semicolon will insert semicolon then newline only for rust
(defun my-rust-lang-mode-config ()
  (local-set-key (kbd ";") (lambda () (interactive) (insert ";")(newline-and-indent))))


;; keybiding available onyl in dired mode
;; (define-key dired-mode-map "c" 'dired-do-compress-to)


;;; esc always quits
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'keyboard-quit)

;;;
(define-key dired-mode-map "z" 'my-dired-get-size)
(define-key dired-mode-map "j" 'dired-next-line)
(define-key dired-mode-map "k" 'dired-previous-line)
(define-key dired-mode-map "`" 'my-dired-open-term)
(define-key dired-mode-map "r" 'dired-start-process)
