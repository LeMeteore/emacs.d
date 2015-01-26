;; -*- coding: utf-8 -*-

;; emacs customization for command aliases and keybindings


 (setq x-select-enable-clipboard t)

;; create new empty buffer
(global-set-key (kbd "C-n") 'my-empty-buffer)

;; next/previous user buffer
(global-set-key (kbd "<f8>") 'my-previous-user-buffer)
(global-set-key (kbd "<f9>") 'my-next-user-buffer)



;; split vertically| & horizontally--
(global-set-key (kbd "M-|") 'split-window-horizontally)
(global-set-key (kbd "M--") 'split-window-vertically)

;; describe function, mode, key, apropos command
(global-set-key (kbd "<f2> f") 'describe-function)
(global-set-key (kbd "<f2> m") 'describe-mode)
(global-set-key (kbd "<f2> k") 'describe-key)
(global-set-key (kbd "<f2> v") 'describe-variable)
(global-set-key (kbd "<f2> b") 'describe-bindings)
(global-set-key (kbd "<f2> a") 'apropos-command)
(global-set-key (kbd "<f2> w") 'where-is)
(global-set-key (kbd "<f2> C") 'comment-or-uncomment-region)
(global-set-key (kbd "<f2> c") 'endless/comment-line)
(global-set-key (kbd "<f2> s") 'magit-status) ;; not sure
(global-set-key (kbd "<f2> p") 'package-list-packages)
(global-set-key (kbd "<f2> q") 'kill-emacs)
(global-set-key (kbd "<f2> d") 'my-date)
(global-set-key (kbd "<f2> i") 'info)
(global-set-key (kbd "<f2> z") 'switch-to-minibuffer-window)
(global-set-key (kbd "<f2> <home>") 'beginning-of-buffer)
(global-set-key (kbd "<f2> <end>") 'end-of-buffer)

(global-set-key (kbd "<f5>") 'revert-buffer)


;; navigation by sexp
(global-set-key (kbd "<f2> <up>") 'backward-sexp)
(global-set-key (kbd "<f2> <down>") 'forward-sexp)



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
;; I should normally stick to standard first & rest
(defalias 'head 'car)
(defalias 'tail 'cdr)


;; (defalias 'ipl ('describe-variable package-activated-list))
(global-set-key (kbd "<f2> <left>")  'windmove-left)
(global-set-key (kbd "<f2> <right>") 'windmove-right)
(global-set-key (kbd "<f2> <up>")    'windmove-up)
(global-set-key (kbd "<f2> <down>")  'windmove-down)

(key-chord-define-global "zz"     'undo)
(key-chord-define-global "yy"     'redo)
(key-chord-define-global "vv"     'find-file)
(key-chord-define-global "hh"     (lambda () (interactive) (dired "~/.emacs.d/")))
(key-chord-define-global "qq"     'ddg)


;; or some other keybinding...
;;(global-set-key (kbd "C-x F") 'djcb-find-file-as-root)

;; multiple info buffers keybindings
(global-set-key "\C-hr" 'my-emacs-info)
(global-set-key "\C-hj" 'my-elisp-info)
(global-set-key "\C-ho" 'my-org-info)

;; quickly open some files
(global-set-key (kbd "<f6> h") (lambda () (interactive) (find-file "~/.emacs.d/lisp/init_hooks.el")))
(global-set-key (kbd "<f6> p") (lambda () (interactive) (find-file "~/.emacs.d/lisp/init_load_packages.el")))
(global-set-key (kbd "<f6> k") (lambda () (interactive) (find-file "~/.emacs.d/lisp/init_key_bindings.el")))
(global-set-key (kbd "<f6> k") (lambda () (interactive) (find-file "~/.emacs.d/lisp/init_key_bindings.el")))
;; git
(global-set-key (kbd "<f6> a") 'git-add-current-buffer)


;; from emax redux
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))
;;(global-set-key (kbd "C-<TAB>") (lambda () (interactive) (other-window -1)))

;; use keypad as extra function key, from ergoemacs; not working within emacs-nox
(global-set-key (kbd "<kp-0>") 'delete-window)
(global-set-key (kbd "<kp-1>") 'delete-other-windows)
(global-set-key (kbd "<kp-2>") 'split-window-vertically)
(global-set-key (kbd "<kp-3>") 'xah-open-file-at-cursor)
