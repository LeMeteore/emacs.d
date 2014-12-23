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
(global-set-key (kbd "<f2> c") 'comment-or-uncomment-region) ;; (un)comment
(global-set-key (kbd "<f2> s") 'magit-status) ;; not sure
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

;(defalias 'ipl ('describe-variable package-activated-list))

(global-set-key (kbd "<f2> <left>")  'windmove-left)
(global-set-key (kbd "<f2> <right>") 'windmove-right)
(global-set-key (kbd "<f2> <up>")    'windmove-up)
(global-set-key (kbd "<f2> <down>")  'windmove-down)

(key-chord-define-global "zz"     'undo)
(key-chord-define-global "yy"     'redo)
(key-chord-define-global "vv"     'find-file)
(key-chord-define-global "hh"     'dired)
(key-chord-define-global "qq"     'ddg)

(global-set-key "\M-9" 'backward-sexp)
(global-set-key "\M-0" 'forward-sexp)
(global-set-key "\M-1" 'delete-other-windows)
