;; -*- coding: utf-8 -*-

;; emacs customization for command aliases and keybindings

;; create new empty buffer
(global-set-key (kbd "C-n") 'my-empty-buffer)

;; M-x remapped to f8 keky
(global-set-key (kbd "<f8>") 'execute-extended-command)

;; Linux, menu/apps key
(global-set-key (kbd "<apps>") 'execute-extended-command)

;; split vertically
(global-set-key (kbd "M-|") 'split-window-horizontally)

;; split horizontally
(global-set-key (kbd "M--") 'split-window-vertically)

;; describe function, mode, key, apropos command
(global-set-key (kbd "<f2> f") 'describe-function)
(global-set-key (kbd "<f2> m") 'describe-mode)
(global-set-key (kbd "<f2> k") 'describe-key)
(global-set-key (kbd "<f2> a") 'apropos-command)
(global-set-key (kbd "<f2> c") 'comment-or-uncomment-region) ;; (un)comment
(global-set-key (kbd "<f2> s") 'magit-status) ;; not sure

;; shortening of often used commands
(defalias 'psql 'sql-postgres)
(defalias 'wsc 'whitespace-cleanup)
(defalias 'fd 'find-dired)
(defalias 'ff 'find-file)
(defalias 'pff 'projectile-find-file)
(defalias 'gf 'grep-find)
(defalias 'rb 'revert-buffer)
(defalias 'g 'grep)
(defalias 'mgs 'magit-status)
(defalias 'ddg 'ddg)

;; abbrev
(setq abbrev-file-name             ;; tell emacs where to read abbrev
        "~/.emacs.d/abbrev_defs")    ;; definitions from...

(setq save-abbrevs t)              ;; save abbrevs when files are saved
                                     ;; you will be asked before the abbreviations are saved
