;; -*- coding: utf-8 -*-
;; If your init file contains code which depends on packages being available,
;; then you need to call (package-initialize) beforehand.
;;(setq package-enable-at-startup nil)
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/lisp")
;; the exact path to mu4e
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

(load "init_load_packages")
(load "init_elisp_functions")
(load "init_settings")
(load "init_key_bindings")
(load "init_hooks")
(load "init_modeline")
(load "init_python")
(load "init_ido")
(load "init_mail")
(load "init_font_lock")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#ffffff" "#f36c60" "#8bc34a" "#fff59d" "#4dd0e1" "#b39ddb" "#81d4fa" "#263238"))
 '(custom-enabled-themes (quote (misterioso)))
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "d4e9f95acd51433b776f1127143bbc7d0f1d41112d547e5b7a9a506be369dc39" default)))
 '(explicit-shell-file-name "/bin/bash")
 '(fci-rule-color "#37474f")
 '(hl-sexp-background-color "#1c1f26")
 '(package-selected-packages
   (quote
    (yasnippet w3m use-package undo-tree tao-theme smartparens seq rustfmt restclient racer pyvenv python-mode projectile pretty-lambdada openwith nim-mode nginx-mode names multiple-cursors move-text material-theme markdown-mode magit-tramp magit-push-remote key-chord json-reformat jedi jabber hi2 helm haskell-mode guide-key go-mode git-rebase-mode git-commit-mode find-file-in-project f elixir-mix drag-stuff db-pg color-theme cargo alchemist aggressive-indent ag ack)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
