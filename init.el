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
