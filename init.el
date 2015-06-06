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
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (misterioso))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
