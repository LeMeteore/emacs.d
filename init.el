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
 '(custom-enabled-themes (quote (material)))
 '(custom-safe-themes
   (quote
    ("d4e9f95acd51433b776f1127143bbc7d0f1d41112d547e5b7a9a506be369dc39" "603a9c7f3ca3253cb68584cb26c408afcf4e674d7db86badcfe649dd3c538656" "0aa12caf6127772c1a38f7966de8258e7a0651fb6f7220d0bbb3a0232fba967f" "870a63a25a2756074e53e5ee28f3f890332ddc21f9e87d583c5387285e882099" default)))
 '(explicit-shell-file-name "/bin/bash")
 '(fci-rule-color "#37474f")
 '(hl-sexp-background-color "#1c1f26")
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
