;; -*- coding: utf-8 -*-;;

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; If your init file contains code which depends on packages being available,
;; then you need to call (package-initialize) beforehand.
;;(setq package-enable-at-startup nil)
(package-initialize)

;; find list of installed package w/
;; describe-variable package-activated-list
(defvar my-packages
  '(color-theme db-pg db kv magit-push-remote magit-tramp
                magit git-rebase-mode git-commit-mode pg
                pretty-lambdada projectile pkg-info epl dash
                python-mode rich-minority s yasnippet
                use-package key-chord undo-tree guide-key move-text
                openwith ack ag aggressive-indent nginx-mode multiple-cursors
                smartparens with-editor haskell-mode find-file-in-project jedi
                flycheck neotree interaction-log)
  "A list of packages to ensure are installed at launch.")


;; install package if missing, for package in package list
(dolist (p my-packages)
  (unless (package-installed-p p)
      (message "%s" "Get latest versions of all packages...")
      (package-refresh-contents)
      (message "%s" " done.")
      (package-install p)))
