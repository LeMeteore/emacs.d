;; -*- coding: utf-8 -*-;;


(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)


(defvar my-packages
  '(color-theme db-pg db kv magit-push-remote magit-tramp
                magit git-rebase-mode git-commit-mode pg
                pretty-lambdada projectile pkg-info epl dash
                python-mode rich-minority s yasnippet yasnippet-bundle)
  "A list of packages to ensure are installed at launch.")


(defun my-packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))


(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))
