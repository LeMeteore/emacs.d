;; -*- coding: utf-8 -*-;;

(require 'ido)
(ido-mode t)

;; That’ll enable basic Ido support for files and buffers
;; and the very useful “flex matching” as well.
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Find File At Point, also known generally as “ffap”,
;; is an intelligent system for opening files, and URLs.
;; (setq ido-use-filename-at-point 'guess)

;;You can disable URL ffap support by toggling
;;(ido-use-url-at-point)

;; create new buffer if buffer name not exists
(setq ido-create-new-buffer 'always)

;;  customize the order in which files are sorted
;; when Ido displays them in the minibuffer
(setq ido-file-extensions-order '(".txt" ".py" ".el" ".emacs" ".json" ".ini" ".cfg" ".cnf"))


(global-set-key
 "\M-x"
 (lambda ()
   (interactive)
   (call-interactively
    (intern
     (ido-completing-read
      "M-x "
      (all-completions "" obarray 'commandp))))))
