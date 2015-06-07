;; -*- coding: utf-8 -*-

;;(set-frame-font "FiraCode-Regular-12" )
;;(set-default-font "FiraCode-Regular-12")
;; useful only when you do not use emacsclient

;; the best way to set font when using emacsclient
;; is to edit your ~/.Xdefaults file & put:
;; Emacs.font: Inconsolata-12
;; then, from your terminal:
;; xrdb -merge ~/.Xdefaults

;; set default font in initial window and for any new window

;; (cond
;;  ((string-equal system-type "windows-nt") ; Microsoft Windows
;;   (when (member "DejaVu Sans Mono" (font-family-list))
;;     (add-to-list 'initial-frame-alist '(font . "DejaVu Sans Mono-10"))
;;     (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10"))))
;;  ((string-equal system-type "darwin") ; Mac OS X
;;   (when (member "DejaVu Sans Mono" (font-family-list))
;;     (add-to-list 'initial-frame-alist '(font . "DejaVu Sans Mono-10"))
;;     (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10"))))
;;  ((string-equal system-type "gnu/linux") ; linux
;;   (when (member "DejaVu Sans Mono" (font-family-list))
;;     (add-to-list 'initial-frame-alist '(font . "DejaVu Sans Mono-10"))
;;     (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10")))))


(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\(FIXME\\|TODO\\):"
                                       (1 font-lock-warning-face t))))))

;; (defface font-lock-sigil-face
;;   '((t (:foreground "#BD8800")))
;;   "Face to display sigils in.")

;; (font-lock-add-keywords 'text-mode
;;                         '(("\\$\\([_a-zA-Z]\\)" 1 'font-lock-type-face)))

;; (font-lock-add-keywords 'text-mode
;;                         '(("\\<\\(FIXME\\):" 1  font-lock-warning-face t)))


;; (add-hook 'prog-mode-hook
;;           (lambda ()
;;             (font-lock-add-keywords nil
;;                                     '(("\\<\\(FIXME\\)\\([[:digit:]]\\)"
;;                                        (1 font-lock-type-face t)
;;                                        (2 font-lock-warning-face t))))))

;; (add-hook 'text-mode-hook
;;           (lambda ()
;;             (font-lock-add-keywords nil
;;                                     '(("\\(\\$\\)[[:word:]]+ = [[:digit:]]+" 1
;;                                        font-lock-warning-face t)))))
