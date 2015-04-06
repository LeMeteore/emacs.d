;; -*- coding: utf-8 -*-


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
