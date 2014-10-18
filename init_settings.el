;; -*- coding: utf-8 -*-

;; end files with a newline
(setq require-final-newline t)

;; scroll one line at a time
(setq scroll-step 1)

;; initial major mode, text, instead of lisp
(setq initial-major-mode (quote text-mode))

;; backup files
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs_backup")))

;; Save all backup file in this directory.
;(setq backup-directory-alist (quote ((".*" . "~/.emacs.d/emacs_backup/"))))

;; no toolbar
;;(tool-bar-mode -1)
 ;; no menubar!
(menu-bar-mode -1)

;; yes or no to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; default to ssh for tramp
(setq tramp-default-method "ssh")

;;; uncomment this line to disable loading of "default.el" at startup
(setq inhibit-default-init t)

;;;; open with single window, no startup screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)

;; enable visual feedback on selections
(setq transient-mark-mode t)
(transient-mark-mode 1) ; highlight text selection

; delete seleted text when typing
(delete-selection-mode 1)

; turn on syntax coloring
(global-font-lock-mode 1)

;(setq search-highlight           t) ; Highlight search object
;(setq query-replace-highlight    t) ; Highlight query object
;(setq mouse-sel-retain-highlight t) ; Keep mouse high-lightening

; Set region background color
(set-face-background 'region "purple")
;;(set-background-color        "wheat3") ; Set emacs bg color
(set-face-foreground 'minibuffer-prompt "white") ;;

;; show matching parens by default
(show-paren-mode t)

;; automatic pair insert
(electric-pair-mode 1)

 ; display line numbers in margin. New in Emacs 23
;(global-linum-mode 1)

;; display column number
(column-number-mode 1)

; keep a list of recently opened files
(recentf-mode 1)
(setq-default recent-save-file "~/.emacs.d/recentf")
; 1 for on, 0 for off | to have lines soft wrapped at word boundary
(global-visual-line-mode 1)

;; make indentation commands use space only (never tab character)
(setq-default indent-tabs-mode nil)

;; set default tab char's display width to 4 spaces
(setq-default tab-width 2) ; emacs 23.1, 24.2, default to 8

;; set current buffer's tab char's display width to 4 spaces
(setq-default tab-width 4)

;; make return key also do indent, for current buffer only
;;(electric-indent-local-mode 1)

;; make return key also do indent, globally, not so cool
;; (electric-indent-mode 1)

;; me
(setq user-full-name "Nsukami _")
(setq user-mail-address "ndkpatt@gmail.com")

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; will make the display of date and time persistent
;;(setq display-time-day-and-date t) (display-time)
;; Time in 24 hour format, plus day and date.
(setq display-time-day-and-date t
display-time-24hr-format t
display-time-use-mail-icon t
display-time-mail-file nil)
(display-time)

;; utf-8 all the time
(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

; Don't want any startup message
(setq inhibit-startup-message   t)

; Don't want any backup files
;;(setq make-backup-files         nil)

; Don't want any .saves files
(setq auto-save-list-file-name  nil)

; Don't want any auto saving
(setq auto-save-default         nil)

;; update the buffer whenever the disk file gets updated independent of Emacs
(setq auto-revert-mode t)

;; because sometimes i need to edit php files
(load "php/php-mode")

;; display a custom welcome message
(display-startup-echo-area-message )

;; custom scratch message
(setq initial-scratch-message ";; I am your editor, please describe your program.\n")

;; automatically show completions for all prompts
;; t does auto completion for any command that prompts for a list of completions
(icomplete-mode 1)

;; show completion and mode for find file and switch to buffer
(ido-mode 1)

;; turn on abbrev mode globally, not really sure
(setq-default abbrev-mode t)

;; current line on globally
(global-hl-line-mode 1)

;; just underline, nothing else
(set-face-attribute hl-line-face nil :underline t)
(set-face-foreground 'highlight nil)
(set-face-background 'highlight nil)

;; dired, display file size in metric prifex of k,
;; and display date in yyyy-mm-dd | when in dired, C-u s to change listing option
(setq dired-listing-switches "-Al --si --time-style long-iso")

;; abbrev
(setq abbrev-file-name             ;; tell emacs where to read abbrev
        "~/.emacs.d/abbrev_defs")    ;; definitions from...

;; save abbrevs when files are saved
;; you will be asked before the abbreviations are saved
(setq save-abbrevs t)
