;; -*- coding: utf-8 -*-

(require 'cl)
(require 'recentf)
(defun find-last-killed-file ()
  (interactive)
  (let ((active-files (loop for buf in (buffer-list)
                            when (buffer-file-name buf) collect it)))
    (loop for file in recentf-list
          unless (member file active-files) return (find-file file))))

(define-key global-map (kbd "C-S-t") 'find-last-killed-file)


(defvar my-filelist nil "alist for files i need to open frequently. Key is a short abbrev string, Value is file path string.")

(setq my-filelist
      '(
        ("github" . "~/GITHUB/" )
        ("init" . "~/.emacs.d/" )
        ("wappa" . "~/envs/wappa/source/wappa/" )
        ;; more here
        ) )

(defun my-open-file-fast ()
  "Prompt to open a file from `my-filelist'.
URL `http://ergoemacs.org/emacs/emacs_hotkey_open_file_fast.html'
Version 2015-04-23"
  (interactive)
  (let ((ξabbrevCode
         (ido-completing-read "Open:" (mapcar (lambda (ξx) (car ξx)) my-filelist))))
    (find-file (cdr (assoc ξabbrevCode my-filelist)))))

;; create a list named p1
;; return a sorted list by string desc
;; the list to sort is a list of names as string (mapcar 'symbol-name (mapcar 'car package-alist))
;; the original list is a list of installed package (mapcar 'car package-alist)
;; (dolist (p1 (sort (mapcar 'symbol-name (mapcar 'car package-alist)) 'string<))
;;   ;; print p1
;;   (message "%s" p1))


(defun my-delete-surrounded-parens ()
  (interactive)
  ;; save where region begins & ends
  (let ((beginning (region-beginning))
        (end (region-end)))
    (cond ((not (eq (char-after beginning) ?\())
           ;; if region not begins by (, trigger error
           (error "Char at region-begin not an open-parens"))
          ((not (eq (char-before end) ?\)))
           ;; if region not ends by ), trigger error
           (error "Char at region-end not a close-parens"))
          ;; save mark, pt, current buffer & execute body
          ((save-excursion
             (goto-char beginning)
             (forward-sexp)
             (not (eq (point) end)))
           ;; if parens are not balanced, trigger error
           (error "parens not balanced"))
          (t (save-excursion
               (goto-char end)
               (delete-char -1)
               (goto-char beginning)
               (delete-char 1))))))

(defun my-select-current-word ()
  "Select the current word."
  (interactive)
  (beginning-of-thing 'symbol)
  (push-mark (point) nil t)
  (end-of-thing 'symbol)
  (exchange-point-and-mark))

(defun my-horizontal-recenter ()
  "make the point horizontally centered in the window"
  (interactive)
  ;; middle of screen
  (let ((mid (/ (window-width) 2))
        ;; current column
        (cur (current-column)))
    ;; if current position is after middle of window
    (if (< mid cur)
        ;; scroll from left to right
        (set-window-hscroll (selected-window) (- cur mid))
      )))

(defun my-vertical-recenter ()
  "make the point vertically centered in the window"
  (interactive)
  ;; middle of screen
  (let ((mid (/ (window-height) 2))
    ;; current line
    (cur (+ 1 (count-lines 1 (point)))))
    (message "mid is: %d" mid)
    (message "cur is: %d" cur)
    ;; if current position is after middle of window
    (if (< mid cur)
    ;; scroll from
    (set-window-vscroll (selected-window) (- cur mid))
      )))


(defun browse-file-directory ()
  "Open the current file's directory however the OS would."
  (interactive)
  (if default-directory
      (browse-url-of-file (expand-file-name default-directory))
    (error "No `default-directory' to open")))

(defun crontab-e ()
  (interactive)
  (with-editor-async-shell-command "crontab -e"))

;; (defun incrontab-e ()
;;   (interactive)
;;   (with-editor-async-shell-command "incrontab -e"))

(defun test-letter ()
  (interactive)
  (if (looking-at "[a-z-A-Z]")
      (message "This is a letter")
    (message "This is not a letter")))

(defun test-letter1 ()
  (interactive)
  (let ((char (char-after)))
    (if (and (eq (char-syntax char) ?w)
             (or (> char ?9)
                 (< char ?1)))
        (message "This is a letter")
      (message "This is not a letter"))))


(defun count-words (start end)
  "Print number of words in the region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (count-matches "\\sw+"))))

(defun wc (&optional start end)
  "Prints number of lines, words and characters in region or whole buffer."
  (interactive)
  (let ((n 0)
        (start (if mark-active (region-beginning) (point-min)))
        (end (if mark-active (region-end) (point-max))))
    (save-excursion
      (goto-char start)
      (while (< (point) end) (if (forward-word 1) (setq n (1+ n)))))
    (message "%3d %3d %3d" (count-lines start end) n (- end start))))

;; http://endlessparentheses.com/implementing-comment-line.html
(defun endless/comment-line (n)
  "Comment or uncomment current line and leave point after it.
With positive prefix, apply to N lines including current one.
With negative prefix, apply to -N lines above."
  (interactive "p")
  (comment-or-uncomment-region
   (line-beginning-position)
   (goto-char (line-end-position n)))
  (forward-line 1)
  (back-to-indentation))

(defun switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(defun move-region-to-other-window (start end)
  "Move selected text to other window"
  (interactive "r")
  (if (use-region-p)
      (let ((count (count-words-region start end)))
        (save-excursion
          (kill-region start end)
          (other-window 1)
          (yank)
          (newline))
        (other-window -1)
        (message "Moved %s words" count))
    (message "No region selected")))

(defun my-mark-this-window-as-main ()
  "Mark the current window as the main window."
  (interactive)
  (mapc (lambda (win) (set-window-parameter win 'main nil))
        (window-list))
  (set-window-parameter nil 'main t))

(defun my-get-main-window()
  "Find and return the main window or nil if non exists."
  (cl-find-if (lambda (win) (window-parameter win 'main)) (window-list)))

(defun my-just-my-main-window ()
  "Show only the main window"
  (interactive)
  (delete-other-windows (my-get-main-window)))


(defvar toggle-window-backward nil)
(defun my-last-window ()
  (interactive)
  (cond
   (toggle-window-backward
    (setq toggle-window-backward nil)
    (other-window -1))
   (t
    (setq toggle-window-backward t)
    (other-window 1))))


(defun djcb-find-file-as-root ()
  "Like `ido-find-file, but automatically edit the file with
root-privileges (using tramp/sudo), if the file is not writable by
user."
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))


(defun git-add-current-buffer ()
  "call 'git add [current-buffer]'"
  (interactive)
  (let* ((buffile (buffer-file-name))
         (output (shell-command-to-string
                  (concat "git add " (buffer-file-name)))))
    (message (if (not (string= output ""))
                 output
               (concat "Added " buffile)))))


;; function to return the name of the parent directory of the currently visited file
(defun buffer-file-parent-directory ()
  ;; if the buffer is visiting a file
  (when buffer-file-name
    (concat "[" (file-name-nondirectory (directory-file-name (file-name-directory buffer-file-name))) "]")))


;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs)
  )

;;
(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  ;;(when (equal major-mode 'Emacs-Lisp)) (byte-compile-file buffer-file-name))
  (when (and (equal major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(defun auto-recompile-elisp-file ()
  (interactive)
  (when (and buffer-file-name (string-match "\\.el" buffer-file-name))
    (let ((byte-file (concat buffer-file-name "\\.elc")))
      (if (or (not (file-exists-p byte-file))
              (file-newer-than-file-p buffer-file-name byte-file))
          (byte-compile-file buffer-file-name)))))

;; (defun my-date-again ()
;;   "insert date"
;;   (interactive)
;;   (when (use-region-p)
;;     (delete-region (region-beginning) (region-end) )
;;     )
;;   (insert
;;    (concat
;;     (format-time-string "%c")
;;     ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
;;      (format-time-string "%z")))))

(defun my-date ()
  "insert date"
  (interactive)
  (when (use-region-p)
    (delete-region (region-beginning) (region-end) )
    )
  (insert
   (format-time-string "%c")))

(defun my-empty-buffer ()
  "Open a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))


(defun ssh-add-process-filter (process string)
  (save-match-data
    (if (string-match ":\\s *\\'" string)
        (process-send-string process (concat (read-passwd string) "\n"))
      (message "%s" string))))


(defun ssh-add (key-file)
  "Run ssh-add to add a key to the running SSH agent. Let Emacs prompt for the passphrase."
  ;; enter the path to an existing pubkey
  (interactive "fAdd key: \n")
  ;; def 2 vars, process-connextion-type, default true & process
  (let ((process-connection-type t)
        process)
    ;; do this
    (unwind-protect
        ;; eval this, and return value of last seq
        (progn
          ;; start ssh-add in a subproc, buf=nil arg=pubkey
          (setq process (start-process "ssh-add"
                                       nil
                                       "ssh-add"
                                       (expand-file-name key-file)))
          ;; give ssh-add process the filter function
          ;; wich will get the ssh-add output
          (set-process-filter process 'ssh-add-process-filter)
          ;; while, there is output, do process: ssh-add
          (while (accept-process-output process)))
      ;; whatever the exit status, do this
      ;; if our process is still running
      (if (eq (process-status process) 'run)
          ;; kill it
          (kill-process process)))))


(defun ddg ()
  "Look up the word under cursor or selected region in ddg. This command switches you to firefox."
  (interactive)
  ;; myWord & myUrl variables
  (let (myWord myUrl)
    ;; myWord is selected region or word the point is under
    (setq myWord
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (thing-at-point 'symbol)))
    ;; replace space by plus in the word
    (setq myWord (replace-regexp-in-string " " "+" myWord))
    (setq myUrl (concat "https://duckduckgo.com/?t=lm&q=" myWord))
    (browse-url-firefox myUrl)
    ))

(defun duckduckgo (what)
  "Use ddg to search for WHAT."
  (interactive "sSearch: ")
  (browse-url-firefox (concat "https://duckduckgo.com/?t=lm&q="
                              (url-unhex-string what))))


(defun display-startup-echo-area-message ()
  (message "Welcome mister Nsukami_, I hope you're doing fine.!"))


(defun my-next-user-buffer ()
  "Switch to the next user buffer.
 (buffer name does not start with “*”.)"
  (interactive)
  (next-buffer)
  ;; i = 0
  (let ((i 0))
    ;; while buffer name starts w * and i < 20
    (while (and (string-equal "*" (substring (buffer-name) 0 1)) (< i 20))
      ;; i = i+1 then go to next buffer
      (setq i (1+ i)) (next-buffer))))

(defun my-previous-user-buffer ()
  "Switch to the previous user buffer.
 (buffer name does not start with “*”.)"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (string-equal "*" (substring (buffer-name) 0 1)) (< i 20))
      (setq i (1+ i)) (previous-buffer))))

(defun my-copy-line-or-region ()
  "Copy current line, or text selection.
When `universal-argument' is called first, copy whole buffer (but respect `narrow-to-region')."
  (interactive)
  (let (p1 p2)
    (if (null current-prefix-arg)
        (progn (if (use-region-p)
                   (progn (setq p1 (region-beginning))
                          (setq p2 (region-end)))
                 (progn (setq p1 (line-beginning-position))
                        (setq p2 (line-end-position)))))
      (progn (setq p1 (point-min))
             (setq p2 (point-max))))
    (kill-ring-save p1 p2)))


(defun my-cut-line-or-region ()
  "Cut current line, or text selection.
When `universal-argument' is called first, cut whole buffer (but respect `narrow-to-region')."
  (interactive)
  (let (p1 p2)
    (if (null current-prefix-arg)
        (progn (if (use-region-p)
                   (progn (setq p1 (region-beginning))
                          (setq p2 (region-end)))
                 (progn (setq p1 (line-beginning-position))
                        (setq p2 (line-beginning-position 2)))))
      (progn (setq p1 (point-min))
             (setq p2 (point-max))))
    (kill-region p1 p2)))


;; open multiple info buffers

;; (defun my-info (topic bufname)
;;   "Read documentation for TOPIC in the info system.  Name the
;; buffer BUFNAME.  If that buffer is already present, just switch
;; to it."
;;   (if (get-buffer bufname)
;;       (switch-to-buffer bufname)
;;     (info topic bufname)))



;; (defun my-org-info ()
;;   "Jump to Org-mode info buffer, creating it if necessary.  This
;; is *not* the buffer \\[info] would jump to, it is a separate
;; entity."
;;   (interactive)
;;   (my-info "org" "*Org Info*"))



;; (defun my-elisp-info ()
;;   "Jump to Emacs Lisp info buffer, creating it if necessary.  This
;; is *not* the buffer \\[info] would jump to, it is a separate
;; entity."
;;   (interactive)
;;   (my-info "elisp" "*Emacs Lisp Info*"))



;; (defun my-emacs-info ()
;;   "Jump to Emacs Lisp info buffer, creating it if necessary.  This
;; is *not* the buffer \\[info] would jump to, it is a separate
;; entity."
;;   (interactive)
;;   (my-info "emacs" "*Emacs Info*"))

;; from ergoemacs http://ergoemacs.org/emacs/emacs_kill-ring.html

(defun my-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push erased text to kill-ring."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push erased text to kill-ring."
  (interactive "p")
  (my-delete-word (- arg)))

(defun my-delete-line ()
  "Delete text from current position to end of line char."
  (interactive)
  (delete-region
   (point)
   (save-excursion (move-end-of-line 1) (point)))
  (delete-char 1))

(defun my-delete-line-backward ()
  "Delete text between the beginning of the line to the cursor position."
  (interactive)
  (let (x1 x2)
    (setq x1 (point))
    (move-beginning-of-line 1)
    (setq x2 (point))
    (delete-region x1 x2)))


;; (defadvice kill-region (before slick-cut activate compile)
;;   "When called interactively with no active region, kill a single line instead."
;;   (interactive
;;    (if mark-active
;;        (list (region-beginning) (region-end))
;;      (list (line-beginning-position) (line-beginning-position 2)))))

;; (defadvice kill-ring-save (before slick-copy activate compile)
;;   "When called interactively with no active region, copy a single line instead."
;;   (interactive
;;    (if mark-active
;;        (list (region-beginning) (region-end))
;;      (message "Copied line")
;;      (list (line-beginning-position) (line-beginning-position 2)))))

;; (defun slick-cut (beg end)
;;   (interactive
;;    (if mark-active
;;        (list (region-beginning) (region-end))
;;      (list (line-beginning-position) (line-beginning-position 2)))))

;; (advice-add 'kill-region :before #'slick-cut)

;; (defun slick-copy (beg end)
;;   (interactive
;;    (if mark-active
;;        (list (region-beginning) (region-end))
;;      (message "Copied line")
;;      (list (line-beginning-position) (line-beginning-position 2)))))

;; (advice-add 'kill-ring-save :before #'slick-copy)

(defun my-wappa-project-grep ()
  (interactive)
  (helm-do-grep-1 '("~/envs/w/source/wappa")
                  '(4)
                  nil
                  '("*.py")))

(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))

;; a function to rust save, compile, run
(defun my-rust-save-compile-and-run ()
  (interactive)
  (save-buffer)
  (if (locate-dominating-file (buffer-file-name) "Cargo.toml")
      (compile "cargo run")
    (compile
     (format "rustc %s & %s"
             (buffer-file-name)
             (file-name-sans-extension (buffer-file-name))))))

;; a function to save compile c program
;; (defun my-c-save-compile ()
;;   (interactive)
;;   (save-buffer)
;;   (compile
;;    (format "cc -Wall -g    %s -o ./bin/%s"
;;            (buffer-file-name)
;;            (file-name-sans-extension (buffer-file-name)))))

(defun my-c-save-compile ()
  (interactive)
  (save-buffer)
  (compile
   ;; (format "cc -Wall -g    %s -o ./bin/%s"
   (format "cc -Wall -Wextra -pedantic -std=c11 -g    %s -o ./bin/%s"
           (buffer-file-name)
           (file-name-base (buffer-file-name)))))

;; a function to run c program
(defun my-c-run ()
  (interactive)
  (compile (format "./bin/%s"
                   (file-name-base (buffer-file-name)))))

;; a function to run c program when used input is needed
(defun my-c-run-comint ()
  (interactive)
  (setq current-prefix-arg '(4))
  (call-interactively 'compile))

;; a function to run go scripts
(defun my-go-run ()
  (interactive)
  (compile (format "go run %s"
                   (buffer-file-name))))

;; a function to set gopath
(defun my-setup-go-env ()
  (require 'go-mode)
  (defvar my-gopath)
  (let ((my-gopath (shell-command-to-string "echo -n $GOPATH"))))
  (setenv "GOPATH" my-gopath))

;; a function to run python scripts
(defun my-python-run ()
  (setq compilation-scroll-output t)
  (interactive)
  (save-buffer)
  (compile
   (format "python3.5 %s"
           (buffer-file-name))))

;; a function to insert breakpoints inside python script
(defun my-python-add-breakpoint ()
  "Add a break point"
  (interactive)
  (newline-and-indent)
  (insert "import ipdb; ipdb.set_trace()"))


(defun my-lsb-release ()
  (interactive)
  (shell-command (format "lsb_release -a | sed -n '$p' | awk '{print $2}'")))


(defun my-close-and-kill-this-pane ()
  "If there are multiple windows, then close this pane and kill the buffer in it also."
  (interactive)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))

(defun my-close-and-kill-next-pane ()
  "If there are multiple windows, then close the other pane and kill the buffer in it also."
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))

(defun xah-open-in-terminal ()
  "Open the current dir in a new terminal window.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-12-10"
  (interactive)
  (cond
   ((string-equal system-type "gnu/linux")
    (let ((process-connection-type nil))
      (start-process "" nil "x-terminal-emulator"
                     (concat "--working-directory=" default-directory) )))))

(defun xah-open-in-desktop ()
  "Show current file in desktop (OS's file manager).
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-11-30"
  (interactive)
  (cond
   ((string-equal system-type "gnu/linux")
    (let (
          (process-connection-type nil)
          (openFileProgram (if (file-exists-p "/usr/bin/gvfs-open")
                               "/usr/bin/gvfs-open"
                             "/usr/bin/xdg-open")))
      (start-process "" nil openFileProgram "."))
    ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. ⁖ with nautilus
    )))


(defun xah-copy-rectangle-to-kill-ring (φbegin φend)
  "Copy region as column (rectangle region) to `kill-ring'

See also: `kill-rectangle', `copy-to-register'.
URL `http://ergoemacs.org/emacs/emacs_copy_rectangle_text_to_clipboard.html'
version 2015-11-16"
  ;; extract-rectangle suggested by YoungFrog, 2012-07-25
  (interactive "r")
  (require 'rect)
  (kill-new
   (with-temp-buffer
     (mapc (lambda (ξx) (insert ξx "\n"))
           (extract-rectangle φbegin φend))
     (delete-char -1)
     (buffer-string))))

;; launch bash terminal
(defun my-term ()
  (interactive)
  (term "/bin/bash"))
