;; -*- coding: utf-8 -*-

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


;; (defun ddg ()
;;   "Look up the word under cursor or selected region in ddg. This command switches you to firefox."
;;  (interactive)
;;  ;; myWord & myUrl variables
;;  (let (myWord myUrl)
;;    ;; myWord is selected region or word the point is under
;;    (setq myWord
;;          (if (use-region-p)
;;              (buffer-substring-no-properties (region-beginning) (region-end))
;;            (thing-at-point 'symbol)))
;;    ;; replace space by plus in the word
;;   (setq myWord (replace-regexp-in-string " " "+" myWord))
;;   (setq myUrl (concat "https://duckduckgo.com/?t=lm&q=" myWord))
;;   (browse-url-firefox myUrl)
;;    ))

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
