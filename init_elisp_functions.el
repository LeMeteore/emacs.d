;; -*- coding: utf-8 -*-

;;
(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(defun my_date_again ()
  "insert date"
  (interactive)
  (when (use-region-p)
    (delete-region (region-beginning) (region-end) )
    )
  (insert
   (concat
    (format-time-string "%c")
    ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
     (format-time-string "%z")))))

(defun my_date ()
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
  (interactive "fAdd key: \n")
  (let ((process-connection-type t)
        process)
    (unwind-protect
        (progn
          (setq process (start-process "ssh-add" nil
                                       "ssh-add" (expand-file-name key-file)))
          (set-process-filter process 'ssh-add-process-filter)
          (while (accept-process-output process)))
      (if (eq (process-status process) 'run)
          (kill-process process)))))


(defun ddg ()
  "Look up the word under cursor or selected region in ddg.
   This command switches you to firefox."
 (interactive)
 (let (myWord myUrl)
   (setq myWord
         (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (thing-at-point 'symbol)))

  (setq myWord (replace-regexp-in-string " " "+" myWord))
  (setq myUrl (concat "https://duckduckgo.com/?t=lm&q=" myWord))
  (browse-url-firefox myUrl)
   ))



(defun display-startup-echo-area-message ()
  (message "Welcome mister Nsukami_, I hope you're doing fine.!"))
