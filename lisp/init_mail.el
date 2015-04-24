;; -*- coding: utf-8 -*-

;; send mail
(setq
 send-mail-function 'smtpmail-send-it
 message-send-mail-function 'smtpmail-send-it
 user-mail-address "patrick.nsukami@gmail.com"
 smtpmail-starttls-credentials '(("smtp.gmail.com" "587" nil nil))
 smtpmail-auth-credentials (expand-file-name "~/.authinfo")
 smtpmail-default-smtp-server "smtp.gmail.com"
 smtpmail-smtp-server "smtp.gmail.com"
 smtpmail-smtp-service 587
 smtpmail-debug-info t
 starttls-extra-arguments nil
 starttls-gnutls-program "/usr/bin/gnutls-cli"
 starttls-extra-arguments nil
 starttls-use-gnutls t
 )

(require 'mu4e)

;; default
;; (setq mu4e-maildir "~/Maildir")

(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; (See the documentation for `mu4e-sent-messages-behavior' if you have
;; additional non-Gmail addresses and want assign them different
;; behavior.)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
      '( ("/INBOX"               . ?i)
         ("/[Gmail].Sent Mail"   . ?s)
         ("/[Gmail].Trash"       . ?t)
         ("/[Gmail].All Mail"    . ?a)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; something about ourselves
(setq
 user-mail-address "patrick.nsukami@gmail.com"
 user-full-name  "Foo X. Bar"
 mu4e-compose-signature
 (concat
  "Foo X. Bar\n"
  "http://www.example.com\n"))



;; don't keep message buffers around
;; (setq message-kill-buffer-on-exit t)
