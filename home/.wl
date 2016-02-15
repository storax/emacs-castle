;; You should set this variable if you use multiple e-mail addresses.
(setq wl-user-mail-address-list (quote ("zuber.david@gmx.de" "zuber.david@web.de")))

;;(NOTE: "M-: wl-draft-parent-folder" => %INBOX:myname/clear@imap.gmail.com:993)
(setq wl-draft-config-alist
      '(((string-match "web.de" wl-draft-parent-folder)
	 (template . "web"))
	((string-match "gmx.de" wl-draft-parent-folder)
	 (template . "gmx"))
	;; automatic for replies
	(reply "\\(To\\|Cc\\|Delivered-To\\): .*web.de*"
	 (template . "web"))
	(reply "\\(To\\|Cc\\|Delivered-To\\): .*gmx.de*"
	 (template . "gmx"))))

;;choose template with C-c C-j
(setq wl-template-alist
      '(("gmx"
	 (wl-from . "David Zuber <zuber.david@gmx.de>")
	 (wl-smtp-posting-user . "zuber.david@gmx.de")
	 (wl-smtp-posting-server . "mail.gmx.net")
	 (wl-smtp-authenticate-type ."plain")
	 (wl-smtp-connection-type . 'starttls)
	 (wl-smtp-posting-port . 587)
	 (wl-local-domain . "gmx.net")
	 (wl-message-id-domain . "mail.gmx.net"))
	("web"
	 (wl-from . "David Zuber <zuber.david@web.de>")
	 (wl-smtp-posting-user . "zuber.david")
	 (wl-smtp-posting-server . "smtp.web.de")
	 (wl-smtp-authenticate-type ."plain")
	 (wl-smtp-connection-type . 'starttls)
	 (wl-smtp-posting-port . 587)
	 (wl-local-domain . "web.de")
	 (wl-message-id-domain . "smtp.web.de"))))

(setq
  ;elmo-maildir-folder-path "~/Maildir"          ;; where i store my mail
  mime-edit-split-message nil                   ;; Dont split attachements in many messages
  mime-view-buttons-visible nil
  wl-stay-folder-window t                       ;; show the folder pane (left)
  wl-folder-window-width 25                     ;; toggle on/off with 'i'
  wl-summary-showto-folder-regexp ".*Sent.*"    ;; Show to instead of from
  ;; note: all below are dirs (Maildirs) under elmo-maildir-folder-path
  ;; the '.'-prefix is for marking them as maildirs
  wl-fcc-force-as-read t               ;; mark sent messages as read

  ;; check this folder periodically, and update modeline
  ;wl-biff-check-folder-list '(".todo") ;; check every 180 seconds
				       ;; (default: wl-biff-check-interval)

  ;; hide many fields from message buffers
  wl-message-ignored-field-list '("^.*:")
  wl-message-visible-field-list
  '("^\\(To\\|Cc\\):"
    "^Subject:"
    "^\\(From\\|Reply-To\\):"
    "^Organization:"
    "^\\(Posted\\|Date\\):"
    )
  wl-message-sort-field-list
  '("^From"
    "^Organization:"
    "^X-Attribution:"
     "^Subject"
     "^Date"
     "^To"
     "^Cc")
  wl-summary-line-format
  "%n%T%P %D.%M.%Y %h:%m %t%[%17(%c %f%) %] %s"
  mime-view-type-subtype-score-alist
  '(((text . plain) . 4)
    ((text . enriched) . 3)
    ((text . html) . 2)
    ((text . richtext) . 1)))
