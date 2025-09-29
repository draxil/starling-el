;;; starling.el -- Staling bank info in emacs.  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Joe Higton

;; Author: Joe Higton <draxil@gmail.com>
;; Contributors: Alex Drysdale <reissuecardboard@duck.com>
;; Version: 0.1.1
;; Homepage: https://github.com/draxil/starling-el
;; Keywords: banking, finance
;; Package-Requires: ((emacs "28") (plz "0.7.2"))"

;;; Commentary:
;;
;; Get info from your starling bank account in Emacs!
;; See the Readme.org for more.
;;
;;; Licence:
;;
;; Please see the LICENCE file.

;;; Code:

(require 'plz)
(require 'auth-source)

(defgroup starling ()
  "starling bank module")

(defcustom starling-show-accounts-as-spaces 't
  "Show account balances along with spaces."
  :type 'boolean
  :group 'starling)

;; TODO options for dealing with multiple accounts.

(defun starling--url-base ()
  ""
  "https://api.starlingbank.com/")

(defun starling--url (path)
  "build a full url for path"
  (concat (starling--url-base) path))

(defun starling--key ()
  (let ((key
         (auth-source-pick-first-password
          :host "api.starlingbank.com"
          :user "personal-token")))
    (cond
     ((null key)
      (error
       "No starling key found, please check the docs for how to configure"))
     (t
      key))))


(defun starling--headers ()
  ""
  `(("Authorization" . ,(concat "Bearer " (starling--key)))))

(defun starling--get-accounts ()
  "get the list of accounts"
  (starling--do 'get "api/v2/accounts"))


(defun starling--do (verb path &optional body)
  "Call the Starling API, and decode the response from JSON to an alist.

VERB is a HTTP verb, e.g 'get.
PATH is the path (with no leading slash) of the call you want to
make, e.g api/v2/accounts
BODY optional, body to send in the request (TODO, not actually any use for this yet)."
  ;; TODO: things go wrong!
  (let ((req
         (plz
          verb
          (starling--url path)
          :headers (starling--headers)
          :as #'json-read)))
    req))

(defun starling--main-account ()
  ;; TODO: not fault tolerant, assumes first account is the one!
  ;; ..which is dumb.
  ;; TODO: cache?
  (let ((accounts (alist-get 'accounts (starling--get-accounts))))
    (cond
     ((arrayp accounts)
      (aref accounts 0)))))

(defun starling--main-account-uuid ()
  (alist-get 'accountUid (starling--main-account)))

(defun starling--main-account-default-category ()
  (alist-get 'defaultCategory (starling--main-account)))


(defun starling--get-spaces ()
  "fetch current state of spaces"
  (starling--do
   'get
   (concat
    "api/v2/account/" (starling--main-account-uuid) "/spaces")))


(defun starling-space-table ()
  ;; TODO process all accounts?
  (let ((spaces (starling--get-spaces)))
    (append
     (mapcar
      (lambda (space)
        (list
         (alist-get 'savingsGoalUid space)
         (vector
          (alist-get 'name space)
          (starling--display-cash (alist-get 'totalSaved space)))))
      (alist-get 'savingsGoals spaces))
     (mapcar
      (lambda (space)
        (list
         (alist-get 'spaceUid space)
         (vector
          (alist-get 'name space)
          (starling--display-cash (alist-get 'balance space)))))
      (alist-get 'spendingSpaces spaces))
     (when starling-show-accounts-as-spaces
       (mapcar
        (lambda (balance)
          (list
           (alist-get 'uuid balance)
           (vector
	    (alist-get 'name balance)
            (starling--display-cash (car (alist-get 'balance balance))))))
        (starling--account-display-balances))))))

(defun starling--display-cash (cash)
  "Display a starling cash value."
  ;; TODO care for currency?
  (starling--to-major (alist-get 'minorUnits cash)))

(defun starling--to-major (units)
  "Convert minor UNITS (pence cents) to major (pounds dollars).   
Also make it a string, for display purposes."
  (format "%.2f" (/ units 100.00)))
;;(/ units 100.00)

(defun starling--account-display-balances ()
  "Get account balances."
  ;; FUTURE: option to pick which balance to display
  ;; TODO: other accounts?
  ;; TODO: other name?
  (let*
      (
       (main-uuid (starling--main-account-uuid))
       (main-account (starling--do
           'get
           (concat
            "api/v2/accounts/"
            main-uuid
            "/balance")))
       )
    `(
      (
       ;; TODO: real name?
       (name . "Main account")
       (balance . (,(alist-get
		    'effectiveBalance
		    main-account
		    )))
       (uuid .
	     ,(starling--main-account-default-category)
      )))))

(defvar-keymap starling-spaces-mode-map
  :suppress 't
  :parent tabulated-list-mode-map
  "RET" #'starling--maybe-show-transactions)


(define-derived-mode
 starling-spaces-mode
 tabulated-list-mode
 "starling-spaces-mode"
 "Mode for viewing starling spaces."
 (setq tabulated-list-format
       [("Name" 60 t) ("Amount" 10 t :right-align 't)])
 (setq tabulated-list-sort-key '("Name" . nil))
 (tabulated-list-init-header))

(defun starling-spaces ()
  "Shows the current balances of your Starling Spaces. "
  (interactive)
  (pop-to-buffer "*Starling Spaces*" nil)
  (starling-spaces-mode)
  (setq tabulated-list-entries (starling-space-table))
  (tabulated-list-print 1)
  )


(defun starling--txns-since ()
  (format-time-string "%FT00:00:00Z" (- (time-convert (current-time) 'integer) 2592000)))

(defun starling--maybe-show-transactions ()
  "Possibly show transactions, if we're on a line with an id."
  (interactive)
  (when (tabulated-list-get-id)
    (starling--show-transactions
       (starling--do
	'get

	;; TODO: sensible date:
	(concat "api/v2/feed/account/" (starling--main-account-uuid) "/category/" (tabulated-list-get-id) "?changesSince=" (starling--txns-since)))
       )))

(define-derived-mode
  starling-transactions-mode
  tabulated-list-mode
  "starling-transactions-mode"
  "Mode for viewing Starling transactions."
  ;; TODO customisable columns?
  (setq tabulated-list-format
	[("Who" 20 t) ("Description" 60 t) ("Category" 20 t) ("Amount" 10 t :right-align 't) ("Time" 20 t)])
  (tabulated-list-init-header))


(defun starling--show-transactions (txns)
  "Show the current balances of your Starling Spaces for TXNS."
  ;; TODO space name?
  (pop-to-buffer "*Starling Trnsactions*" nil)
  (starling-transactions-mode)
  (setq tabulated-list-entries (starling-transactions--table txns))
  (tabulated-list-print 1))

(defun starling-transactions--table (txns)
  "Table for starling transactions TXNS."
  (mapcar
   (lambda (txn)
     (list
      (alist-get 'feedItemUid txn)
      (vector
       (alist-get 'counterPartyName txn)
       (starling--describe-txn txn)
       (starling--format-category (alist-get 'spendingCategory txn))
       (starling--txn-amount txn)
       (starling--txn-time txn)
       )))
   (alist-get 'feedItems txns)))

(defun starling--describe-txn (txn)
  "Describe a starling transaction TXN."
  (concat
   (alist-get 'reference txn)
   ))

(defun starling--txn-amount (txn)
  "Present the amount of a transaction TXN."
  (concat
   (when (equal (alist-get 'direction txn) "OUT") "-")
   (starling--display-cash (alist-get 'amount txn))))

(defun starling--txn-time (txn)
  "Present the time of a transaction TXN."
  (alist-get 'transactionTime txn))

(defun starling--format-category (category)
  "Format a starling spending CATEGORY."
  (upcase-initials (string-replace "_" " " (downcase category))))

;; Insights
(defun starling--get-spending-insights (&optional year month)
  "Display spending insights for a given YEAR and MONTH."
  (let ((account-uid (starling--main-account-uuid))
        (query-year (if year year (format-time-string "%Y")))
        (query-month (if month month (format-time-string "%B"))))
    (starling--do 'get (concat "api/v2/accounts/"
                               account-uid
                               "/spending-insights/spending-category?year="
                               query-year
                               "&month="
                               (upcase query-month)))))

(define-derived-mode
  starling-insights-mode
  tabulated-list-mode
  "starling-insights-mode"
  "Mode for viewing Starling insights."
  (setq tabulated-list-format
	[("Category" 20 t)
     ("Total Spent" 20 t :right-align 't)
     ("Total Received" 20 t :right-align 't)
     ("Net" 8 t :right-align 't)
     ("" 1 t)
     ("Percentage" 20 t :right-align 't)
     ("# Transactions" 10 t :right-align 't)])
  (tabulated-list-init-header))


(defun starling-insight--total-spent (insight)
  "Total spent for an INSIGHT."
  (number-to-string (alist-get 'totalSpent insight)))

(defun starling-insight--total-recieved (insight)
  "Total received for an INSIGHT."
  (number-to-string (alist-get 'totalReceived insight)))

(defun starling-insight--net-direction (insight)
  "Get the net direction of the INSIGHT."
  (if (string-equal (alist-get 'netDirection insight) "OUT") "-" "+"))

(defun starling-insight--net (insight)
  "Net spent for an INSIGHT."
  (number-to-string (alist-get 'netSpend insight)))

(defun starling-insight--percentage (insight)
  "Percentage of spending for an INSIGHT."
  (number-to-string (alist-get 'percentage insight)))

(defun starling-insight--transaction-count (insight)
  "Number of transactions for an INSIGHT."
  (number-to-string (alist-get 'transactionCount insight)))

(defun starling-insights--table (insights)
  "Table for starling INSIGHTS."
  (mapcar
   (lambda (insight)
     (list
      (alist-get 'spendingCategory insight)
      (vector
       (starling--format-category (alist-get 'spendingCategory insight))
       (starling-insight--total-spent insight)
       (starling-insight--total-recieved insight)
       (starling-insight--net insight)
       (starling-insight--net-direction insight)
       (starling-insight--percentage insight)
       (starling-insight--transaction-count insight)
       )))
   (alist-get 'breakdown insights)))

(defun starling--show-insights (insights)
  "Show the insights for INSIGHTS."
  (pop-to-buffer "*Starling Insights*" nil)
  (starling-insights-mode)
  (setq tabulated-list-entries (starling-insights--table insights))
  (tabulated-list-print 1))

(defun starling-insights ()
  "Shows the starling insights for the current month."
  (interactive)
  (let ((insights (starling--get-spending-insights)))
    (starling--show-insights insights)))

(provide 'starling)
