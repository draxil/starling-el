;;; starling.el --- Starling bank interaction  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Joe Higton

;; Author: Joe Higton <draxil@gmail.com>
;; Contributors: Alex Drysdale <reissuecardboard@duck.com>
;; Version: 0.1.4
;; Homepage: https://codeberg.org/draxil/starling-el
;; Package-Requires: ((emacs "29.1") (plz "0.7.2"))
;; Keywords: data, applications, banking

;;; Commentary:

;; Get info from your starling bank account in Emacs!

;; See the Readme.org, or go to https://codeberg.org/draxil/starling-el
;; for instructions on getting started.

;;; Licence:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/

;;; Code:

(require 'plz)
(require 'auth-source)

(defgroup starling ()
  "Options for starling."
  :group 'applications)

(defcustom starling-show-accounts-as-spaces t
  "Show account balances along with spaces."
  :type 'boolean
  :group 'starling)

(defcustom starling-log-requests nil
  "When set log all requests with messages."
  :type 'boolean
  :group 'starling)

(defvar-local starling--current-account nil
  "The current account in use.")

(defvar-local starling--current-category nil
  "The current category we're looking at in this buffer.")

(defvar-local starling--current-category-accounts nil
  "Stores account associations for categories.")

;; FUTURE options for dealing with multiple accounts.

(defun starling--url-base ()
  "The baseurl for the api."
  "https://api.starlingbank.com/")

(defun starling--url (path)
  "Build a full url for PATH."
  (concat (starling--url-base) path))

(defun starling--key ()
  "Get the starling API Key."
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


(defun starling--headers (body)
  "Construct the standard starling headers for BODY."
  `(("Authorization" . ,(concat "Bearer " (starling--key)))
    ,(when body
       '("Content-type" . "application/json"))))

(defun starling--get-accounts ()
  "Get the list of accounts."
  (starling--do 'get "api/v2/accounts"))


(defun starling--do (verb path &optional body)
  "Call the Starling API, and decode the response from JSON to an alist.

VERB is a HTTP verb, e.g \='get.
PATH is the path (with no leading slash) of the call you want to
make, e.g api/v2/accounts
BODY optional, body to send in the request."

  (when starling-log-requests
    (message "%s %s %s" verb path body))

  ;; FUTURE: better error handling!
  (let* ((body-encoded
          (when body
            (json-encode body)))
         (as
          (if (equal verb 'get)
              #'json-read
            #'string))
         (req
          (plz
           verb
           (starling--url path)
           :headers (starling--headers body)
           :as as
           :body body-encoded)))
    req))

(defun starling--main-account ()
  "Get the main starling account."
  ;; FUTURE: not fault tolerant, assumes first account is the one!
  ;; ..which is dumb. Could at least respect account type primary?
  ;; FUTURE: cache?
  (let ((accounts (alist-get 'accounts (starling--get-accounts))))
    (cond
     ((arrayp accounts)
      (aref accounts 0)))))

(defun starling--accounts ()
  "Get the main starling account."
  (alist-get 'accounts (starling--get-accounts)))

(defun starling--get-current-account ()
  "Get the currently selected account uid."
  (cond
   ((not starling--current-account)
    (setq starling--current-account (starling--main-account-uuid)))
   (starling--current-account)))

(defun starling--main-account-uuid ()
  "Get the UUID for the main account."
  (alist-get 'accountUid (starling--main-account)))

(defun starling--main-account-default-category ()
  "Get the main account's default category."
  (alist-get 'defaultCategory (starling--main-account)))


(defun starling--get-spaces ()
  "Fetch current state of spaces."
  (starling--do
   'get
   (concat
    "api/v2/account/" (starling--main-account-uuid) "/spaces")))


(defun starling-space-table ()
  "Build the starling space table."
  ;; FUTURE process all accounts?
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
           ;; we need to store the account and the category for accounts:
           `((category . ,(alist-get 'uuid balance))
             (account . ,(alist-get 'account-uid balance)))
           (vector
            (alist-get 'name balance)
            (starling--display-cash
             (car (alist-get 'balance balance))))))
        (starling--account-display-balances))))))

(defun starling--display-cash (cash)
  "Display formatting for CASH."
  ;; FUTURE care for currency?
  (starling--to-major (alist-get 'minorUnits cash)))

(defun starling--to-major (units)
  "Convert minor UNITS (pence cents) to major (pounds dollars).
Also make it a string, for display purposes."
  (format "%.2f" (/ units 100.00)))

(defun starling--account-display-balances ()
  "Get balances for accounts."
  (mapcar
   (lambda (account)
     `((name . ,(alist-get 'name account))
       (balance
        .
        ,(starling--get-effective-account-balance
          (alist-get 'accountUid account)))
       (account-uid . ,(alist-get 'accountUid account))
       ;; FUTURE: should be category-uid, or similar.
       (uuid . ,(alist-get 'defaultCategory account))))
   (starling--accounts)))

(defun starling--get-effective-account-balance (account-uid)
  "Get ACCOUNT-UID balance."
  (list
   (alist-get
    'effectiveBalance
    (starling--do
     'get (concat "api/v2/accounts/" account-uid "/balance")))))


(defvar-keymap starling-spaces-mode-map
  :parent
  tabulated-list-mode-map
  "RET"
  #'starling--maybe-show-transactions)

(define-derived-mode
 starling-spaces-mode
 tabulated-list-mode
 "starling-spaces-mode"
 "Mode for viewing starling spaces."
 (setq tabulated-list-format
       [("Name" 60 t) ("Amount" 10 t :right-align 't)])
 (setq tabulated-list-sort-key '("Name" . nil))
 (tabulated-list-init-header))

;;;###autoload
(defun starling-spaces ()
  "Show the current balances of your Starling Spaces."
  (interactive)
  (pop-to-buffer "*Starling Spaces*" nil)
  (starling-spaces-mode)
  (setq tabulated-list-entries (starling-space-table))
  (tabulated-list-print 1))

(defun starling--txns-since ()
  "The time we get transactions since."
  (format-time-string "%FT00:00:00Z"
                      (- (time-convert (current-time) 'integer)
                         2592000)))

(defun starling--maybe-show-transactions ()
  "Possibly show transactions, if we're on a line with an id."
  (interactive)
  (let ((current (tabulated-list-get-id)))
    (when current
      (if (listp current)
          (starling--do-category-transactions
           (alist-get 'account current) (alist-get 'category current))
        (starling--do-category-transactions
         (starling--main-account-uuid) current)))))

(defun starling--do-category-transactions
    (account-uuid category-uuid &optional txn-uuid)
  "Get and show transactions for ACCOUNT-UUID and CATEGORY-UUID.
Optionally pick TXN-UUID."
  (setq starling--current-account account-uuid)
  (starling--show-transactions (starling--do
                                'get
                                ;; FUTURE: sensible date:
                                (concat
                                 "api/v2/feed/account/"
                                 account-uuid
                                 "/category/"
                                 category-uuid
                                 "?changesSince="
                                 (starling--txns-since)))
                               category-uuid
                               txn-uuid))

(defun starling--maybe-show-transaction ()
  "Possibly show transaction details, if we're on a line with an id."
  (interactive)
  (when (tabulated-list-get-id)
    (starling--show-transaction
     (starling--do
      'get

      (concat
       "api/v2/feed/account/"
       (starling--get-current-account)
       "/category/"
       starling--current-category
       "/"
       (tabulated-list-get-id))))))

(defvar-keymap starling-transactions-mode-map
  :parent
  (make-composed-keymap tabulated-list-mode-map)
  "RET"
  ;; FUTURE: these should be "public"?
  #'starling--maybe-show-transaction
  "c"
  #'starling--maybe-set-category
  "g"
  #'starling-refresh-current-transaction-view)

(define-derived-mode
 starling-transactions-mode
 tabulated-list-mode
 "starling-transactions-mode"
 "Mode for viewing Starling transactions."
 ;; FUTURE customisable columns?
 (setq tabulated-list-format
       [("Who" 20 t)
        ("Ref" 30 t)
        ("Category" 20 t)
        ("Amount" 10 t :right-align 't)
        ("Time" 20 t)])
 (tabulated-list-init-header))

(defun starling--show-transactions (txns category &optional txn-uuid)
  "Show the current balances of your Starling Spaces for TXNS in CATEGORY.
Optionally pick TXN-UUID."
  ;; FUTURE space name?
  (let ((account (starling--get-current-account)))
    (pop-to-buffer "*Starling Transactions*" nil)
    (starling-transactions-mode)
    (setq starling--current-account account)
    (setq starling--current-category category)
    (setq tabulated-list-entries (starling-transactions--table txns))
    (tabulated-list-print 1)
    (when txn-uuid
      (starling--find-txn txn-uuid))))

(defun starling--find-txn (txn-uuid)
  "Find the TXN-UUID if in buffer."
  (when (not (starling--txn-is txn-uuid))
    (goto-char (point-min))
    (while (and (not (starling--txn-is txn-uuid))
                (> (forward-line) 1)))))

(defun starling--txn-is (txn-uuid)
  "Is the current txn TXN-UUID?"
  (equal (tabulated-list-get-id) txn-uuid))


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
       (starling--txn-time txn))))
   (alist-get 'feedItems txns)))

(defun starling--describe-txn (txn)
  "Describe a starling transaction TXN."
  (concat (alist-get 'reference txn)))

(defun starling--txn-amount (txn)
  "Present the amount of a transaction TXN."
  (concat
   (when (equal (alist-get 'direction txn) "OUT")
     "-")
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
        (query-year
         (if year
             year
           (format-time-string "%Y")))
        (query-month
         (if month
             month
           (format-time-string "%B"))))
    (starling--do
     'get
     (concat
      "api/v2/accounts/"
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
  (if (string-equal (alist-get 'netDirection insight) "OUT")
      "-"
    "+"))

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
       (starling--format-category
        (alist-get 'spendingCategory insight))
       (starling-insight--total-spent insight)
       (starling-insight--total-recieved insight)
       (starling-insight--net insight)
       (starling-insight--net-direction insight)
       (starling-insight--percentage insight)
       (starling-insight--transaction-count insight))))
   (alist-get 'breakdown insights)))

(defun starling--show-insights (insights)
  "Show the insights for INSIGHTS."
  (pop-to-buffer "*Starling Insights*" nil)
  (starling-insights-mode)
  (setq tabulated-list-entries (starling-insights--table insights))
  (tabulated-list-print 1))

;;;###autoload
(defun starling-insights ()
  "Show the starling insights for the current month."
  (interactive)
  (let ((insights (starling--get-spending-insights)))
    (starling--show-insights insights)))

(define-derived-mode
 starling-transaction-mode
 special-mode
 "starling-transaction-mode"
 "Mode for viewing a starling transaction.")

(defun starling--show-transaction (txn)
  "Show transaction TXN."
  (pop-to-buffer "*Starling Transaction*" nil)
  (starling-transaction-mode)
  (cl-flet
   ;; little helper to get transaction fields:
   ((get-field (what) (alist-get what txn)))
   (let ((inhibit-read-only 't)
         (direction (get-field 'direction)))
     (erase-buffer)
     (if (equal direction "IN")
         (starling--transaction-field
          "From" (get-field 'counterPartyName)))
     (if (equal direction "OUT")
         (starling--transaction-field
          "To" (get-field 'counterPartyName)))
     (starling--transaction-field "Amount" (starling--txn-amount txn))
     (starling--transaction-field "Status" (get-field 'status))
     (starling--transaction-field "Source" (get-field 'source))
     (starling--transaction-field "Ref" (get-field 'reference))
     (starling--transaction-field "Time" (starling--txn-time txn))
     (starling--transaction-field
      "Category"
      (starling--format-category (get-field 'spendingCategory)))
     ;; FUTURE: more fields!
     )))

(defun starling--transaction-field (label value)
  "Show a transaction field for LABEL and VALUE, skipping nil VALUE ones."
  (when (not (equal value 'nil))
    (insert (propertize (format "%s: " label) 'face 'bold))
    ;; FUTURE: must be a better way? But some alignment.
    (insert (make-string (- 10 (length label)) 32))
    (insert value)
    (insert "\n")))

;;;###autoload
(defun starling-transactions ()
  "Show transactions, right now show the main account."
  (interactive)
  (let ((account-uuid (starling--main-account-uuid))
        (category-uuid (starling--main-account-default-category)))
    (if (and account-uuid category-uuid)
        (starling--do-category-transactions
         account-uuid category-uuid)
      (error "No account details found"))))

(defun starling--maybe-set-category ()
  "Possibly set spending category, if we're on a line with an id."
  (interactive)
  (when (tabulated-list-get-id)
    (starling--set-spending-category (tabulated-list-get-id))))

(defun starling--set-spending-category (txn-uuid)
  "Prompt for a new spending category for TXN-UUID."
  (interactive)
  (starling--maybe-action-spending-category
   txn-uuid
   (completing-read
    "New spending category: " (starling--spending-categories)
    nil 't)))

;; Would be nice if starling could give us these :(
(defun starling--spending-categories ()
  "List of possible spending categories."
  '(BIKE
    BILLS_AND_SERVICES
    BUCKET_LIST
    CAR
    CASH
    CELEBRATION
    CHARITY
    CHILDREN
    CLOTHES
    COFFEE
    DEBT_REPAYMENT
    DIY
    DRINKS
    EATING_OUT
    EDUCATION
    EMERGENCY
    ENTERTAINMENT
    ESSENTIAL_SPEND
    EXPENSES
    FAMILY
    FITNESS
    FUEL
    GAMBLING
    GAMING
    GARDEN
    GENERAL
    GIFTS
    GROCERIES
    HOBBY
    HOLIDAYS
    HOME
    IMPULSE_BUY
    INCOME
    INSURANCE
    INVESTMENTS
    LIFESTYLE
    MAINTENANCE_AND_REPAIRS
    MEDICAL
    MORTGAGE
    NON_ESSENTIAL_SPEND
    PAYMENTS
    PERSONAL_CARE
    PERSONAL_TRANSFERS
    PETS
    PROJECTS
    RELATIONSHIPS
    RENT
    SAVING
    SHOPPING
    SUBSCRIPTIONS
    TAKEAWAY
    TAXI
    TRANSPORT
    TREATS
    WEDDING
    WELLBEING
    NONE
    REVENUE
    OTHER_INCOME
    CLIENT_REFUNDS
    INVENTORY
    STAFF
    TRAVEL
    WORKPLACE
    REPAIRS_AND_MAINTENANCE
    ADMIN
    MARKETING
    BUSINESS_ENTERTAINMENT
    INTEREST_PAYMENTS
    BANK_CHARGES
    OTHER
    FOOD_AND_DRINK
    EQUIPMENT
    PROFESSIONAL_SERVICES
    PHONE_AND_INTERNET
    VEHICLES
    DIRECTORS_WAGES
    VAT
    CORPORATION_TAX
    SELF_ASSESSMENT_TAX
    INVESTMENT_CAPITAL
    TRANSFERS
    LOAN_PRINCIPAL
    PERSONAL
    DIVIDENDS))

(defun starling--maybe-action-spending-category
    (txn-uuid new-category)
  "Set the spending category for TXN-UUID to NEW-CATEGORY."

  (when (and txn-uuid new-category starling--current-category)
    (starling--do
     'put
     (concat
      "api/v2/feed/account/"
      (starling--main-account-uuid)
      "/category/"
      starling--current-category
      "/"
      txn-uuid
      "/spending-category/")
     `((spendingCategory . ,new-category))
     ;; FUTURE: possibility to do it permnanently, and for old tnxs
     )
    (starling--refresh-transactions)))

(defun starling--refresh-transactions ()
  "Refresh a transactions view."
  (starling--do-category-transactions
   (starling--get-current-account) starling--current-category
   (tabulated-list-get-id)))

(defun starling-refresh-current-transaction-view ()
  "Refresh the current starling transaction view assuming one is selected."
  (interactive)
  (when starling--current-category
    (starling--refresh-transactions)))
(provide 'starling)
;;; starling.el ends here
