;;; starling-test -- tests for starling -*- lexical-binding: t -*-


;;; Commentary:
;; Very minimal tests for starling.el

(require 'starling)

   

(defun test-starling-txn-1 ()
  (json-read-from-string " {\"feedItemUid\":\"11221122-1122-1122-1122-112211221122\",\"categoryUid\":\"ccddccdd-ccdd-ccdd-ccdd-ccddccddccdd\",\"amount\":{\"currency\":\"GBP\",\"minorUnits\":123456},\"sourceAmount\":{\"currency\":\"GBP\",\"minorUnits\":123456},\"direction\":\"IN\",\"updatedAt\":\"2024-03-17T11:53:50.899Z\",\"transactionTime\":\"2024-03-17T11:53:50.899Z\",\"settlementTime\":\"2024-03-17T11:53:50.899Z\",\"retryAllocationUntilTime\":\"2024-03-17T11:53:50.899Z\",\"source\":\"MASTER_CARD\",\"sourceSubType\":\"CONTACTLESS\",\"status\":\"PENDING\",\"transactingApplicationUserUid\":\"aaaaaaaa-aaaa-4aaa-aaaa-aaaaaaaaaaaa\",\"counterPartyType\":\"MERCHANT\",\"counterPartyUid\":\"68e16af4-c2c3-413b-bf93-1056b90097fa\",\"counterPartyName\":\"Tesco\",\"counterPartySubEntityUid\":\"35d46207-d90e-483c-a40a-128cc4da4bee\",\"counterPartySubEntityName\":\"Tesco Southampton\",\"counterPartySubEntityIdentifier\":\"608371\",\"counterPartySubEntitySubIdentifier\":\"01234567\",\"exchangeRate\":0,\"totalFees\":0,\"totalFeeAmount\":{\"currency\":\"GBP\",\"minorUnits\":123456},\"reference\":\"TESCO-STORES-6148      SOUTHAMPTON   GBR\",\"country\":\"GB\",\"spendingCategory\":\"GROCERIES\",\"userNote\":\"Tax deductable, submit me to payroll\",\"roundUp\":{\"goalCategoryUid\":\"68e16af4-c2c3-413b-bf93-1056b90097fa\",\"amount\":{\"currency\":\"GBP\",\"minorUnits\":123456}},\"hasAttachment\":true,\"hasReceipt\":true,\"batchPaymentDetails\":{\"batchPaymentUid\":\"5fedd7fa-c670-4ca8-9fcb-a77a6f2219c5\",\"batchPaymentType\":\"BULK_PAYMENT\"}}"))
(defun test-starling-txn-2 ()
  (json-read-from-string "{\"feedItemUid\":\"11221122-1122-1122-1122-112211221122\",\"categoryUid\":\"ccddccdd-ccdd-ccdd-ccdd-ccddccddccdd\",\"amount\":{\"currency\":\"GBP\",\"minorUnits\":123456},\"sourceAmount\":{\"currency\":\"GBP\",\"minorUnits\":123456},\"direction\":\"OUT\",\"updatedAt\":\"2024-03-17T11:53:50.899Z\",\"transactionTime\":\"2024-03-17T11:53:50.899Z\",\"settlementTime\":\"2024-03-17T11:53:50.899Z\",\"retryAllocationUntilTime\":\"2024-03-17T11:53:50.899Z\",\"source\":\"MASTER_CARD\",\"sourceSubType\":\"CONTACTLESS\",\"status\":\"PENDING\",\"transactingApplicationUserUid\":\"aaaaaaaa-aaaa-4aaa-aaaa-aaaaaaaaaaaa\",\"counterPartyType\":\"MERCHANT\",\"counterPartyUid\":\"68e16af4-c2c3-413b-bf93-1056b90097fa\",\"counterPartyName\":\"Tesco\",\"counterPartySubEntityUid\":\"35d46207-d90e-483c-a40a-128cc4da4bee\",\"counterPartySubEntityName\":\"Tesco Southampton\",\"counterPartySubEntityIdentifier\":\"608371\",\"counterPartySubEntitySubIdentifier\":\"01234567\",\"exchangeRate\":0,\"totalFees\":0,\"totalFeeAmount\":{\"currency\":\"GBP\",\"minorUnits\":123456},\"reference\":\"TESCO-STORES-6148      SOUTHAMPTON   GBR\",\"country\":\"GB\",\"spendingCategory\":\"GROCERIES\",\"userNote\":\"Tax deductable, submit me to payroll\",\"roundUp\":{\"goalCategoryUid\":\"68e16af4-c2c3-413b-bf93-1056b90097fa\",\"amount\":{\"currency\":\"GBP\",\"minorUnits\":123456}},\"hasAttachment\":true,\"hasReceipt\":true,\"batchPaymentDetails\":{\"batchPaymentUid\":\"5fedd7fa-c670-4ca8-9fcb-a77a6f2219c5\",\"batchPaymentType\":\"BULK_PAYMENT\"}}"))


(ert-deftest test-starling-txn-amount-positive ()
  (should (equal "1234.56" (starling--txn-amount (test-starling-txn-1) ))))
(ert-deftest test-starling-txn-amount-negative ()
  (should (equal "-1234.56" (starling--txn-amount (test-starling-txn-2) ))))

(provide 'starling_test)
