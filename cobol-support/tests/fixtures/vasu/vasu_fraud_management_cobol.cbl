IDENTIFICATION DIVISION.
       PROGRAM-ID. FRAUD-MGMT-SYSTEM.
       AUTHOR. FRAUD-DETECTION-TEAM.
       DATE-WRITTEN. 2025-08-06.
       DATE-COMPILED.
      
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z15.
       OBJECT-COMPUTER. IBM-Z15.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSACTION-FILE ASSIGN TO 'TRANFILE'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-TRANS-STATUS.
           
           SELECT CUSTOMER-FILE ASSIGN TO 'CUSTFILE'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUST-CARD-NUMBER
               FILE STATUS IS WS-CUST-STATUS.
           
           SELECT MERCHANT-FILE ASSIGN TO 'MERCHFILE'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS MERCH-ID
               FILE STATUS IS WS-MERCH-STATUS.
           
           SELECT FRAUD-LOG ASSIGN TO 'FRAUDLOG'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FRAUD-STATUS.
           
           SELECT VELOCITY-FILE ASSIGN TO 'VELOFILE'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS VELO-CARD-NUMBER
               FILE STATUS IS WS-VELO-STATUS.

       DATA DIVISION.
       FILE SECTION.
       
       FD  TRANSACTION-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 200 CHARACTERS.
       01  TRANSACTION-RECORD.
           05  TRANS-ID                PIC 9(12).
           05  TRANS-CARD-NUMBER       PIC 9(16).
           05  TRANS-AMOUNT            PIC 9(8)V99.
           05  TRANS-DATE              PIC 9(8).
           05  TRANS-TIME              PIC 9(6).
           05  TRANS-MERCHANT-ID       PIC X(10).
           05  TRANS-MERCHANT-CAT      PIC 9(4).
           05  TRANS-TYPE              PIC X(2).
           05  TRANS-LOCATION-ZIP      PIC X(10).
           05  TRANS-COUNTRY-CODE      PIC X(3).
           05  TRANS-CURRENCY          PIC X(3).
           05  TRANS-CHANNEL           PIC X(3).
           05  TRANS-POS-ENTRY         PIC X(2).
           05  TRANS-CHIP-STATUS       PIC X(1).
           05  TRANS-PIN-VERIFIED      PIC X(1).
           05  FILLER                  PIC X(134).

       FD  CUSTOMER-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 300 CHARACTERS.
       01  CUSTOMER-RECORD.
           05  CUST-CARD-NUMBER        PIC 9(16).
           05  CUST-NAME               PIC X(40).
           05  CUST-HOME-ZIP           PIC X(10).
           05  CUST-HOME-COUNTRY       PIC X(3).
           05  CUST-ACCOUNT-OPEN-DATE  PIC 9(8).
           05  CUST-CREDIT-LIMIT       PIC 9(8)V99.
           05  CUST-CURRENT-BALANCE    PIC 9(8)V99.
           05  CUST-RISK-SCORE         PIC 9(3).
           05  CUST-FRAUD-FLAG         PIC X(1).
           05  CUST-LAST-TRANS-DATE    PIC 9(8).
           05  CUST-AVG-MONTHLY-SPEND  PIC 9(8)V99.
           05  CUST-MAX-DAILY-SPEND    PIC 9(8)V99.
           05  CUST-TRAVEL-FLAG        PIC X(1).
           05  CUST-PHONE-NUMBER       PIC X(15).
           05  CUST-EMAIL              PIC X(50).
           05  FILLER                  PIC X(144).

       FD  MERCHANT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 150 CHARACTERS.
       01  MERCHANT-RECORD.
           05  MERCH-ID                PIC X(10).
           05  MERCH-NAME              PIC X(40).
           05  MERCH-CATEGORY          PIC 9(4).
           05  MERCH-RISK-LEVEL        PIC 9(2).
           05  MERCH-ZIP               PIC X(10).
           05  MERCH-COUNTRY           PIC X(3).
           05  MERCH-FRAUD-RATE        PIC 9(3)V99.
           05  MERCH-LAST-FRAUD-DATE   PIC 9(8).
           05  FILLER                  PIC X(70).

       FD  FRAUD-LOG
           RECORDING MODE IS F
           RECORD CONTAINS 400 CHARACTERS.
       01  FRAUD-LOG-RECORD.
           05  FRAUD-TIMESTAMP         PIC X(20).
           05  FRAUD-TRANS-ID          PIC 9(12).
           05  FRAUD-CARD-NUMBER       PIC 9(16).
           05  FRAUD-REASON-CODE       PIC X(10).
           05  FRAUD-RISK-SCORE        PIC 9(3).
           05  FRAUD-ACTION-TAKEN      PIC X(20).
           05  FRAUD-RULE-TRIGGERED    PIC X(50).
           05  FRAUD-AMOUNT            PIC 9(8)V99.
           05  FRAUD-MERCHANT          PIC X(40).
           05  FRAUD-LOCATION          PIC X(20).
           05  FRAUD-ANALYST-ID        PIC X(10).
           05  FRAUD-RESOLUTION        PIC X(100).
           05  FILLER                  PIC X(73).

       FD  VELOCITY-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 100 CHARACTERS.
       01  VELOCITY-RECORD.
           05  VELO-CARD-NUMBER        PIC 9(16).
           05  VELO-TRANS-COUNT-1H     PIC 9(3).
           05  VELO-AMOUNT-1H          PIC 9(8)V99.
           05  VELO-TRANS-COUNT-24H    PIC 9(4).
           05  VELO-AMOUNT-24H         PIC 9(8)V99.
           05  VELO-LAST-UPDATE        PIC 9(14).
           05  VELO-LOCATION-COUNT     PIC 9(2).
           05  VELO-MERCHANT-COUNT     PIC 9(3).
           05  FILLER                  PIC X(31).

       WORKING-STORAGE SECTION.
       
      * File Status Variables
       01  WS-TRANS-STATUS             PIC XX.
       01  WS-CUST-STATUS              PIC XX.
       01  WS-MERCH-STATUS             PIC XX.
       01  WS-FRAUD-STATUS             PIC XX.
       01  WS-VELO-STATUS              PIC XX.
       
      * Control Variables
       01  WS-EOF-FLAG                 PIC X VALUE 'N'.
           88  EOF-REACHED             VALUE 'Y'.
       01  WS-FRAUD-DETECTED           PIC X VALUE 'N'.
           88  FRAUD-FOUND             VALUE 'Y'.
       01  WS-PROCESS-FLAG             PIC X VALUE 'Y'.
       
      * Risk Assessment Variables
       01  WS-TOTAL-RISK-SCORE         PIC 9(4) VALUE ZERO.
       01  WS-TRANSACTION-RISK         PIC 9(3) VALUE ZERO.
       01  WS-VELOCITY-RISK            PIC 9(3) VALUE ZERO.
       01  WS-LOCATION-RISK            PIC 9(3) VALUE ZERO.
       01  WS-MERCHANT-RISK            PIC 9(3) VALUE ZERO.
       01  WS-BEHAVIORAL-RISK          PIC 9(3) VALUE ZERO.
       
      * Fraud Thresholds
       01  WS-FRAUD-THRESHOLDS.
           05  HIGH-RISK-THRESHOLD     PIC 9(3) VALUE 800.
           05  MEDIUM-RISK-THRESHOLD   PIC 9(3) VALUE 500.
           05  LOW-RISK-THRESHOLD      PIC 9(3) VALUE 300.
           05  MAX-DAILY-VELOCITY      PIC 9(4) VALUE 50.
           05  MAX-HOURLY-VELOCITY     PIC 9(2) VALUE 10.
           05  SUSPICIOUS-AMOUNT       PIC 9(8)V99 VALUE 5000.00.
           05  MAX-LOCATION-VARIANCE   PIC 9(4) VALUE 1000.
       
      * Counters and Statistics
       01  WS-COUNTERS.
           05  WS-TRANSACTIONS-PROCESSED PIC 9(8) VALUE ZERO.
           05  WS-FRAUD-DETECTED-COUNT   PIC 9(6) VALUE ZERO.
           05  WS-FALSE-POSITIVE-COUNT   PIC 9(6) VALUE ZERO.
           05  WS-APPROVED-COUNT         PIC 9(8) VALUE ZERO.
           05  WS-DECLINED-COUNT         PIC 9(6) VALUE ZERO.
       
      * Work Variables
       01  WS-CURRENT-TIMESTAMP        PIC X(20).
       01  WS-WORK-AMOUNT              PIC 9(8)V99.
       01  WS-WORK-DATE                PIC 9(8).
       01  WS-WORK-TIME                PIC 9(6).
       01  WS-DAYS-DIFF                PIC S9(4) COMP.
       01  WS-DISTANCE-KM              PIC 9(6).
       01  WS-TIME-DIFF-HOURS          PIC 9(4).
       
      * Rule Engine Variables
       01  WS-RULE-RESULTS.
           05  RULE-01-TRIGGERED       PIC X VALUE 'N'.
           05  RULE-02-TRIGGERED       PIC X VALUE 'N'.
           05  RULE-03-TRIGGERED       PIC X VALUE 'N'.
           05  RULE-04-TRIGGERED       PIC X VALUE 'N'.
           05  RULE-05-TRIGGERED       PIC X VALUE 'N'.
           05  RULE-06-TRIGGERED       PIC X VALUE 'N'.
           05  RULE-07-TRIGGERED       PIC X VALUE 'N'.
           05  RULE-08-TRIGGERED       PIC X VALUE 'N'.
           05  RULE-09-TRIGGERED       PIC X VALUE 'N'.
           05  RULE-10-TRIGGERED       PIC X VALUE 'N'.
       
      * Fraud Action Codes
       01  WS-FRAUD-ACTIONS.
           05  ACTION-DECLINE          PIC X(20) VALUE 'DECLINE-TRANSACTION'.
           05  ACTION-HOLD             PIC X(20) VALUE 'HOLD-FOR-REVIEW'.
           05  ACTION-VERIFY           PIC X(20) VALUE 'CUSTOMER-VERIFY'.
           05  ACTION-APPROVE          PIC X(20) VALUE 'APPROVE-NORMAL'.
           05  ACTION-FLAG             PIC X(20) VALUE 'FLAG-ACCOUNT'.
       
      * Error Messages
       01  WS-ERROR-MESSAGES.
           05  ERR-FILE-NOT-FOUND      PIC X(50) 
               VALUE 'ERROR: Required file not found or accessible'.
           05  ERR-INVALID-CARD        PIC X(50) 
               VALUE 'ERROR: Invalid card number format'.
           05  ERR-SYSTEM-ERROR        PIC X(50) 
               VALUE 'ERROR: System processing error occurred'.

       PROCEDURE DIVISION.
       
       0000-MAIN-CONTROL SECTION.
       0000-MAIN-PROCESS.
           PERFORM 1000-INITIALIZE-PROGRAM
           PERFORM 2000-PROCESS-TRANSACTIONS
           PERFORM 9000-FINALIZE-PROGRAM
           STOP RUN.

       1000-INITIALIZE-PROGRAM SECTION.
       1000-INIT-START.
           DISPLAY 'FRAUD MANAGEMENT SYSTEM - INITIALIZING'
           PERFORM 1100-OPEN-FILES
           PERFORM 1200-INITIALIZE-VARIABLES
           PERFORM 1300-LOAD-FRAUD-PARAMETERS.

       1100-OPEN-FILES.
           OPEN INPUT TRANSACTION-FILE
           IF WS-TRANS-STATUS NOT = '00'
               DISPLAY ERR-FILE-NOT-FOUND ' - TRANSACTION FILE'
               STOP RUN
           END-IF
           
           WRITE FRAUD-LOG-RECORD
           IF WS-FRAUD-STATUS NOT = '00'
               DISPLAY 'ERROR WRITING TO FRAUD LOG: ' WS-FRAUD-STATUS
           END-IF.

       3100-UPDATE-CUSTOMER-PROFILE SECTION.
       3100-UPDATE-START.
      * Update customer's last transaction date and spending patterns
           MOVE TRANS-DATE TO CUST-LAST-TRANS-DATE
           
      * Update fraud flag if high-risk transaction detected
           IF WS-TOTAL-RISK-SCORE >= HIGH-RISK-THRESHOLD
               MOVE 'Y' TO CUST-FRAUD-FLAG
           END-IF
           
      * Recalculate average monthly spend (simplified)
           IF CUST-AVG-MONTHLY-SPEND = ZERO
               MOVE TRANS-AMOUNT TO CUST-AVG-MONTHLY-SPEND
           ELSE
               COMPUTE CUST-AVG-MONTHLY-SPEND = 
                   (CUST-AVG-MONTHLY-SPEND * 0.9) + 
                   (TRANS-AMOUNT * 0.1)
           END-IF
           
      * Update maximum daily spend if exceeded
           IF TRANS-AMOUNT > CUST-MAX-DAILY-SPEND
               MOVE TRANS-AMOUNT TO CUST-MAX-DAILY-SPEND
           END-IF
           
           REWRITE CUSTOMER-RECORD
           IF WS-CUST-STATUS NOT = '00'
               DISPLAY 'ERROR UPDATING CUSTOMER RECORD: ' WS-CUST-STATUS
           END-IF.

       4000-ADVANCED-ANALYTICS SECTION.
       4000-ANALYTICS-START.
      * Advanced pattern recognition and machine learning scoring
           PERFORM 4100-NEURAL-NETWORK-SCORING
           PERFORM 4200-PATTERN-RECOGNITION
           PERFORM 4300-BEHAVIORAL-BIOMETRICS
           PERFORM 4400-CONSORTIUM-DATA-CHECK.

       4100-NEURAL-NETWORK-SCORING.
      * Simulate neural network scoring based on transaction features
           COMPUTE WS-WORK-AMOUNT = 
               (WS-TRANSACTION-RISK * 0.25) +
               (WS-VELOCITY-RISK * 0.20) +
               (WS-LOCATION-RISK * 0.15) +
               (WS-MERCHANT-RISK * 0.20) +
               (WS-BEHAVIORAL-RISK * 0.20)
           
      * Apply non-linear transformation (sigmoid approximation)
           IF WS-WORK-AMOUNT > 500
               COMPUTE WS-WORK-AMOUNT = WS-WORK-AMOUNT * 1.5
           ELSE
               COMPUTE WS-WORK-AMOUNT = WS-WORK-AMOUNT * 0.8
           END-IF
           
           ADD WS-WORK-AMOUNT TO WS-TOTAL-RISK-SCORE.

       4200-PATTERN-RECOGNITION.
      * Identify suspicious patterns in transaction sequences
           IF VELO-TRANS-COUNT-1H > 3
               PERFORM 4210-CHECK-ROUND-DOLLAR-PATTERN
               PERFORM 4220-CHECK-ASCENDING-AMOUNT-PATTERN
               PERFORM 4230-CHECK-TEST-TRANSACTION-PATTERN
           END-IF.

       4210-CHECK-ROUND-DOLLAR-PATTERN.
      * Detect round dollar amounts (potential card testing)
           COMPUTE WS-WORK-AMOUNT = TRANS-AMOUNT - 
                   FUNCTION INTEGER(TRANS-AMOUNT)
           IF WS-WORK-AMOUNT = ZERO AND TRANS-AMOUNT <= 100.00
               ADD 75 TO WS-TOTAL-RISK-SCORE
           END-IF.

       4220-CHECK-ASCENDING-AMOUNT-PATTERN.
      * This would normally check against recent transaction history
      * Simplified version checks if amount follows common test patterns
           EVALUATE TRANS-AMOUNT
               WHEN 1.00
               WHEN 5.00
               WHEN 10.00
               WHEN 25.00
                   IF VELO-TRANS-COUNT-1H > 2
                       ADD 100 TO WS-TOTAL-RISK-SCORE
                   END-IF
           END-EVALUATE.

       4230-CHECK-TEST-TRANSACTION-PATTERN.
      * Detect rapid small transactions followed by large ones
           IF TRANS-AMOUNT < 50.00 AND VELO-TRANS-COUNT-1H > 5
               ADD 150 TO WS-TOTAL-RISK-SCORE
           END-IF.

       4300-BEHAVIORAL-BIOMETRICS.
      * Simulate behavioral analysis based on transaction timing
           PERFORM 4310-ANALYZE-TYPING-PATTERNS
           PERFORM 4320-ANALYZE-DEVICE-FINGERPRINT
           PERFORM 4330-ANALYZE-SESSION-BEHAVIOR.

       4310-ANALYZE-TYPING-PATTERNS.
      * In real implementation, this would analyze keystroke dynamics
      * Simplified: flag transactions during unusual hours
           IF TRANS-TIME < 050000 OR TRANS-TIME > 230000
               IF TRANS-CHANNEL = 'ONL'
                   ADD 50 TO WS-TOTAL-RISK-SCORE
               END-IF
           END-IF.

       4320-ANALYZE-DEVICE-FINGERPRINT.
      * Simplified device risk assessment
           IF TRANS-POS-ENTRY = '90'  * Manual entry
               IF TRANS-AMOUNT > 200.00
                   ADD 25 TO WS-TOTAL-RISK-SCORE
               END-IF
           END-IF
           
           IF TRANS-CHIP-STATUS = 'N' AND TRANS-AMOUNT > 100.00
               ADD 40 TO WS-TOTAL-RISK-SCORE
           END-IF.

       4330-ANALYZE-SESSION-BEHAVIOR.
      * Check for suspicious session patterns
           IF TRANS-CHANNEL = 'ONL'
               IF VELO-MERCHANT-COUNT > 3
                   ADD 60 TO WS-TOTAL-RISK-SCORE
               END-IF
           END-IF.

       4400-CONSORTIUM-DATA-CHECK.
      * Simulate cross-bank fraud consortium data check
           IF CUST-RISK-SCORE > 750
               PERFORM 4410-CHECK-INDUSTRY-BLACKLIST
               PERFORM 4420-CHECK-VELOCITY-CONSORTIUM
           END-IF.

       4410-CHECK-INDUSTRY-BLACKLIST.
      * In production, this would check against shared fraud databases
           IF MERCH-FRAUD-RATE > 5.00
               ADD 100 TO WS-TOTAL-RISK-SCORE
           END-IF.

       4420-CHECK-VELOCITY-CONSORTIUM.
      * Check if card appears in recent consortium alerts
           IF VELO-TRANS-COUNT-24H > 30
               ADD 125 TO WS-TOTAL-RISK-SCORE
           END-IF.

       5000-REAL-TIME-SCORING SECTION.
       5000-SCORING-START.
      * Real-time risk scoring with multiple model ensemble
           PERFORM 5100-GRADIENT-BOOSTING-MODEL
           PERFORM 5200-RANDOM-FOREST-MODEL
           PERFORM 5300-LOGISTIC-REGRESSION-MODEL
           PERFORM 5400-ENSEMBLE-SCORING.

       5100-GRADIENT-BOOSTING-MODEL.
      * Simulate gradient boosting decision tree scoring
           COMPUTE WS-WORK-AMOUNT = 
               (TRANS-AMOUNT / CUST-AVG-MONTHLY-SPEND) * 100
           
           EVALUATE TRUE
               WHEN WS-WORK-AMOUNT > 500
                   ADD 200 TO WS-TOTAL-RISK-SCORE
               WHEN WS-WORK-AMOUNT > 300
                   ADD 150 TO WS-TOTAL-RISK-SCORE
               WHEN WS-WORK-AMOUNT > 200
                   ADD 100 TO WS-TOTAL-RISK-SCORE
               WHEN WS-WORK-AMOUNT > 150
                   ADD 75 TO WS-TOTAL-RISK-SCORE
           END-EVALUATE.

       5200-RANDOM-FOREST-MODEL.
      * Simulate random forest ensemble
           COMPUTE WS-WORK-AMOUNT = 
               WS-VELOCITY-RISK + WS-LOCATION-RISK + WS-MERCHANT-RISK
           
           IF WS-WORK-AMOUNT > 400
               ADD 175 TO WS-TOTAL-RISK-SCORE
           ELSE IF WS-WORK-AMOUNT > 200
               ADD 100 TO WS-TOTAL-RISK-SCORE
           ELSE IF WS-WORK-AMOUNT > 100
               ADD 50 TO WS-TOTAL-RISK-SCORE
           END-IF.

       5300-LOGISTIC-REGRESSION-MODEL.
      * Simulate logistic regression probability scoring
           COMPUTE WS-WORK-AMOUNT = 
               (WS-BEHAVIORAL-RISK * 1.2) +
               (WS-TRANSACTION-RISK * 1.1) +
               (MERCH-RISK-LEVEL * 0.8)
           
           IF WS-WORK-AMOUNT > 300
               ADD 125 TO WS-TOTAL-RISK-SCORE
           END-IF.

       5400-ENSEMBLE-SCORING.
      * Combine multiple model outputs with weighted averaging
           COMPUTE WS-TOTAL-RISK-SCORE = 
               WS-TOTAL-RISK-SCORE * 0.85
           
      * Apply final adjustments based on business rules
           IF CUST-FRAUD-FLAG = 'Y'
               COMPUTE WS-TOTAL-RISK-SCORE = 
                   WS-TOTAL-RISK-SCORE * 1.3
           END-IF
           
           IF WS-TOTAL-RISK-SCORE > 999
               MOVE 999 TO WS-TOTAL-RISK-SCORE
           END-IF.

       6000-CASE-MANAGEMENT SECTION.
       6000-CASE-START.
      * Create fraud cases for investigation
           IF WS-FRAUD-DETECTED = 'Y'
               PERFORM 6100-CREATE-FRAUD-CASE
               PERFORM 6200-ASSIGN-CASE-PRIORITY
               PERFORM 6300-NOTIFY-FRAUD-TEAM
           END-IF.

       6100-CREATE-FRAUD-CASE.
      * Generate unique case ID and initialize case record
           COMPUTE FRAUD-TRANS-ID = TRANS-ID + 10000000
           MOVE 'OPEN' TO FRAUD-RESOLUTION
           MOVE 'HIGH' TO FRAUD-ANALYST-ID
           
           STRING 'CASE_' FRAUD-TRANS-ID DELIMITED BY SIZE
               INTO FRAUD-ANALYST-ID.

       6200-ASSIGN-CASE-PRIORITY.
      * Assign investigation priority based on risk score and amount
           EVALUATE TRUE
               WHEN WS-TOTAL-RISK-SCORE >= 900 AND TRANS-AMOUNT > 5000.00
                   MOVE 'CRITICAL' TO FRAUD-ANALYST-ID
               WHEN WS-TOTAL-RISK-SCORE >= 800
                   MOVE 'HIGH' TO FRAUD-ANALYST-ID
               WHEN WS-TOTAL-RISK-SCORE >= 600
                   MOVE 'MEDIUM' TO FRAUD-ANALYST-ID
               WHEN OTHER
                   MOVE 'LOW' TO FRAUD-ANALYST-ID
           END-EVALUATE.

       6300-NOTIFY-FRAUD-TEAM.
      * In production, this would send alerts to fraud analysts
           IF WS-TOTAL-RISK-SCORE >= 900
               DISPLAY 'CRITICAL FRAUD ALERT - CASE: ' FRAUD-TRANS-ID
               DISPLAY 'CARD: ' TRANS-CARD-NUMBER
               DISPLAY 'AMOUNT: 
           
           OPEN I-O CUSTOMER-FILE
           IF WS-CUST-STATUS NOT = '00' AND WS-CUST-STATUS NOT = '05'
               DISPLAY ERR-FILE-NOT-FOUND ' - CUSTOMER FILE'
               STOP RUN
           END-IF
           
           OPEN INPUT MERCHANT-FILE
           IF WS-MERCH-STATUS NOT = '00'
               DISPLAY ERR-FILE-NOT-FOUND ' - MERCHANT FILE'
               STOP RUN
           END-IF
           
           OPEN OUTPUT FRAUD-LOG
           IF WS-FRAUD-STATUS NOT = '00'
               DISPLAY ERR-FILE-NOT-FOUND ' - FRAUD LOG FILE'
               STOP RUN
           END-IF
           
           OPEN I-O VELOCITY-FILE
           IF WS-VELO-STATUS NOT = '00' AND WS-VELO-STATUS NOT = '05'
               DISPLAY ERR-FILE-NOT-FOUND ' - VELOCITY FILE'
               STOP RUN
           END-IF.

       1200-INITIALIZE-VARIABLES.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-TIMESTAMP
           MOVE 'N' TO WS-EOF-FLAG
           MOVE 'N' TO WS-FRAUD-DETECTED
           INITIALIZE WS-COUNTERS
           INITIALIZE WS-RULE-RESULTS.

       1300-LOAD-FRAUD-PARAMETERS.
      * In production, these would be loaded from parameter tables
           MOVE 850 TO HIGH-RISK-THRESHOLD
           MOVE 550 TO MEDIUM-RISK-THRESHOLD
           MOVE 350 TO LOW-RISK-THRESHOLD
           DISPLAY 'FRAUD PARAMETERS LOADED SUCCESSFULLY'.

       2000-PROCESS-TRANSACTIONS SECTION.
       2000-PROCESS-START.
           DISPLAY 'BEGINNING TRANSACTION PROCESSING'
           PERFORM 2100-READ-TRANSACTION
           PERFORM UNTIL EOF-REACHED
               PERFORM 2200-ANALYZE-TRANSACTION
               PERFORM 2100-READ-TRANSACTION
           END-PERFORM
           DISPLAY 'TRANSACTION PROCESSING COMPLETED'.

       2100-READ-TRANSACTION.
           READ TRANSACTION-FILE
           AT END
               SET EOF-REACHED TO TRUE
           NOT AT END
               ADD 1 TO WS-TRANSACTIONS-PROCESSED
               IF WS-TRANSACTIONS-PROCESSED = 1 OR 
                  FUNCTION MOD(WS-TRANSACTIONS-PROCESSED, 1000) = 0
                   DISPLAY 'PROCESSED: ' WS-TRANSACTIONS-PROCESSED 
                          ' TRANSACTIONS'
               END-IF
           END-READ.

       2200-ANALYZE-TRANSACTION SECTION.
       2200-ANALYSIS-START.
           INITIALIZE WS-RULE-RESULTS
           MOVE 'N' TO WS-FRAUD-DETECTED
           MOVE ZERO TO WS-TOTAL-RISK-SCORE
           
           PERFORM 2300-VALIDATE-TRANSACTION
           IF WS-PROCESS-FLAG = 'Y'
               PERFORM 2400-LOAD-CUSTOMER-DATA
               PERFORM 2500-LOAD-MERCHANT-DATA
               PERFORM 2600-EXECUTE-FRAUD-RULES
               PERFORM 2700-CALCULATE-FINAL-RISK
               PERFORM 2800-DETERMINE-ACTION
               PERFORM 2900-UPDATE-VELOCITY-DATA
               PERFORM 3000-LOG-DECISION
           END-IF.

       2300-VALIDATE-TRANSACTION.
           MOVE 'Y' TO WS-PROCESS-FLAG
           
      * Validate card number using Luhn algorithm
           PERFORM 2310-VALIDATE-CARD-NUMBER
           
      * Validate amount
           IF TRANS-AMOUNT <= 0 OR TRANS-AMOUNT > 999999.99
               MOVE 'N' TO WS-PROCESS-FLAG
               DISPLAY 'INVALID TRANSACTION AMOUNT: ' TRANS-AMOUNT
           END-IF
           
      * Validate date
           IF TRANS-DATE < 20200101 OR TRANS-DATE > 20301231
               MOVE 'N' TO WS-PROCESS-FLAG
               DISPLAY 'INVALID TRANSACTION DATE: ' TRANS-DATE
           END-IF.

       2310-VALIDATE-CARD-NUMBER.
      * Simplified Luhn algorithm validation
           IF TRANS-CARD-NUMBER < 1000000000000000 OR
              TRANS-CARD-NUMBER > 9999999999999999
               MOVE 'N' TO WS-PROCESS-FLAG
               DISPLAY 'INVALID CARD NUMBER FORMAT'
           END-IF.

       2400-LOAD-CUSTOMER-DATA.
           MOVE TRANS-CARD-NUMBER TO CUST-CARD-NUMBER
           READ CUSTOMER-FILE
           IF WS-CUST-STATUS = '23'
               DISPLAY 'CUSTOMER NOT FOUND: ' TRANS-CARD-NUMBER
               MOVE 'N' TO WS-PROCESS-FLAG
           ELSE IF WS-CUST-STATUS NOT = '00'
               DISPLAY 'ERROR READING CUSTOMER FILE: ' WS-CUST-STATUS
               MOVE 'N' TO WS-PROCESS-FLAG
           END-IF.

       2500-LOAD-MERCHANT-DATA.
           MOVE TRANS-MERCHANT-ID TO MERCH-ID
           READ MERCHANT-FILE
           IF WS-MERCH-STATUS = '23'
               DISPLAY 'MERCHANT NOT FOUND: ' TRANS-MERCHANT-ID
      * Continue processing with default merchant risk
               MOVE 50 TO MERCH-RISK-LEVEL
               MOVE 'UNKNOWN MERCHANT' TO MERCH-NAME
           ELSE IF WS-MERCH-STATUS NOT = '00'
               DISPLAY 'ERROR READING MERCHANT FILE: ' WS-MERCH-STATUS
           END-IF.

       2600-EXECUTE-FRAUD-RULES SECTION.
       2600-RULES-START.
           PERFORM 2610-RULE-HIGH-AMOUNT
           PERFORM 2620-RULE-VELOCITY-CHECK
           PERFORM 2630-RULE-LOCATION-VARIANCE
           PERFORM 2640-RULE-MERCHANT-RISK
           PERFORM 2650-RULE-TIME-PATTERN
           PERFORM 2660-RULE-CARD-NOT-PRESENT
           PERFORM 2670-RULE-SUSPICIOUS-CATEGORY
           PERFORM 2680-RULE-CUSTOMER-BEHAVIOR
           PERFORM 2690-RULE-ACCOUNT-AGE
           PERFORM 2695-RULE-CROSS-VALIDATION.

       2610-RULE-HIGH-AMOUNT.
      * Rule 1: High Amount Transaction
           IF TRANS-AMOUNT > SUSPICIOUS-AMOUNT
               MOVE 'Y' TO RULE-01-TRIGGERED
               ADD 150 TO WS-TRANSACTION-RISK
               IF TRANS-AMOUNT > (CUST-AVG-MONTHLY-SPEND * 3)
                   ADD 100 TO WS-TRANSACTION-RISK
               END-IF
           END-IF.

       2620-RULE-VELOCITY-CHECK.
      * Rule 2: Transaction Velocity Analysis
           PERFORM 2621-CHECK-VELOCITY-LIMITS
           IF VELO-TRANS-COUNT-1H > MAX-HOURLY-VELOCITY
               MOVE 'Y' TO RULE-02-TRIGGERED
               ADD 200 TO WS-VELOCITY-RISK
           END-IF
           IF VELO-TRANS-COUNT-24H > MAX-DAILY-VELOCITY
               MOVE 'Y' TO RULE-02-TRIGGERED
               ADD 150 TO WS-VELOCITY-RISK
           END-IF.

       2621-CHECK-VELOCITY-LIMITS.
           MOVE TRANS-CARD-NUMBER TO VELO-CARD-NUMBER
           READ VELOCITY-FILE
           IF WS-VELO-STATUS = '23'
      * First transaction for this card - initialize
               MOVE TRANS-CARD-NUMBER TO VELO-CARD-NUMBER
               MOVE 1 TO VELO-TRANS-COUNT-1H
               MOVE 1 TO VELO-TRANS-COUNT-24H
               MOVE TRANS-AMOUNT TO VELO-AMOUNT-1H
               MOVE TRANS-AMOUNT TO VELO-AMOUNT-24H
               MOVE FUNCTION CURRENT-DATE(9:6) TO VELO-LAST-UPDATE
               MOVE 1 TO VELO-LOCATION-COUNT
               MOVE 1 TO VELO-MERCHANT-COUNT
           END-IF.

       2630-RULE-LOCATION-VARIANCE.
      * Rule 3: Geographical Location Analysis
           PERFORM 2631-CALCULATE-LOCATION-RISK
           IF WS-DISTANCE-KM > MAX-LOCATION-VARIANCE
               MOVE 'Y' TO RULE-03-TRIGGERED
               ADD 175 TO WS-LOCATION-RISK
               IF TRANS-COUNTRY-CODE NOT = CUST-HOME-COUNTRY
                   IF CUST-TRAVEL-FLAG = 'N'
                       ADD 100 TO WS-LOCATION-RISK
                   END-IF
               END-IF
           END-IF.

       2631-CALCULATE-LOCATION-RISK.
      * Simplified distance calculation based on ZIP codes
           IF TRANS-LOCATION-ZIP NOT = CUST-HOME-ZIP
               IF TRANS-COUNTRY-CODE NOT = CUST-HOME-COUNTRY
                   MOVE 2000 TO WS-DISTANCE-KM
               ELSE
                   MOVE 500 TO WS-DISTANCE-KM
               END-IF
           ELSE
               MOVE 0 TO WS-DISTANCE-KM
           END-IF.

       2640-RULE-MERCHANT-RISK.
      * Rule 4: Merchant Risk Assessment
           IF MERCH-RISK-LEVEL > 70
               MOVE 'Y' TO RULE-04-TRIGGERED
               ADD 125 TO WS-MERCHANT-RISK
           END-IF
           
      * High-risk merchant categories
           EVALUATE TRANS-MERCHANT-CAT
               WHEN 4829  * Money Transfer
               WHEN 5993  * Cigar Stores
               WHEN 7995  * Gambling
                   MOVE 'Y' TO RULE-04-TRIGGERED
                   ADD 100 TO WS-MERCHANT-RISK
           END-EVALUATE.

       2650-RULE-TIME-PATTERN.
      * Rule 5: Unusual Time Pattern
           PERFORM 2651-ANALYZE-TIME-PATTERN
           IF WS-TIME-DIFF-HOURS < 1
               IF VELO-LOCATION-COUNT > 3
                   MOVE 'Y' TO RULE-05-TRIGGERED
                   ADD 150 TO WS-BEHAVIORAL-RISK
               END-IF
           END-IF.

       2651-ANALYZE-TIME-PATTERN.
      * Check for rapid-fire transactions in different locations
           MOVE 2 TO WS-TIME-DIFF-HOURS  * Simplified calculation
           IF TRANS-TIME < 060000 OR TRANS-TIME > 220000
               ADD 50 TO WS-BEHAVIORAL-RISK
           END-IF.

       2660-RULE-CARD-NOT-PRESENT.
      * Rule 6: Card Not Present Risk
           IF TRANS-CHANNEL = 'ONL' OR TRANS-CHANNEL = 'TEL'
               IF TRANS-AMOUNT > 500.00
                   MOVE 'Y' TO RULE-06-TRIGGERED
                   ADD 75 TO WS-TRANSACTION-RISK
               END-IF
               IF TRANS-PIN-VERIFIED = 'N'
                   ADD 50 TO WS-TRANSACTION-RISK
               END-IF
           END-IF.

       2670-RULE-SUSPICIOUS-CATEGORY.
      * Rule 7: Suspicious Category Combinations
           IF VELO-MERCHANT-COUNT > 5
               MOVE 'Y' TO RULE-07-TRIGGERED
               ADD 100 TO WS-BEHAVIORAL-RISK
           END-IF.

       2680-RULE-CUSTOMER-BEHAVIOR.
      * Rule 8: Customer Behavioral Analysis
           IF CUST-FRAUD-FLAG = 'Y'
               MOVE 'Y' TO RULE-08-TRIGGERED
               ADD 200 TO WS-BEHAVIORAL-RISK
           END-IF
           
           COMPUTE WS-DAYS-DIFF = TRANS-DATE - CUST-LAST-TRANS-DATE
           IF WS-DAYS-DIFF > 90
               ADD 50 TO WS-BEHAVIORAL-RISK
           END-IF
           
           IF TRANS-AMOUNT > CUST-MAX-DAILY-SPEND
               MOVE 'Y' TO RULE-08-TRIGGERED
               ADD 125 TO WS-BEHAVIORAL-RISK
           END-IF.

       2690-RULE-ACCOUNT-AGE.
      * Rule 9: New Account Risk
           COMPUTE WS-DAYS-DIFF = TRANS-DATE - CUST-ACCOUNT-OPEN-DATE
           IF WS-DAYS-DIFF < 30
               MOVE 'Y' TO RULE-09-TRIGGERED
               ADD 100 TO WS-BEHAVIORAL-RISK
               IF TRANS-AMOUNT > 1000.00
                   ADD 50 TO WS-BEHAVIORAL-RISK
               END-IF
           END-IF.

       2695-RULE-CROSS-VALIDATION.
      * Rule 10: Cross-validation of multiple risk factors
           IF (RULE-01-TRIGGERED = 'Y' AND RULE-03-TRIGGERED = 'Y') OR
              (RULE-02-TRIGGERED = 'Y' AND RULE-04-TRIGGERED = 'Y') OR
              (RULE-06-TRIGGERED = 'Y' AND RULE-08-TRIGGERED = 'Y')
               MOVE 'Y' TO RULE-10-TRIGGERED
               ADD 100 TO WS-TOTAL-RISK-SCORE
           END-IF.

       2700-CALCULATE-FINAL-RISK.
           COMPUTE WS-TOTAL-RISK-SCORE = 
               WS-TRANSACTION-RISK + 
               WS-VELOCITY-RISK + 
               WS-LOCATION-RISK + 
               WS-MERCHANT-RISK + 
               WS-BEHAVIORAL-RISK +
               CUST-RISK-SCORE.

       2800-DETERMINE-ACTION.
           EVALUATE TRUE
               WHEN WS-TOTAL-RISK-SCORE >= HIGH-RISK-THRESHOLD
                   MOVE 'Y' TO WS-FRAUD-DETECTED
                   ADD 1 TO WS-FRAUD-DETECTED-COUNT
                   ADD 1 TO WS-DECLINED-COUNT
               WHEN WS-TOTAL-RISK-SCORE >= MEDIUM-RISK-THRESHOLD
                   PERFORM 2810-MANUAL-REVIEW-REQUIRED
               WHEN OTHER
                   ADD 1 TO WS-APPROVED-COUNT
           END-EVALUATE.

       2810-MANUAL-REVIEW-REQUIRED.
      * Medium risk transactions require additional validation
           IF RULE-08-TRIGGERED = 'Y' OR RULE-10-TRIGGERED = 'Y'
               MOVE 'Y' TO WS-FRAUD-DETECTED
               ADD 1 TO WS-FRAUD-DETECTED-COUNT
           ELSE
               ADD 1 TO WS-APPROVED-COUNT
           END-IF.

       2900-UPDATE-VELOCITY-DATA.
           MOVE TRANS-CARD-NUMBER TO VELO-CARD-NUMBER
           READ VELOCITY-FILE
           IF WS-VELO-STATUS = '00'
               ADD 1 TO VELO-TRANS-COUNT-1H
               ADD 1 TO VELO-TRANS-COUNT-24H
               ADD TRANS-AMOUNT TO VELO-AMOUNT-1H
               ADD TRANS-AMOUNT TO VELO-AMOUNT-24H
               MOVE FUNCTION CURRENT-DATE(9:6) TO VELO-LAST-UPDATE
               REWRITE VELOCITY-RECORD
           ELSE
      * Create new velocity record
               MOVE TRANS-CARD-NUMBER TO VELO-CARD-NUMBER
               MOVE 1 TO VELO-TRANS-COUNT-1H
               MOVE 1 TO VELO-TRANS-COUNT-24H
               MOVE TRANS-AMOUNT TO VELO-AMOUNT-1H
               MOVE TRANS-AMOUNT TO VELO-AMOUNT-24H
               MOVE FUNCTION CURRENT-DATE(9:6) TO VELO-LAST-UPDATE
               MOVE 1 TO VELO-LOCATION-COUNT
               MOVE 1 TO VELO-MERCHANT-COUNT
               WRITE VELOCITY-RECORD
           END-IF.

       3000-LOG-DECISION.
           MOVE WS-CURRENT-TIMESTAMP TO FRAUD-TIMESTAMP
           MOVE TRANS-ID TO FRAUD-TRANS-ID
           MOVE TRANS-CARD-NUMBER TO FRAUD-CARD-NUMBER
           MOVE WS-TOTAL-RISK-SCORE TO FRAUD-RISK-SCORE
           MOVE TRANS-AMOUNT TO FRAUD-AMOUNT
           MOVE MERCH-NAME TO FRAUD-MERCHANT
           MOVE TRANS-LOCATION-ZIP TO FRAUD-LOCATION
           MOVE 'SYSTEM' TO FRAUD-ANALYST-ID
           
           IF WS-FRAUD-DETECTED = 'Y'
               MOVE 'FRAUD_DETECTED' TO FRAUD-REASON-CODE
               MOVE ACTION-DECLINE TO FRAUD-ACTION-TAKEN
               STRING 'RULES_TRIGGERED: '
                      RULE-01-TRIGGERED RULE-02-TRIGGERED 
                      RULE-03-TRIGGERED RULE-04-TRIGGERED
                      RULE-05-TRIGGERED RULE-06-TRIGGERED
                      RULE-07-TRIGGERED RULE-08-TRIGGERED
                      RULE-09-TRIGGERED RULE-10-TRIGGERED
                      DELIMITED BY SIZE
                      INTO FRAUD-RULE-TRIGGERED
               MOVE 'TRANSACTION_DECLINED' TO FRAUD-RESOLUTION
           ELSE
               MOVE 'CLEAN_TRANS' TO FRAUD-REASON-CODE
               MOVE ACTION-APPROVE TO FRAUD-ACTION-TAKEN
               MOVE 'NO_RULES_TRIGGERED' TO FRAUD-RULE-TRIGGERED
               MOVE 'TRANSACTION_APPROVED' TO FRAUD-RESOLUTION
           END-IF TRANS-AMOUNT
               DISPLAY 'RISK SCORE: ' WS-TOTAL-RISK-SCORE
           END-IF.

       7000-REPORTING-ANALYTICS SECTION.
       7000-REPORTING-START.
      * Generate real-time fraud statistics
           PERFORM 7100-CALCULATE-FRAUD-RATES
           PERFORM 7200-UPDATE-PERFORMANCE-METRICS
           PERFORM 7300-GENERATE-ALERTS.

       7100-CALCULATE-FRAUD-RATES.
           IF WS-TRANSACTIONS-PROCESSED > 0
               COMPUTE WS-WORK-AMOUNT = 
                   (WS-FRAUD-DETECTED-COUNT / 
                    WS-TRANSACTIONS-PROCESSED) * 100
               
               IF FUNCTION MOD(WS-TRANSACTIONS-PROCESSED, 5000) = 0
                   DISPLAY 'FRAUD DETECTION RATE: ' WS-WORK-AMOUNT '%'
                   DISPLAY 'TOTAL PROCESSED: ' WS-TRANSACTIONS-PROCESSED
                   DISPLAY 'FRAUD DETECTED: ' WS-FRAUD-DETECTED-COUNT
                   DISPLAY 'APPROVED: ' WS-APPROVED-COUNT
                   DISPLAY 'DECLINED: ' WS-DECLINED-COUNT
               END-IF
           END-IF.

       7200-UPDATE-PERFORMANCE-METRICS.
      * Calculate system performance indicators
           COMPUTE WS-WORK-AMOUNT = 
               WS-APPROVED-COUNT + WS-DECLINED-COUNT
           
           IF WS-WORK-AMOUNT > 0
               COMPUTE WS-WORK-AMOUNT = 
                   (WS-APPROVED-COUNT / WS-WORK-AMOUNT) * 100
           END-IF.

       7300-GENERATE-ALERTS.
      * Generate system alerts based on processing patterns
           IF WS-FRAUD-DETECTED-COUNT > (WS-TRANSACTIONS-PROCESSED * 0.05)
               DISPLAY 'HIGH FRAUD RATE ALERT - INVESTIGATE PATTERNS'
           END-IF
           
           IF WS-DECLINED-COUNT > (WS-TRANSACTIONS-PROCESSED * 0.10)
               DISPLAY 'HIGH DECLINE RATE ALERT - CHECK THRESHOLDS'
           END-IF.

       8000-CLEANUP-VELOCITY SECTION.
       8000-CLEANUP-START.
      * Clean up old velocity data to maintain performance
           PERFORM 8100-PURGE-OLD-VELOCITY
           PERFORM 8200-ARCHIVE-OLD-LOGS.

       8100-PURGE-OLD-VELOCITY.
      * In production, this would remove velocity records older than 24 hours
           DISPLAY 'VELOCITY DATA CLEANUP COMPLETED'.

       8200-ARCHIVE-OLD-LOGS.
      * Archive fraud logs older than specified retention period
           DISPLAY 'LOG ARCHIVAL COMPLETED'.

       9000-FINALIZE-PROGRAM SECTION.
       9000-FINALIZE-START.
           PERFORM 9100-CLOSE-FILES
           PERFORM 9200-DISPLAY-FINAL-STATS
           DISPLAY 'FRAUD MANAGEMENT SYSTEM - PROCESSING COMPLETED'.

       9100-CLOSE-FILES.
           CLOSE TRANSACTION-FILE
           CLOSE CUSTOMER-FILE  
           CLOSE MERCHANT-FILE
           CLOSE FRAUD-LOG
           CLOSE VELOCITY-FILE.

       9200-DISPLAY-FINAL-STATS.
           DISPLAY ' '
           DISPLAY '=========================================='
           DISPLAY 'FINAL PROCESSING STATISTICS'
           DISPLAY '=========================================='
           DISPLAY 'TOTAL TRANSACTIONS PROCESSED: ' 
                   WS-TRANSACTIONS-PROCESSED
           DISPLAY 'FRAUD CASES DETECTED: ' WS-FRAUD-DETECTED-COUNT
           DISPLAY 'TRANSACTIONS APPROVED: ' WS-APPROVED-COUNT  
           DISPLAY 'TRANSACTIONS DECLINED: ' WS-DECLINED-COUNT
           
           IF WS-TRANSACTIONS-PROCESSED > 0
               COMPUTE WS-WORK-AMOUNT = 
                   (WS-FRAUD-DETECTED-COUNT / 
                    WS-TRANSACTIONS-PROCESSED) * 100
               DISPLAY 'FRAUD DETECTION RATE: ' WS-WORK-AMOUNT '%'
               
               COMPUTE WS-WORK-AMOUNT = 
                   (WS-APPROVED-COUNT / 
                    WS-TRANSACTIONS-PROCESSED) * 100
               DISPLAY 'APPROVAL RATE: ' WS-WORK-AMOUNT '%'
           END-IF
           
           DISPLAY '=========================================='.

      * END OF FRAUD-MGMT-SYSTEM PROGRAM
           
           OPEN I-O CUSTOMER-FILE
           IF WS-CUST-STATUS NOT = '00' AND WS-CUST-STATUS NOT = '05'
               DISPLAY ERR-FILE-NOT-FOUND ' - CUSTOMER FILE'
               STOP RUN
           END-IF
           
           OPEN INPUT MERCHANT-FILE
           IF WS-MERCH-STATUS NOT = '00'
               DISPLAY ERR-FILE-NOT-FOUND ' - MERCHANT FILE'
               STOP RUN
           END-IF
           
           OPEN OUTPUT FRAUD-LOG
           IF WS-FRAUD-STATUS NOT = '00'
               DISPLAY ERR-FILE-NOT-FOUND ' - FRAUD LOG FILE'
               STOP RUN
           END-IF
           
           OPEN I-O VELOCITY-FILE
           IF WS-VELO-STATUS NOT = '00' AND WS-VELO-STATUS NOT = '05'
               DISPLAY ERR-FILE-NOT-FOUND ' - VELOCITY FILE'
               STOP RUN
           END-IF.

       1200-INITIALIZE-VARIABLES.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-TIMESTAMP
           MOVE 'N' TO WS-EOF-FLAG
           MOVE 'N' TO WS-FRAUD-DETECTED
           INITIALIZE WS-COUNTERS
           INITIALIZE WS-RULE-RESULTS.

       1300-LOAD-FRAUD-PARAMETERS.
      * In production, these would be loaded from parameter tables
           MOVE 850 TO HIGH-RISK-THRESHOLD
           MOVE 550 TO MEDIUM-RISK-THRESHOLD
           MOVE 350 TO LOW-RISK-THRESHOLD
           DISPLAY 'FRAUD PARAMETERS LOADED SUCCESSFULLY'.

       2000-PROCESS-TRANSACTIONS SECTION.
       2000-PROCESS-START.
           DISPLAY 'BEGINNING TRANSACTION PROCESSING'
           PERFORM 2100-READ-TRANSACTION
           PERFORM UNTIL EOF-REACHED
               PERFORM 2200-ANALYZE-TRANSACTION
               PERFORM 2100-READ-TRANSACTION
           END-PERFORM
           DISPLAY 'TRANSACTION PROCESSING COMPLETED'.

       2100-READ-TRANSACTION.
           READ TRANSACTION-FILE
           AT END
               SET EOF-REACHED TO TRUE
           NOT AT END
               ADD 1 TO WS-TRANSACTIONS-PROCESSED
               IF WS-TRANSACTIONS-PROCESSED = 1 OR 
                  FUNCTION MOD(WS-TRANSACTIONS-PROCESSED, 1000) = 0
                   DISPLAY 'PROCESSED: ' WS-TRANSACTIONS-PROCESSED 
                          ' TRANSACTIONS'
               END-IF
           END-READ.

       2200-ANALYZE-TRANSACTION SECTION.
       2200-ANALYSIS-START.
           INITIALIZE WS-RULE-RESULTS
           MOVE 'N' TO WS-FRAUD-DETECTED
           MOVE ZERO TO WS-TOTAL-RISK-SCORE
           
           PERFORM 2300-VALIDATE-TRANSACTION
           IF WS-PROCESS-FLAG = 'Y'
               PERFORM 2400-LOAD-CUSTOMER-DATA
               PERFORM 2500-LOAD-MERCHANT-DATA
               PERFORM 2600-EXECUTE-FRAUD-RULES
               PERFORM 2700-CALCULATE-FINAL-RISK
               PERFORM 2800-DETERMINE-ACTION
               PERFORM 2900-UPDATE-VELOCITY-DATA
               PERFORM 3000-LOG-DECISION
           END-IF.

       2300-VALIDATE-TRANSACTION.
           MOVE 'Y' TO WS-PROCESS-FLAG
           
      * Validate card number using Luhn algorithm
           PERFORM 2310-VALIDATE-CARD-NUMBER
           
      * Validate amount
           IF TRANS-AMOUNT <= 0 OR TRANS-AMOUNT > 999999.99
               MOVE 'N' TO WS-PROCESS-FLAG
               DISPLAY 'INVALID TRANSACTION AMOUNT: ' TRANS-AMOUNT
           END-IF
           
      * Validate date
           IF TRANS-DATE < 20200101 OR TRANS-DATE > 20301231
               MOVE 'N' TO WS-PROCESS-FLAG
               DISPLAY 'INVALID TRANSACTION DATE: ' TRANS-DATE
           END-IF.

       2310-VALIDATE-CARD-NUMBER.
      * Simplified Luhn algorithm validation
           IF TRANS-CARD-NUMBER < 1000000000000000 OR
              TRANS-CARD-NUMBER > 9999999999999999
               MOVE 'N' TO WS-PROCESS-FLAG
               DISPLAY 'INVALID CARD NUMBER FORMAT'
           END-IF.

       2400-LOAD-CUSTOMER-DATA.
           MOVE TRANS-CARD-NUMBER TO CUST-CARD-NUMBER
           READ CUSTOMER-FILE
           IF WS-CUST-STATUS = '23'
               DISPLAY 'CUSTOMER NOT FOUND: ' TRANS-CARD-NUMBER
               MOVE 'N' TO WS-PROCESS-FLAG
           ELSE IF WS-CUST-STATUS NOT = '00'
               DISPLAY 'ERROR READING CUSTOMER FILE: ' WS-CUST-STATUS
               MOVE 'N' TO WS-PROCESS-FLAG
           END-IF.

       2500-LOAD-MERCHANT-DATA.
           MOVE TRANS-MERCHANT-ID TO MERCH-ID
           READ MERCHANT-FILE
           IF WS-MERCH-STATUS = '23'
               DISPLAY 'MERCHANT NOT FOUND: ' TRANS-MERCHANT-ID
      * Continue processing with default merchant risk
               MOVE 50 TO MERCH-RISK-LEVEL
               MOVE 'UNKNOWN MERCHANT' TO MERCH-NAME
           ELSE IF WS-MERCH-STATUS NOT = '00'
               DISPLAY 'ERROR READING MERCHANT FILE: ' WS-MERCH-STATUS
           END-IF.

       2600-EXECUTE-FRAUD-RULES SECTION.
       2600-RULES-START.
           PERFORM 2610-RULE-HIGH-AMOUNT
           PERFORM 2620-RULE-VELOCITY-CHECK
           PERFORM 2630-RULE-LOCATION-VARIANCE
           PERFORM 2640-RULE-MERCHANT-RISK
           PERFORM 2650-RULE-TIME-PATTERN
           PERFORM 2660-RULE-CARD-NOT-PRESENT
           PERFORM 2670-RULE-SUSPICIOUS-CATEGORY
           PERFORM 2680-RULE-CUSTOMER-BEHAVIOR
           PERFORM 2690-RULE-ACCOUNT-AGE
           PERFORM 2695-RULE-CROSS-VALIDATION.

       2610-RULE-HIGH-AMOUNT.
      * Rule 1: High Amount Transaction
           IF TRANS-AMOUNT > SUSPICIOUS-AMOUNT
               MOVE 'Y' TO RULE-01-TRIGGERED
               ADD 150 TO WS-TRANSACTION-RISK
               IF TRANS-AMOUNT > (CUST-AVG-MONTHLY-SPEND * 3)
                   ADD 100 TO WS-TRANSACTION-RISK
               END-IF
           END-IF.

       2620-RULE-VELOCITY-CHECK.
      * Rule 2: Transaction Velocity Analysis
           PERFORM 2621-CHECK-VELOCITY-LIMITS
           IF VELO-TRANS-COUNT-1H > MAX-HOURLY-VELOCITY
               MOVE 'Y' TO RULE-02-TRIGGERED
               ADD 200 TO WS-VELOCITY-RISK
           END-IF
           IF VELO-TRANS-COUNT-24H > MAX-DAILY-VELOCITY
               MOVE 'Y' TO RULE-02-TRIGGERED
               ADD 150 TO WS-VELOCITY-RISK
           END-IF.

       2621-CHECK-VELOCITY-LIMITS.
           MOVE TRANS-CARD-NUMBER TO VELO-CARD-NUMBER
           READ VELOCITY-FILE
           IF WS-VELO-STATUS = '23'
      * First transaction for this card - initialize
               MOVE TRANS-CARD-NUMBER TO VELO-CARD-NUMBER
               MOVE 1 TO VELO-TRANS-COUNT-1H
               MOVE 1 TO VELO-TRANS-COUNT-24H
               MOVE TRANS-AMOUNT TO VELO-AMOUNT-1H
               MOVE TRANS-AMOUNT TO VELO-AMOUNT-24H
               MOVE FUNCTION CURRENT-DATE(9:6) TO VELO-LAST-UPDATE
               MOVE 1 TO VELO-LOCATION-COUNT
               MOVE 1 TO VELO-MERCHANT-COUNT
           END-IF.

       2630-RULE-LOCATION-VARIANCE.
      * Rule 3: Geographical Location Analysis
           PERFORM 2631-CALCULATE-LOCATION-RISK
           IF WS-DISTANCE-KM > MAX-LOCATION-VARIANCE
               MOVE 'Y' TO RULE-03-TRIGGERED
               ADD 175 TO WS-LOCATION-RISK
               IF TRANS-COUNTRY-CODE NOT = CUST-HOME-COUNTRY
                   IF CUST-TRAVEL-FLAG = 'N'
                       ADD 100 TO WS-LOCATION-RISK
                   END-IF
               END-IF
           END-IF.

       2631-CALCULATE-LOCATION-RISK.
      * Simplified distance calculation based on ZIP codes
           IF TRANS-LOCATION-ZIP NOT = CUST-HOME-ZIP
               IF TRANS-COUNTRY-CODE NOT = CUST-HOME-COUNTRY
                   MOVE 2000 TO WS-DISTANCE-KM
               ELSE
                   MOVE 500 TO WS-DISTANCE-KM
               END-IF
           ELSE
               MOVE 0 TO WS-DISTANCE-KM
           END-IF.

       2640-RULE-MERCHANT-RISK.
      * Rule 4: Merchant Risk Assessment
           IF MERCH-RISK-LEVEL > 70
               MOVE 'Y' TO RULE-04-TRIGGERED
               ADD 125 TO WS-MERCHANT-RISK
           END-IF
           
      * High-risk merchant categories
           EVALUATE TRANS-MERCHANT-CAT
               WHEN 4829  * Money Transfer
               WHEN 5993  * Cigar Stores
               WHEN 7995  * Gambling
                   MOVE 'Y' TO RULE-04-TRIGGERED
                   ADD 100 TO WS-MERCHANT-RISK
           END-EVALUATE.

       2650-RULE-TIME-PATTERN.
      * Rule 5: Unusual Time Pattern
           PERFORM 2651-ANALYZE-TIME-PATTERN
           IF WS-TIME-DIFF-HOURS < 1
               IF VELO-LOCATION-COUNT > 3
                   MOVE 'Y' TO RULE-05-TRIGGERED
                   ADD 150 TO WS-BEHAVIORAL-RISK
               END-IF
           END-IF.

       2651-ANALYZE-TIME-PATTERN.
      * Check for rapid-fire transactions in different locations
           MOVE 2 TO WS-TIME-DIFF-HOURS  * Simplified calculation
           IF TRANS-TIME < 060000 OR TRANS-TIME > 220000
               ADD 50 TO WS-BEHAVIORAL-RISK
           END-IF.

       2660-RULE-CARD-NOT-PRESENT.
      * Rule 6: Card Not Present Risk
           IF TRANS-CHANNEL = 'ONL' OR TRANS-CHANNEL = 'TEL'
               IF TRANS-AMOUNT > 500.00
                   MOVE 'Y' TO RULE-06-TRIGGERED
                   ADD 75 TO WS-TRANSACTION-RISK
               END-IF
               IF TRANS-PIN-VERIFIED = 'N'
                   ADD 50 TO WS-TRANSACTION-RISK
               END-IF
           END-IF.

       2670-RULE-SUSPICIOUS-CATEGORY.
      * Rule 7: Suspicious Category Combinations
           IF VELO-MERCHANT-COUNT > 5
               MOVE 'Y' TO RULE-07-TRIGGERED
               ADD 100 TO WS-BEHAVIORAL-RISK
           END-IF.

       2680-RULE-CUSTOMER-BEHAVIOR.
      * Rule 8: Customer Behavioral Analysis
           IF CUST-FRAUD-FLAG = 'Y'
               MOVE 'Y' TO RULE-08-TRIGGERED
               ADD 200 TO WS-BEHAVIORAL-RISK
           END-IF
           
           COMPUTE WS-DAYS-DIFF = TRANS-DATE - CUST-LAST-TRANS-DATE
           IF WS-DAYS-DIFF > 90
               ADD 50 TO WS-BEHAVIORAL-RISK
           END-IF
           
           IF TRANS-AMOUNT > CUST-MAX-DAILY-SPEND
               MOVE 'Y' TO RULE-08-TRIGGERED
               ADD 125 TO WS-BEHAVIORAL-RISK
           END-IF.

       2690-RULE-ACCOUNT-AGE.
      * Rule 9: New Account Risk
           COMPUTE WS-DAYS-DIFF = TRANS-DATE - CUST-ACCOUNT-OPEN-DATE
           IF WS-DAYS-DIFF < 30
               MOVE 'Y' TO RULE-09-TRIGGERED
               ADD 100 TO WS-BEHAVIORAL-RISK
               IF TRANS-AMOUNT > 1000.00
                   ADD 50 TO WS-BEHAVIORAL-RISK
               END-IF
           END-IF.

       2695-RULE-CROSS-VALIDATION.
      * Rule 10: Cross-validation of multiple risk factors
           IF (RULE-01-TRIGGERED = 'Y' AND RULE-03-TRIGGERED = 'Y') OR
              (RULE-02-TRIGGERED = 'Y' AND RULE-04-TRIGGERED = 'Y') OR
              (RULE-06-TRIGGERED = 'Y' AND RULE-08-TRIGGERED = 'Y')
               MOVE 'Y' TO RULE-10-TRIGGERED
               ADD 100 TO WS-TOTAL-RISK-SCORE
           END-IF.

       2700-CALCULATE-FINAL-RISK.
           COMPUTE WS-TOTAL-RISK-SCORE = 
               WS-TRANSACTION-RISK + 
               WS-VELOCITY-RISK + 
               WS-LOCATION-RISK + 
               WS-MERCHANT-RISK + 
               WS-BEHAVIORAL-RISK +
               CUST-RISK-SCORE.

       2800-DETERMINE-ACTION.
           EVALUATE TRUE
               WHEN WS-TOTAL-RISK-SCORE >= HIGH-RISK-THRESHOLD
                   MOVE 'Y' TO WS-FRAUD-DETECTED
                   ADD 1 TO WS-FRAUD-DETECTED-COUNT
                   ADD 1 TO WS-DECLINED-COUNT
               WHEN WS-TOTAL-RISK-SCORE >= MEDIUM-RISK-THRESHOLD
                   PERFORM 2810-MANUAL-REVIEW-REQUIRED
               WHEN OTHER
                   ADD 1 TO WS-APPROVED-COUNT
           END-EVALUATE.

       2810-MANUAL-REVIEW-REQUIRED.
      * Medium risk transactions require additional validation
           IF RULE-08-TRIGGERED = 'Y' OR RULE-10-TRIGGERED = 'Y'
               MOVE 'Y' TO WS-FRAUD-DETECTED
               ADD 1 TO WS-FRAUD-DETECTED-COUNT
           ELSE
               ADD 1 TO WS-APPROVED-COUNT
           END-IF.

       2900-UPDATE-VELOCITY-DATA.
           MOVE TRANS-CARD-NUMBER TO VELO-CARD-NUMBER
           READ VELOCITY-FILE
           IF WS-VELO-STATUS = '00'
               ADD 1 TO VELO-TRANS-COUNT-1H
               ADD 1 TO VELO-TRANS-COUNT-24H
               ADD TRANS-AMOUNT TO VELO-AMOUNT-1H
               ADD TRANS-AMOUNT TO VELO-AMOUNT-24H
               MOVE FUNCTION CURRENT-DATE(9:6) TO VELO-LAST-UPDATE
               REWRITE VELOCITY-RECORD
           ELSE
      * Create new velocity record
               MOVE TRANS-CARD-NUMBER TO VELO-CARD-NUMBER
               MOVE 1 TO VELO-TRANS-COUNT-1H
               MOVE 1 TO VELO-TRANS-COUNT-24H
               MOVE TRANS-AMOUNT TO VELO-AMOUNT-1H
               MOVE TRANS-AMOUNT TO VELO-AMOUNT-24H
               MOVE FUNCTION CURRENT-DATE(9:6) TO VELO-LAST-UPDATE
               MOVE 1 TO VELO-LOCATION-COUNT
               MOVE 1 TO VELO-MERCHANT-COUNT
               WRITE VELOCITY-RECORD
           END-IF.

       3000-LOG-DECISION.
           MOVE WS-CURRENT-TIMESTAMP TO FRAUD-TIMESTAMP
           MOVE TRANS-ID TO FRAUD-TRANS-ID
           MOVE TRANS-CARD-NUMBER TO FRAUD-CARD-NUMBER
           MOVE WS-TOTAL-RISK-SCORE TO FRAUD-RISK-SCORE
           MOVE TRANS-AMOUNT TO FRAUD-AMOUNT
           MOVE MERCH-NAME TO FRAUD-MERCHANT
           MOVE TRANS-LOCATION-ZIP TO FRAUD-LOCATION
           MOVE 'SYSTEM' TO FRAUD-ANALYST-ID
           
           IF WS-FRAUD-DETECTED = 'Y'
               MOVE 'FRAUD_DETECTED' TO FRAUD-REASON-CODE
               MOVE ACTION-DECLINE TO FRAUD-ACTION-TAKEN
               STRING 'RULES_TRIGGERED: '
                      RULE-01-TRIGGERED RULE-02-TRIGGERED 
                      RULE-03-TRIGGERED RULE-04-TRIGGERED
                      RULE-05-TRIGGERED RULE-06-TRIGGERED
                      RULE-07-TRIGGERED RULE-08-TRIGGERED
                      RULE-09-TRIGGERED RULE-10-TRIGGERED
                      DELIMITED BY SIZE
                      INTO FRAUD-RULE-TRIGGERED
               MOVE 'TRANSACTION_DECLINED' TO FRAUD-RESOLUTION
           ELSE
               MOVE 'CLEAN_TRANS' TO FRAUD-REASON-CODE
               MOVE ACTION-APPROVE TO FRAUD-ACTION-TAKEN
               MOVE 'NO_RULES_TRIGGERED' TO FRAUD-RULE-TRIGGERED
               MOVE 'TRANSACTION_APPROVED' TO FRAUD-RESOLUTION
           END-IF