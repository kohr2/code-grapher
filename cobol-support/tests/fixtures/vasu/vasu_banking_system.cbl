*================================================================
      * ENTERPRISE BANKING SYSTEM - COMPLETE COBOL IMPLEMENTATION
      * Simulates real-world bank operations with 4 integrated systems
      *================================================================

      *================================================================
      * COMMON COPYBOOKS AND DATA STRUCTURES
      *================================================================

      *----------------------------------------------------------------
      * CUSTOMER-RECORD COPYBOOK (CUSTCOPY)
      *----------------------------------------------------------------
       01  CUSTOMER-RECORD.
           05  CUST-ID                 PIC 9(10).
           05  CUST-SSN                PIC 9(9).
           05  CUST-FIRST-NAME         PIC X(20).
           05  CUST-LAST-NAME          PIC X(25).
           05  CUST-DOB                PIC 9(8).
           05  CUST-PHONE              PIC 9(10).
           05  CUST-EMAIL              PIC X(50).
           05  CUST-ADDRESS.
               10  ADDR-LINE1          PIC X(35).
               10  ADDR-LINE2          PIC X(35).
               10  ADDR-CITY           PIC X(20).
               10  ADDR-STATE          PIC X(2).
               10  ADDR-ZIP            PIC 9(5).
           05  CUST-CREDIT-SCORE       PIC 9(3).
           05  CUST-STATUS             PIC X(1).
               88  CUST-ACTIVE         VALUE 'A'.
               88  CUST-INACTIVE       VALUE 'I'.
               88  CUST-DECEASED       VALUE 'D'.
           05  CUST-CREATE-DATE        PIC 9(8).
           05  CUST-LAST-UPDATE        PIC 9(8).

      *----------------------------------------------------------------
      * ACCOUNT-RECORD COPYBOOK (ACCTCOPY)
      *----------------------------------------------------------------
       01  ACCOUNT-RECORD.
           05  ACCT-NUMBER             PIC 9(12).
           05  ACCT-CUST-ID            PIC 9(10).
           05  ACCT-TYPE               PIC X(2).
               88  CHECKING-ACCT       VALUE 'CK'.
               88  SAVINGS-ACCT        VALUE 'SV'.
               88  BUSINESS-ACCT       VALUE 'BZ'.
               88  CREDIT-CARD         VALUE 'CC'.
               88  MORTGAGE-ACCT       VALUE 'MG'.
           05  ACCT-STATUS             PIC X(1).
               88  ACCT-ACTIVE         VALUE 'A'.
               88  ACCT-CLOSED         VALUE 'C'.
               88  ACCT-FROZEN         VALUE 'F'.
               88  ACCT-DORMANT        VALUE 'D'.
           05  ACCT-OPEN-DATE          PIC 9(8).
           05  ACCT-CLOSE-DATE         PIC 9(8).
           05  ACCT-CURRENT-BALANCE    PIC S9(13)V99 COMP-3.
           05  ACCT-AVAILABLE-BALANCE  PIC S9(13)V99 COMP-3.
           05  ACCT-CREDIT-LIMIT       PIC S9(13)V99 COMP-3.
           05  ACCT-INTEREST-RATE      PIC S9(3)V99 COMP-3.
           05  ACCT-OVERDRAFT-LIMIT    PIC S9(9)V99 COMP-3.
           05  ACCT-LAST-STMT-DATE     PIC 9(8).
           05  ACCT-LAST-UPDATE        PIC 9(8).

      *----------------------------------------------------------------
      * TRANSACTION-RECORD COPYBOOK (TRANCOPY)
      *----------------------------------------------------------------
       01  TRANSACTION-RECORD.
           05  TRAN-ID                 PIC 9(15).
           05  TRAN-ACCT-NUMBER        PIC 9(12).
           05  TRAN-TYPE               PIC X(3).
               88  DEPOSIT             VALUE 'DEP'.
               88  WITHDRAWAL          VALUE 'WTH'.
               88  TRANSFER            VALUE 'TRF'.
               88  PAYMENT             VALUE 'PAY'.
               88  INTEREST            VALUE 'INT'.
               88  FEE                 VALUE 'FEE'.
               88  REVERSAL            VALUE 'REV'.
               88  PURCHASE            VALUE 'PUR'.
           05  TRAN-AMOUNT             PIC S9(13)V99 COMP-3.
           05  TRAN-DATE               PIC 9(8).
           05  TRAN-TIME               PIC 9(6).
           05  TRAN-DESCRIPTION        PIC X(50).
           05  TRAN-MERCHANT-ID        PIC X(15).
           05  TRAN-LOCATION           PIC X(30).
           05  TRAN-STATUS             PIC X(1).
               88  TRAN-PENDING        VALUE 'P'.
               88  TRAN-POSTED         VALUE 'C'.
               88  TRAN-REVERSED       VALUE 'R'.
               88  TRAN-DECLINED       VALUE 'D'.
           05  TRAN-AUTH-CODE          PIC X(6).
           05  TRAN-REFERENCE          PIC X(20).

      *----------------------------------------------------------------
      * CREDIT CARD RECORD COPYBOOK (CCCOPY)
      *----------------------------------------------------------------
       01  CREDIT-CARD-RECORD.
           05  CC-NUMBER               PIC 9(16).
           05  CC-ACCT-NUMBER          PIC 9(12).
           05  CC-CUST-ID              PIC 9(10).
           05  CC-TYPE                 PIC X(2).
               88  VISA-CARD           VALUE 'VI'.
               88  MASTERCARD          VALUE 'MC'.
               88  AMEX-CARD           VALUE 'AX'.
           05  CC-STATUS               PIC X(1).
               88  CC-ACTIVE           VALUE 'A'.
               88  CC-BLOCKED          VALUE 'B'.
               88  CC-EXPIRED          VALUE 'E'.
               88  CC-STOLEN           VALUE 'S'.
           05  CC-EXPIRY-DATE          PIC 9(4).
           05  CC-CVV                  PIC 9(3).
           05  CC-ISSUE-DATE           PIC 9(8).
           05  CC-CREDIT-LIMIT         PIC S9(9)V99 COMP-3.
           05  CC-AVAILABLE-CREDIT     PIC S9(9)V99 COMP-3.
           05  CC-CASH-ADVANCE-LIMIT   PIC S9(9)V99 COMP-3.
           05  CC-APR                  PIC S9(3)V99 COMP-3.

      *================================================================
      * SYSTEM 1: ACCOUNT MANAGEMENT SYSTEM (ACCTMGMT)
      *================================================================

      *----------------------------------------------------------------
      * ACCT001 - ACCOUNT CREATION PROGRAM
      *----------------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCT001.
       AUTHOR. ENTERPRISE-BANKING-SYSTEM.
       DATE-WRITTEN. 2025-07-20.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO 'CUSTMAST'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUST-ID
               FILE STATUS IS WS-CUST-STATUS.
           
           SELECT ACCOUNT-FILE ASSIGN TO 'ACCTMAST'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ACCT-NUMBER
               FILE STATUS IS WS-ACCT-STATUS.
               
           SELECT AUDIT-FILE ASSIGN TO 'AUDITLOG'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-AUDIT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
       COPY CUSTCOPY.
       
       FD  ACCOUNT-FILE.
       COPY ACCTCOPY.
       
       FD  AUDIT-FILE.
       01  AUDIT-RECORD            PIC X(200).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-CUST-STATUS          PIC XX.
           05  WS-ACCT-STATUS          PIC XX.
           05  WS-AUDIT-STATUS         PIC XX.

       01  WS-WORK-AREAS.
           05  WS-CURRENT-DATE         PIC 9(8).
           05  WS-CURRENT-TIME         PIC 9(6).
           05  WS-NEW-ACCOUNT-NUMBER   PIC 9(12).
           05  WS-RESPONSE-CODE        PIC X(2).
               88  SUCCESS             VALUE '00'.
               88  CUSTOMER-NOT-FOUND  VALUE '01'.
               88  ACCOUNT-EXISTS      VALUE '02'.
               88  INVALID-TYPE        VALUE '03'.
               88  CREDIT-CHECK-FAIL   VALUE '04'.

       01  WS-INPUT-DATA.
           05  WS-INPUT-CUST-ID        PIC 9(10).
           05  WS-INPUT-ACCT-TYPE      PIC X(2).
           05  WS-INPUT-INITIAL-DEP    PIC S9(13)V99 COMP-3.
           05  WS-INPUT-CREDIT-LIMIT   PIC S9(13)V99 COMP-3.

       PROCEDURE DIVISION.
       0000-MAIN-CONTROL.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-REQUEST
           PERFORM 3000-FINALIZE
           GOBACK.

       1000-INITIALIZE.
           OPEN INPUT CUSTOMER-FILE
           OPEN I-O ACCOUNT-FILE
           OPEN OUTPUT AUDIT-FILE
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           ACCEPT WS-CURRENT-TIME FROM TIME
           MOVE SPACES TO WS-RESPONSE-CODE.

       2000-PROCESS-REQUEST.
      *    In real implementation, this would accept input from CICS
      *    For demo purposes, we'll simulate with hard-coded values
           MOVE 1234567890 TO WS-INPUT-CUST-ID
           MOVE 'CK' TO WS-INPUT-ACCT-TYPE
           MOVE 1000.00 TO WS-INPUT-INITIAL-DEP
           
           PERFORM 2100-VALIDATE-CUSTOMER
           IF SUCCESS
               PERFORM 2200-GENERATE-ACCOUNT-NUMBER
               PERFORM 2300-CREATE-ACCOUNT-RECORD
               PERFORM 2400-WRITE-AUDIT-LOG
           END-IF.

       2100-VALIDATE-CUSTOMER.
           MOVE WS-INPUT-CUST-ID TO CUST-ID
           READ CUSTOMER-FILE
           IF WS-CUST-STATUS = '00'
               IF CUST-ACTIVE
                   MOVE '00' TO WS-RESPONSE-CODE
               ELSE
                   MOVE '01' TO WS-RESPONSE-CODE
               END-IF
           ELSE
               MOVE '01' TO WS-RESPONSE-CODE
           END-IF.

       2200-GENERATE-ACCOUNT-NUMBER.
      *    Simple account number generation logic
           COMPUTE WS-NEW-ACCOUNT-NUMBER = 
               WS-INPUT-CUST-ID * 100 + 
               FUNCTION CURRENT-DATE(7:2).

       2300-CREATE-ACCOUNT-RECORD.
           INITIALIZE ACCOUNT-RECORD
           MOVE WS-NEW-ACCOUNT-NUMBER TO ACCT-NUMBER
           MOVE WS-INPUT-CUST-ID TO ACCT-CUST-ID
           MOVE WS-INPUT-ACCT-TYPE TO ACCT-TYPE
           MOVE 'A' TO ACCT-STATUS
           MOVE WS-CURRENT-DATE TO ACCT-OPEN-DATE
           MOVE WS-INPUT-INITIAL-DEP TO ACCT-CURRENT-BALANCE
           MOVE WS-INPUT-INITIAL-DEP TO ACCT-AVAILABLE-BALANCE
           
           IF CREDIT-CARD
               MOVE WS-INPUT-CREDIT-LIMIT TO ACCT-CREDIT-LIMIT
               MOVE 18.99 TO ACCT-INTEREST-RATE
           END-IF
           
           WRITE ACCOUNT-RECORD
           IF WS-ACCT-STATUS = '00'
               MOVE '00' TO WS-RESPONSE-CODE
           ELSE
               MOVE '02' TO WS-RESPONSE-CODE
           END-IF.

       2400-WRITE-AUDIT-LOG.
           STRING 'ACCT001|' WS-CURRENT-DATE '|' WS-CURRENT-TIME
                  '|ACCOUNT_CREATED|' WS-NEW-ACCOUNT-NUMBER
                  '|CUST:' WS-INPUT-CUST-ID
                  '|TYPE:' WS-INPUT-ACCT-TYPE
                  DELIMITED BY SIZE INTO AUDIT-RECORD
           WRITE AUDIT-RECORD.

       3000-FINALIZE.
           CLOSE CUSTOMER-FILE
           CLOSE ACCOUNT-FILE
           CLOSE AUDIT-FILE.

      *----------------------------------------------------------------
      * ACCT002 - ACCOUNT INQUIRY PROGRAM
      *----------------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCT002.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO 'ACCTMAST'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ACCT-NUMBER
               FILE STATUS IS WS-ACCT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE.
       COPY ACCTCOPY.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-ACCT-STATUS          PIC XX.

       01  WS-INPUT-DATA.
           05  WS-INQUIRY-ACCT-NUMBER  PIC 9(12).

       01  WS-OUTPUT-DATA.
           05  WS-BALANCE-INFO.
               10  WS-CURRENT-BAL      PIC S9(13)V99 COMP-3.
               10  WS-AVAILABLE-BAL    PIC S9(13)V99 COMP-3.
               10  WS-CREDIT-LIMIT     PIC S9(13)V99 COMP-3.
           05  WS-ACCOUNT-STATUS       PIC X(1).
           05  WS-RESPONSE-CODE        PIC X(2).

       PROCEDURE DIVISION.
       0000-MAIN-CONTROL.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-INQUIRY
           PERFORM 3000-FINALIZE
           GOBACK.

       1000-INITIALIZE.
           OPEN INPUT ACCOUNT-FILE
           MOVE 123456789012 TO WS-INQUIRY-ACCT-NUMBER.

       2000-PROCESS-INQUIRY.
           MOVE WS-INQUIRY-ACCT-NUMBER TO ACCT-NUMBER
           READ ACCOUNT-FILE
           IF WS-ACCT-STATUS = '00'
               MOVE ACCT-CURRENT-BALANCE TO WS-CURRENT-BAL
               MOVE ACCT-AVAILABLE-BALANCE TO WS-AVAILABLE-BAL
               MOVE ACCT-CREDIT-LIMIT TO WS-CREDIT-LIMIT
               MOVE ACCT-STATUS TO WS-ACCOUNT-STATUS
               MOVE '00' TO WS-RESPONSE-CODE
           ELSE
               MOVE '01' TO WS-RESPONSE-CODE
           END-IF.

       3000-FINALIZE.
           CLOSE ACCOUNT-FILE.

      *================================================================
      * SYSTEM 2: TRANSACTION PROCESSING SYSTEM (TRANPROC)
      *================================================================

      *----------------------------------------------------------------
      * TRAN001 - TRANSACTION VALIDATION AND PROCESSING
      *----------------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRAN001.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO 'ACCTMAST'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ACCT-NUMBER
               FILE STATUS IS WS-ACCT-STATUS.
               
           SELECT TRANSACTION-FILE ASSIGN TO 'TRANLOG'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS TRAN-ID
               FILE STATUS IS WS-TRAN-STATUS.
               
           SELECT CC-FILE ASSIGN TO 'CCMAST'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CC-NUMBER
               FILE STATUS IS WS-CC-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE.
       COPY ACCTCOPY.
       
       FD  TRANSACTION-FILE.
       COPY TRANCOPY.
       
       FD  CC-FILE.
       COPY CCCOPY.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-ACCT-STATUS          PIC XX.
           05  WS-TRAN-STATUS          PIC XX.
           05  WS-CC-STATUS            PIC XX.

       01  WS-WORK-AREAS.
           05  WS-CURRENT-DATE         PIC 9(8).
           05  WS-CURRENT-TIME         PIC 9(6).
           05  WS-NEW-BALANCE          PIC S9(13)V99 COMP-3.
           05  WS-AUTH-CODE            PIC X(6).
           05  WS-TRANSACTION-ID       PIC 9(15).

       01  WS-INPUT-TRANSACTION.
           05  WS-IN-ACCT-NUMBER       PIC 9(12).
           05  WS-IN-CARD-NUMBER       PIC 9(16).
           05  WS-IN-TRAN-TYPE         PIC X(3).
           05  WS-IN-AMOUNT            PIC S9(13)V99 COMP-3.
           05  WS-IN-MERCHANT          PIC X(50).
           05  WS-IN-LOCATION          PIC X(30).

       01  WS-VALIDATION-FLAGS.
           05  WS-VALID-ACCOUNT        PIC X VALUE 'N'.
               88  ACCOUNT-VALID       VALUE 'Y'.
           05  WS-SUFFICIENT-FUNDS     PIC X VALUE 'N'.
               88  FUNDS-AVAILABLE     VALUE 'Y'.
           05  WS-WITHIN-LIMITS        PIC X VALUE 'N'.
               88  LIMITS-OK           VALUE 'Y'.
           05  WS-CARD-VALID           PIC X VALUE 'N'.
               88  CARD-OK             VALUE 'Y'.

       01  WS-RESPONSE-CODES.
           05  WS-RESPONSE-CODE        PIC X(2).
               88  APPROVED            VALUE '00'.
               88  DECLINED-NSF        VALUE '51'.
               88  DECLINED-LIMIT      VALUE '61'.
               88  DECLINED-CARD       VALUE '05'.
               88  DECLINED-ACCT       VALUE '14'.

       PROCEDURE DIVISION.
       0000-MAIN-CONTROL.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-TRANSACTION
           PERFORM 3000-FINALIZE
           GOBACK.

       1000-INITIALIZE.
           OPEN I-O ACCOUNT-FILE
           OPEN I-O TRANSACTION-FILE
           OPEN INPUT CC-FILE
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           ACCEPT WS-CURRENT-TIME FROM TIME
           
      *    Demo transaction data
           MOVE 123456789012 TO WS-IN-ACCT-NUMBER
           MOVE 4532123456789012 TO WS-IN-CARD-NUMBER
           MOVE 'PUR' TO WS-IN-TRAN-TYPE
           MOVE 125.50 TO WS-IN-AMOUNT
           MOVE 'AMAZON.COM' TO WS-IN-MERCHANT
           MOVE 'ONLINE' TO WS-IN-LOCATION.

       2000-PROCESS-TRANSACTION.
           PERFORM 2100-VALIDATE-TRANSACTION
           IF ACCOUNT-VALID AND FUNDS-AVAILABLE AND 
              LIMITS-OK AND CARD-OK
               PERFORM 2200-PROCESS-APPROVED-TRANSACTION
               MOVE '00' TO WS-RESPONSE-CODE
           ELSE
               PERFORM 2300-PROCESS-DECLINED-TRANSACTION
           END-IF
           PERFORM 2400-LOG-TRANSACTION.

       2100-VALIDATE-TRANSACTION.
      *    Validate account
           MOVE WS-IN-ACCT-NUMBER TO ACCT-NUMBER
           READ ACCOUNT-FILE
           IF WS-ACCT-STATUS = '00' AND ACCT-ACTIVE
               MOVE 'Y' TO WS-VALID-ACCOUNT
           END-IF
           
      *    Validate card if credit card transaction
           IF WS-IN-TRAN-TYPE = 'PUR'
               MOVE WS-IN-CARD-NUMBER TO CC-NUMBER
               READ CC-FILE
               IF WS-CC-STATUS = '00' AND CC-ACTIVE
                   MOVE 'Y' TO WS-CARD-VALID
               END-IF
           ELSE
               MOVE 'Y' TO WS-CARD-VALID
           END-IF
           
      *    Check funds/limits
           IF ACCOUNT-VALID
               IF CREDIT-CARD
                   IF WS-IN-AMOUNT <= ACCT-CREDIT-LIMIT
                       MOVE 'Y' TO WS-WITHIN-LIMITS
                       MOVE 'Y' TO WS-SUFFICIENT-FUNDS
                   END-IF
               ELSE
                   IF WS-IN-AMOUNT <= ACCT-AVAILABLE-BALANCE
                       MOVE 'Y' TO WS-SUFFICIENT-FUNDS
                       MOVE 'Y' TO WS-WITHIN-LIMITS
                   END-IF
               END-IF
           END-IF.

       2200-PROCESS-APPROVED-TRANSACTION.
           IF CREDIT-CARD
               COMPUTE WS-NEW-BALANCE = 
                   ACCT-CURRENT-BALANCE + WS-IN-AMOUNT
           ELSE
               COMPUTE WS-NEW-BALANCE = 
                   ACCT-CURRENT-BALANCE - WS-IN-AMOUNT
           END-IF
           
           MOVE WS-NEW-BALANCE TO ACCT-CURRENT-BALANCE
           MOVE WS-NEW-BALANCE TO ACCT-AVAILABLE-BALANCE
           REWRITE ACCOUNT-RECORD
           
      *    Generate authorization code
           COMPUTE WS-AUTH-CODE = 
               FUNCTION RANDOM * 900000 + 100000.

       2300-PROCESS-DECLINED-TRANSACTION.
           IF NOT ACCOUNT-VALID
               MOVE '14' TO WS-RESPONSE-CODE
           ELSE IF NOT CARD-OK
               MOVE '05' TO WS-RESPONSE-CODE
           ELSE IF NOT FUNDS-AVAILABLE
               MOVE '51' TO WS-RESPONSE-CODE
           ELSE IF NOT LIMITS-OK
               MOVE '61' TO WS-RESPONSE-CODE
           END-IF
           MOVE '000000' TO WS-AUTH-CODE.

       2400-LOG-TRANSACTION.
           COMPUTE WS-TRANSACTION-ID = 
               FUNCTION CURRENT-DATE(9:7) * 100000 +
               FUNCTION RANDOM * 99999
               
           INITIALIZE TRANSACTION-RECORD
           MOVE WS-TRANSACTION-ID TO TRAN-ID
           MOVE WS-IN-ACCT-NUMBER TO TRAN-ACCT-NUMBER
           MOVE WS-IN-TRAN-TYPE TO TRAN-TYPE
           MOVE WS-IN-AMOUNT TO TRAN-AMOUNT
           MOVE WS-CURRENT-DATE TO TRAN-DATE
           MOVE WS-CURRENT-TIME TO TRAN-TIME
           MOVE WS-IN-MERCHANT TO TRAN-DESCRIPTION
           MOVE WS-IN-LOCATION TO TRAN-LOCATION
           MOVE WS-AUTH-CODE TO TRAN-AUTH-CODE
           
           IF APPROVED
               MOVE 'C' TO TRAN-STATUS
           ELSE
               MOVE 'D' TO TRAN-STATUS
           END-IF
           
           WRITE TRANSACTION-RECORD.

       3000-FINALIZE.
           CLOSE ACCOUNT-FILE
           CLOSE TRANSACTION-FILE
           CLOSE CC-FILE.

      *================================================================
      * SYSTEM 3: BALANCE MANAGEMENT SYSTEM (BALMGMT)
      *================================================================

      *----------------------------------------------------------------
      * BAL001 - REAL-TIME BALANCE INQUIRY
      *----------------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BAL001.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO 'ACCTMAST'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ACCT-NUMBER
               FILE STATUS IS WS-ACCT-STATUS.
               
           SELECT HOLD-FILE ASSIGN TO 'HOLDFILE'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS HOLD-ACCT-NUMBER
               FILE STATUS IS WS-HOLD-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE.
       COPY ACCTCOPY.
       
       FD  HOLD-FILE.
       01  HOLD-RECORD.
           05  HOLD-ACCT-NUMBER        PIC 9(12).
           05  HOLD-AMOUNT             PIC S9(13)V99 COMP-3.
           05  HOLD-TYPE               PIC X(2).
               88  AUTH-HOLD           VALUE 'AH'.
               88  CHECK-HOLD          VALUE 'CH'.
               88  LEGAL-HOLD          VALUE 'LH'.
           05  HOLD-DATE               PIC 9(8).
           05  HOLD-EXPIRY             PIC 9(8).
           05  HOLD-STATUS             PIC X(1).
               88  HOLD-ACTIVE         VALUE 'A'.
               88  HOLD-RELEASED       VALUE 'R'.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-ACCT-STATUS          PIC XX.
           05  WS-HOLD-STATUS          PIC XX.

       01  WS-BALANCE-CALCULATION.
           05  WS-LEDGER-BALANCE       PIC S9(13)V99 COMP-3.
           05  WS-AVAILABLE-BALANCE    PIC S9(13)V99 COMP-3.
           05  WS-TOTAL-HOLDS          PIC S9(13)V99 COMP-3.
           05  WS-PENDING-CREDITS      PIC S9(13)V99 COMP-3.
           05  WS-PENDING-DEBITS       PIC S9(13)V99 COMP-3.

       01  WS-INPUT-DATA.
           05  WS-INQUIRY-ACCT         PIC 9(12).

       PROCEDURE DIVISION.
       0000-MAIN-CONTROL.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-CALCULATE-BALANCES
           PERFORM 3000-FINALIZE
           GOBACK.

       1000-INITIALIZE.
           OPEN INPUT ACCOUNT-FILE
           OPEN INPUT HOLD-FILE
           MOVE ZERO TO WS-TOTAL-HOLDS
           MOVE ZERO TO WS-PENDING-CREDITS
           MOVE ZERO TO WS-PENDING-DEBITS
           MOVE 123456789012 TO WS-INQUIRY-ACCT.

       2000-CALCULATE-BALANCES.
           PERFORM 2100-GET-ACCOUNT-BALANCE
           PERFORM 2200-CALCULATE-HOLDS
           PERFORM 2300-CALCULATE-AVAILABLE-BALANCE.

       2100-GET-ACCOUNT-BALANCE.
           MOVE WS-INQUIRY-ACCT TO ACCT-NUMBER
           READ ACCOUNT-FILE
           IF WS-ACCT-STATUS = '00'
               MOVE ACCT-CURRENT-BALANCE TO WS-LEDGER-BALANCE
           END-IF.

       2200-CALCULATE-HOLDS.
           MOVE WS-INQUIRY-ACCT TO HOLD-ACCT-NUMBER
           START HOLD-FILE KEY = HOLD-ACCT-NUMBER
           PERFORM UNTIL WS-HOLD-STATUS NOT = '00'
               READ HOLD-FILE NEXT RECORD
               IF WS-HOLD-STATUS = '00' AND 
                  HOLD-ACCT-NUMBER = WS-INQUIRY-ACCT AND
                  HOLD-ACTIVE
                   ADD HOLD-AMOUNT TO WS-TOTAL-HOLDS
               END-IF
           END-PERFORM.

       2300-CALCULATE-AVAILABLE-BALANCE.
           COMPUTE WS-AVAILABLE-BALANCE = 
               WS-LEDGER-BALANCE - WS-TOTAL-HOLDS +
               WS-PENDING-CREDITS - WS-PENDING-DEBITS.

       3000-FINALIZE.
           CLOSE ACCOUNT-FILE
           CLOSE HOLD-FILE.

      *================================================================
      * SYSTEM 4: REWARDS PROCESSING SYSTEM (REWARDS)
      *================================================================

      *----------------------------------------------------------------
      * REW001 - POINTS CALCULATION AND ACCRUAL
      *----------------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REW001.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REWARDS-FILE ASSIGN TO 'REWMAST'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS REW-ACCT-NUMBER
               FILE STATUS IS WS-REW-STATUS.
               
           SELECT POINTS-TRAN-FILE ASSIGN TO 'PTSTRAN'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-PTS-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  REWARDS-FILE.
       01  REWARDS-RECORD.
           05  REW-ACCT-NUMBER         PIC 9(12).
           05  REW-CUST-ID             PIC 9(10).
           05  REW-POINTS-BALANCE      PIC 9(9).
           05  REW-TIER-STATUS         PIC X(2).
               88  BASIC-TIER          VALUE 'BA'.
               88  SILVER-TIER         VALUE 'SI'.
               88  GOLD-TIER           VALUE 'GO'.
               88  PLATINUM-TIER       VALUE 'PL'.
           05  REW-YTD-SPENDING        PIC S9(13)V99 COMP-3.
           05  REW-LAST-ACTIVITY       PIC 9(8).
           05  REW-LIFETIME-POINTS     PIC 9(9).
           05  REW-POINTS-REDEEMED     PIC 9(9).
           05  REW-TIER-QUALIFYING-SP  PIC S9(13)V99 COMP-3.
           
       FD  POINTS-TRAN-FILE.
       01  POINTS-TRANSACTION.
           05  PTS-TRAN-ID             PIC 9(15).
           05  PTS-ACCT-NUMBER         PIC 9(12).
           05  PTS-TRAN-DATE           PIC 9(8).
           05  PTS-POINTS-EARNED       PIC 9(7).
           05  PTS-POINTS-REDEEMED     PIC 9(7).
           05  PTS-TRANSACTION-AMT     PIC S9(13)V99 COMP-3.
           05  PTS-MERCHANT-CATEGORY   PIC X(4).
               88  GROCERY             VALUE 'GROC'.
               88  GAS-STATION         VALUE 'FUEL'.
               88  RESTAURANT          VALUE 'REST'.
               88  TRAVEL              VALUE 'TRVL'.
               88  GENERAL             VALUE 'GENL'.
           05  PTS-MULTIPLIER          PIC 9V9 COMP-3.
           05  PTS-DESCRIPTION         PIC X(50).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-REW-STATUS           PIC XX.
           05  WS-PTS-STATUS           PIC XX.

       01  WS-POINTS-CALCULATION.
           05  WS-BASE-POINTS          PIC 9(7).
           05  WS-BONUS-POINTS         PIC 9(7).
           05  WS-TOTAL-POINTS         PIC 9(7).
           05  WS-POINTS-MULTIPLIER    PIC 9V9 COMP-3.

       01  WS-INPUT-TRANSACTION.
           05  WS-IN-ACCT-NUM          PIC 9(12).
           05  WS-IN-TRAN-AMT          PIC S9(13)V99 COMP-3.
           05  WS-IN-MERCHANT-CAT      PIC X(4).
           05  WS-IN-TRAN-DATE         PIC 9(8).

       01  WS-TIER-THRESHOLDS.
           05  WS-SILVER-THRESHOLD     PIC S9(9)V99 COMP-3 VALUE 5000.00.
           05  WS-GOLD-THRESHOLD       PIC S9(9)V99 COMP-3 VALUE 15000.00.
           05  WS-PLATINUM-THRESHOLD   PIC S9(9)V99 COMP-3 VALUE 50000.00.

       PROCEDURE DIVISION.
       0000-MAIN-CONTROL.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-POINTS-EARNING
           PERFORM 3000-UPDATE-TIER-STATUS
           PERFORM 4000-FINALIZE
           GOBACK.

       1000-INITIALIZE.
           OPEN I-O REWARDS-FILE
           OPEN OUTPUT POINTS-TRAN-FILE
           
      *    Demo transaction for points calculation
           MOVE 123456789012 TO WS-IN-ACCT-NUM
           MOVE 150.75 TO WS-IN-TRAN-AMT
           MOVE 'REST' TO WS-IN-MERCHANT-CAT
           ACCEPT WS-IN-TRAN-DATE FROM DATE YYYYMMDD.

       2000-PROCESS-POINTS-EARNING.
           PERFORM 2100-DETERMINE-POINTS-MULTIPLIER
           PERFORM 2200-CALCULATE-POINTS
           PERFORM 2300-UPDATE-REWARDS-BALANCE
           PERFORM 2400-LOG-POINTS-TRANSACTION.

       2100-DETERMINE-POINTS-MULTIPLIER.
      *    Get current rewards account
           MOVE WS-IN-ACCT-NUM TO REW-ACCT-NUMBER
           READ REWARDS-FILE
           
      *    Set multipliers based on category and tier
           EVALUATE WS-IN-MERCHANT-CAT
               WHEN 'GROC'
                   IF GOLD-TIER OR PLATINUM-TIER
                       MOVE 3.0 TO WS-POINTS-MULTIPLIER
                   ELSE
                       MOVE 2.0 TO WS-POINTS-MULTIPLIER
                   END-IF
               WHEN 'FUEL'
                   IF PLATINUM-TIER
                       MOVE 3.0 TO WS-POINTS-MULTIPLIER
                   ELSE
                       MOVE 2.0 TO WS-POINTS-MULTIPLIER
                   END-IF
               WHEN 'REST'
                   IF GOLD-TIER OR PLATINUM-TIER
                       MOVE 2.0 TO WS-POINTS-MULTIPLIER
                   ELSE
                       MOVE 1.5 TO WS-POINTS-MULTIPLIER
                   END-IF
               WHEN 'TRVL'
                   IF PLATINUM-TIER
                       MOVE 5.0 TO WS-POINTS-MULTIPLIER
                   ELSE IF GOLD-TIER
                       MOVE 3.0 TO WS-POINTS-MULTIPLIER
                   ELSE
                       MOVE 2.0 TO WS-POINTS-MULTIPLIER
                   END-IF
               WHEN OTHER
                   MOVE 1.0 TO WS-POINTS-MULTIPLIER
           END-EVALUATE.

       2200-CALCULATE-POINTS.
      *    Base calculation: 1 point per dollar
           COMPUTE WS-BASE-POINTS = WS-IN-TRAN-AMT
           
      *    Apply category multiplier
           COMPUTE WS-TOTAL-POINTS = 
               WS-BASE-POINTS * WS-POINTS-MULTIPLIER
           
      *    Tier bonus points
           EVALUATE TRUE
               WHEN PLATINUM-TIER
                   COMPUTE WS-BONUS-POINTS = WS-BASE-POINTS * 0.5
               WHEN GOLD-TIER
                   COMPUTE WS-BONUS-POINTS = WS-BASE-POINTS * 0.25
               WHEN OTHER
                   MOVE ZERO TO WS-BONUS-POINTS
           END-EVALUATE
           
           ADD WS-BONUS-POINTS TO WS-TOTAL-POINTS.

       2300-UPDATE-REWARDS-BALANCE.
           ADD WS-TOTAL-POINTS TO REW-POINTS-BALANCE
           ADD WS-TOTAL-POINTS TO REW-LIFETIME-POINTS
           ADD WS-IN-TRAN-AMT TO REW-YTD-SPENDING
           ADD WS-IN-TRAN-AMT TO REW-TIER-QUALIFYING-SP
           MOVE WS-IN-TRAN-DATE TO REW-LAST-ACTIVITY
           REWRITE REWARDS-RECORD.

       2400-LOG-POINTS-TRANSACTION.
           INITIALIZE POINTS-TRANSACTION
           COMPUTE PTS-TRAN-ID = 
               FUNCTION CURRENT-DATE(9:7) * 100000 +
               FUNCTION RANDOM * 99999
           MOVE WS-IN-ACCT-NUM TO PTS-ACCT-NUMBER
           MOVE WS-IN-TRAN-DATE TO PTS-TRAN-DATE
           MOVE WS-TOTAL-POINTS TO PTS-POINTS-EARNED
           MOVE WS-IN-TRAN-AMT TO PTS-TRANSACTION-AMT
           MOVE WS-IN-MERCHANT-CAT TO PTS-MERCHANT-CATEGORY
           MOVE WS-POINTS-MULTIPLIER TO PTS-MULTIPLIER
           STRING 'POINTS EARNED: ' WS-TOTAL-POINTS 
                  ' FOR  WS-IN-TRAN-AMT
                  DELIMITED BY SIZE INTO PTS-DESCRIPTION
           WRITE POINTS-TRANSACTION.

       3000-UPDATE-TIER-STATUS.
           EVALUATE TRUE
               WHEN REW-TIER-QUALIFYING-SP >= WS-PLATINUM-THRESHOLD
                   MOVE 'PL' TO REW-TIER-STATUS
               WHEN REW-TIER-QUALIFYING-SP >= WS-GOLD-THRESHOLD
                   MOVE 'GO' TO REW-TIER-STATUS
               WHEN REW-TIER-QUALIFYING-SP >= WS-SILVER-THRESHOLD
                   MOVE 'SI' TO REW-TIER-STATUS
               WHEN OTHER
                   MOVE 'BA' TO REW-TIER-STATUS
           END-EVALUATE
           REWRITE REWARDS-RECORD.

       4000-FINALIZE.
           CLOSE REWARDS-FILE
           CLOSE POINTS-TRAN-FILE.

      *----------------------------------------------------------------
      * REW002 - POINTS REDEMPTION PROGRAM
      *----------------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REW002.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REWARDS-FILE ASSIGN TO 'REWMAST'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS REW-ACCT-NUMBER
               FILE STATUS IS WS-REW-STATUS.
               
           SELECT REDEMPTION-CATALOG ASSIGN TO 'REDCAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CAT-ITEM-CODE
               FILE STATUS IS WS-CAT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  REWARDS-FILE.
       01  REWARDS-RECORD.
           05  REW-ACCT-NUMBER         PIC 9(12).
           05  REW-CUST-ID             PIC 9(10).
           05  REW-POINTS-BALANCE      PIC 9(9).
           05  REW-TIER-STATUS         PIC X(2).
           05  REW-YTD-SPENDING        PIC S9(13)V99 COMP-3.
           05  REW-LAST-ACTIVITY       PIC 9(8).
           05  REW-LIFETIME-POINTS     PIC 9(9).
           05  REW-POINTS-REDEEMED     PIC 9(9).
           05  REW-TIER-QUALIFYING-SP  PIC S9(13)V99 COMP-3.
           
       FD  REDEMPTION-CATALOG.
       01  CATALOG-RECORD.
           05  CAT-ITEM-CODE           PIC X(8).
           05  CAT-ITEM-NAME           PIC X(50).
           05  CAT-POINTS-REQUIRED     PIC 9(7).
           05  CAT-CASH-VALUE          PIC S9(7)V99 COMP-3.
           05  CAT-CATEGORY            PIC X(4).
               88  CASH-BACK           VALUE 'CASH'.
               88  TRAVEL-REWARD       VALUE 'TRVL'.
               88  MERCHANDISE         VALUE 'MERC'.
               88  GIFT-CARD           VALUE 'GIFT'.
           05  CAT-TIER-DISCOUNT       PIC 9V99 COMP-3.
           05  CAT-STATUS              PIC X(1).
               88  CAT-ACTIVE          VALUE 'A'.
               88  CAT-INACTIVE        VALUE 'I'.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-REW-STATUS           PIC XX.
           05  WS-CAT-STATUS           PIC XX.

       01  WS-REDEMPTION-REQUEST.
           05  WS-REQ-ACCT-NUMBER      PIC 9(12).
           05  WS-REQ-ITEM-CODE        PIC X(8).
           05  WS-REQ-QUANTITY         PIC 9(3).

       01  WS-REDEMPTION-CALCULATION.
           05  WS-POINTS-NEEDED        PIC 9(9).
           05  WS-DISCOUNTED-POINTS    PIC 9(9).
           05  WS-TIER-DISCOUNT-PCT    PIC 9V99 COMP-3.
           05  WS-AVAILABLE-POINTS     PIC 9(9).

       01  WS-RESPONSE-CODES.
           05  WS-REDEMPTION-RESULT    PIC X(2).
               88  REDEMPTION-SUCCESS  VALUE '00'.
               88  INSUFFICIENT-POINTS VALUE '01'.
               88  INVALID-ITEM        VALUE '02'.
               88  ITEM-UNAVAILABLE    VALUE '03'.

       PROCEDURE DIVISION.
       0000-MAIN-CONTROL.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-REDEMPTION
           PERFORM 3000-FINALIZE
           GOBACK.

       1000-INITIALIZE.
           OPEN I-O REWARDS-FILE
           OPEN INPUT REDEMPTION-CATALOG
           
      *    Demo redemption request
           MOVE 123456789012 TO WS-REQ-ACCT-NUMBER
           MOVE 'CASH0025' TO WS-REQ-ITEM-CODE
           MOVE 1 TO WS-REQ-QUANTITY.

       2000-PROCESS-REDEMPTION.
           PERFORM 2100-VALIDATE-REDEMPTION-REQUEST
           IF REDEMPTION-SUCCESS
               PERFORM 2200-PROCESS-REDEMPTION
               PERFORM 2300-UPDATE-REWARDS-ACCOUNT
           END-IF.

       2100-VALIDATE-REDEMPTION-REQUEST.
      *    Get rewards account
           MOVE WS-REQ-ACCT-NUMBER TO REW-ACCT-NUMBER
           READ REWARDS-FILE
           IF WS-REW-STATUS NOT = '00'
               MOVE '02' TO WS-REDEMPTION-RESULT
               GO TO 2100-EXIT
           END-IF
           
      *    Get catalog item
           MOVE WS-REQ-ITEM-CODE TO CAT-ITEM-CODE
           READ REDEMPTION-CATALOG
           IF WS-CAT-STATUS NOT = '00' OR NOT CAT-ACTIVE
               MOVE '02' TO WS-REDEMPTION-RESULT
               GO TO 2100-EXIT
           END-IF
           
      *    Calculate points needed with tier discount
           COMPUTE WS-POINTS-NEEDED = 
               CAT-POINTS-REQUIRED * WS-REQ-QUANTITY
               
           EVALUATE REW-TIER-STATUS
               WHEN 'PL'
                   MOVE 0.20 TO WS-TIER-DISCOUNT-PCT
               WHEN 'GO'
                   MOVE 0.15 TO WS-TIER-DISCOUNT-PCT
               WHEN 'SI'
                   MOVE 0.10 TO WS-TIER-DISCOUNT-PCT
               WHEN OTHER
                   MOVE 0.00 TO WS-TIER-DISCOUNT-PCT
           END-EVALUATE
           
           COMPUTE WS-DISCOUNTED-POINTS = 
               WS-POINTS-NEEDED * (1 - WS-TIER-DISCOUNT-PCT)
           
      *    Check if sufficient points available
           IF REW-POINTS-BALANCE >= WS-DISCOUNTED-POINTS
               MOVE '00' TO WS-REDEMPTION-RESULT
           ELSE
               MOVE '01' TO WS-REDEMPTION-RESULT
           END-IF.
           
       2100-EXIT.
           EXIT.

       2200-PROCESS-REDEMPTION.
      *    Process the redemption based on type
           EVALUATE TRUE
               WHEN CASH-BACK
                   PERFORM 2210-PROCESS-CASH-BACK
               WHEN TRAVEL-REWARD
                   PERFORM 2220-PROCESS-TRAVEL-REWARD
               WHEN MERCHANDISE OR GIFT-CARD
                   PERFORM 2230-PROCESS-MERCHANDISE
           END-EVALUATE.

       2210-PROCESS-CASH-BACK.
      *    For cash back, credit the account
      *    This would interface with the account management system
           CONTINUE.

       2220-PROCESS-TRAVEL-REWARD.
      *    For travel rewards, process through travel partner
           CONTINUE.

       2230-PROCESS-MERCHANDISE.
      *    For merchandise, create fulfillment order
           CONTINUE.

       2300-UPDATE-REWARDS-ACCOUNT.
           SUBTRACT WS-DISCOUNTED-POINTS FROM REW-POINTS-BALANCE
           ADD WS-DISCOUNTED-POINTS TO REW-POINTS-REDEEMED
           ACCEPT REW-LAST-ACTIVITY FROM DATE YYYYMMDD
           REWRITE REWARDS-RECORD.

       3000-FINALIZE.
           CLOSE REWARDS-FILE
           CLOSE REDEMPTION-CATALOG.

      *================================================================
      * ENHANCED CREDIT CARD PROCESSING PROGRAMS
      *================================================================

      *----------------------------------------------------------------
      * CC001 - CREDIT CARD AUTHORIZATION PROGRAM
      *----------------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CC001.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CC-FILE ASSIGN TO 'CCMAST'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CC-NUMBER
               FILE STATUS IS WS-CC-STATUS.
               
           SELECT CC-TRANSACTION-FILE ASSIGN TO 'CCTRAN'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CC-TRAN-ID
               FILE STATUS IS WS-CCTRAN-STATUS.
               
           SELECT FRAUD-RULES-FILE ASSIGN TO 'FRAUDRUL'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FRAUD-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CC-FILE.
       01  CREDIT-CARD-RECORD.
           05  CC-NUMBER               PIC 9(16).
           05  CC-ACCT-NUMBER          PIC 9(12).
           05  CC-CUST-ID              PIC 9(10).
           05  CC-TYPE                 PIC X(2).
           05  CC-STATUS               PIC X(1).
           05  CC-EXPIRY-DATE          PIC 9(4).
           05  CC-CVV                  PIC 9(3).
           05  CC-ISSUE-DATE           PIC 9(8).
           05  CC-CREDIT-LIMIT         PIC S9(9)V99 COMP-3.
           05  CC-AVAILABLE-CREDIT     PIC S9(9)V99 COMP-3.
           05  CC-CASH-ADVANCE-LIMIT   PIC S9(9)V99 COMP-3.
           05  CC-APR                  PIC S9(3)V99 COMP-3.
           05  CC-LAST-PAYMENT-DATE    PIC 9(8).
           05  CC-LAST-PAYMENT-AMT     PIC S9(9)V99 COMP-3.
           05  CC-MIN-PAYMENT-DUE      PIC S9(9)V99 COMP-3.
           05  CC-PAYMENT-DUE-DATE     PIC 9(8).
           05  CC-LAST-STMT-BALANCE    PIC S9(9)V99 COMP-3.
           
       FD  CC-TRANSACTION-FILE.
       01  CC-TRANSACTION-RECORD.
           05  CC-TRAN-ID              PIC 9(15).
           05  CC-TRAN-CARD-NUMBER     PIC 9(16).
           05  CC-TRAN-TYPE            PIC X(3).
               88  CC-PURCHASE         VALUE 'PUR'.
               88  CC-CASH-ADVANCE     VALUE 'CAS'.
               88  CC-PAYMENT          VALUE 'PAY'.
               88  CC-REFUND           VALUE 'REF'.
               88  CC-REVERSAL         VALUE 'REV'.
           05  CC-TRAN-AMOUNT          PIC S9(11)V99 COMP-3.
           05  CC-TRAN-DATE            PIC 9(8).
           05  CC-TRAN-TIME            PIC 9(6).
           05  CC-TRAN-MERCHANT-ID     PIC X(15).
           05  CC-TRAN-MERCHANT-NAME   PIC X(40).
           05  CC-TRAN-MERCHANT-CAT    PIC X(4).
           05  CC-TRAN-LOCATION        PIC X(30).
           05  CC-TRAN-COUNTRY-CODE    PIC X(3).
           05  CC-TRAN-CURRENCY        PIC X(3).
           05  CC-TRAN-AUTH-CODE       PIC X(6).
           05  CC-TRAN-STATUS          PIC X(1).
           05  CC-TRAN-RESPONSE-CODE   PIC X(2).
           05  CC-TRAN-CVV-RESULT      PIC X(1).
           05  CC-TRAN-AVS-RESULT      PIC X(1).
           
       FD  FRAUD-RULES-FILE.
       01  FRAUD-RULE-RECORD.
           05  FRAUD-RULE-ID           PIC 9(5).
           05  FRAUD-RULE-TYPE         PIC X(3).
               88  VELOCITY-CHECK      VALUE 'VEL'.
               88  LOCATION-CHECK      VALUE 'LOC'.
               88  AMOUNT-CHECK        VALUE 'AMT'.
               88  PATTERN-CHECK       VALUE 'PAT'.
           05  FRAUD-RULE-THRESHOLD    PIC S9(9)V99 COMP-3.
           05  FRAUD-RULE-TIMEFRAME    PIC 9(3).
           05  FRAUD-RULE-ACTION       PIC X(1).
               88  DECLINE-RULE        VALUE 'D'.
               88  REFER-RULE          VALUE 'R'.
               88  APPROVE-RULE        VALUE 'A'.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-CC-STATUS            PIC XX.
           05  WS-CCTRAN-STATUS        PIC XX.
           05  WS-FRAUD-STATUS         PIC XX.

       01  WS-AUTHORIZATION-REQUEST.
           05  WS-AUTH-CARD-NUMBER     PIC 9(16).
           05  WS-AUTH-EXPIRY          PIC 9(4).
           05  WS-AUTH-CVV             PIC 9(3).
           05  WS-AUTH-AMOUNT          PIC S9(11)V99 COMP-3.
           05  WS-AUTH-MERCHANT-ID     PIC X(15).
           05  WS-AUTH-MERCHANT-CAT    PIC X(4).
           05  WS-AUTH-LOCATION        PIC X(30).
           05  WS-AUTH-COUNTRY         PIC X(3).

       01  WS-FRAUD-CHECKS.
           05  WS-FRAUD-SCORE          PIC 9(3).
           05  WS-VELOCITY-COUNT       PIC 9(3).
           05  WS-DAILY-TOTAL          PIC S9(9)V99 COMP-3.
           05  WS-LOCATION-RISK        PIC X(1).
               88  HIGH-RISK-LOCATION  VALUE 'H'.
               88  MEDIUM-RISK-LOC     VALUE 'M'.
               88  LOW-RISK-LOCATION   VALUE 'L'.

       01  WS-AUTHORIZATION-RESPONSE.
           05  WS-AUTH-RESULT          PIC X(2).
               88  AUTH-APPROVED       VALUE '00'.
               88  AUTH-DECLINED       VALUE '05'.
               88  AUTH-REFER          VALUE '01'.
               88  AUTH-PICKUP         VALUE '04'.
           05  WS-AUTH-CODE            PIC X(6).
           05  WS-DECLINE-REASON       PIC X(30).

       01  WS-WORK-AREAS.
           05  WS-CURRENT-DATE         PIC 9(8).
           05  WS-CURRENT-TIME         PIC 9(6).
           05  WS-TRANSACTION-ID       PIC 9(15).

       PROCEDURE DIVISION.
       0000-MAIN-CONTROL.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-AUTHORIZATION
           PERFORM 3000-FINALIZE
           GOBACK.

       1000-INITIALIZE.
           OPEN INPUT CC-FILE
           OPEN OUTPUT CC-TRANSACTION-FILE
           OPEN INPUT FRAUD-RULES-FILE
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           ACCEPT WS-CURRENT-TIME FROM TIME
           
      *    Demo authorization request
           MOVE 4532123456789012 TO WS-AUTH-CARD-NUMBER
           MOVE 1227 TO WS-AUTH-EXPIRY
           MOVE 123 TO WS-AUTH-CVV
           MOVE 85.50 TO WS-AUTH-AMOUNT
           MOVE 'AMZ001' TO WS-AUTH-MERCHANT-ID
           MOVE 'GENL' TO WS-AUTH-MERCHANT-CAT
           MOVE 'SEATTLE WA' TO WS-AUTH-LOCATION
           MOVE 'USA' TO WS-AUTH-COUNTRY.

       2000-PROCESS-AUTHORIZATION.
           PERFORM 2100-VALIDATE-CARD-DATA
           IF AUTH-APPROVED OR AUTH-REFER
               PERFORM 2200-CHECK-CREDIT-LIMIT
           END-IF
           IF AUTH-APPROVED OR AUTH-REFER
               PERFORM 2300-FRAUD-SCREENING
           END-IF
           IF AUTH-APPROVED
               PERFORM 2400-PROCESS-APPROVED-AUTH
           ELSE
               PERFORM 2500-PROCESS-DECLINED-AUTH
           END-IF
           PERFORM 2600-LOG-AUTHORIZATION.

       2100-VALIDATE-CARD-DATA.
           MOVE WS-AUTH-CARD-NUMBER TO CC-NUMBER
           READ CC-FILE
           IF WS-CC-STATUS NOT = '00'
               MOVE '05' TO WS-AUTH-RESULT
               MOVE 'INVALID CARD NUMBER' TO WS-DECLINE-REASON
               GO TO 2100-EXIT
           END-IF
           
           IF CC-STATUS NOT = 'A'
               MOVE '04' TO WS-AUTH-RESULT
               MOVE 'CARD BLOCKED' TO WS-DECLINE-REASON
               GO TO 2100-EXIT
           END-IF
           
           IF WS-AUTH-EXPIRY NOT = CC-EXPIRY-DATE
               MOVE '05' TO WS-AUTH-RESULT
               MOVE 'EXPIRED CARD' TO WS-DECLINE-REASON
               GO TO 2100-EXIT
           END-IF
           
           IF WS-AUTH-CVV NOT = CC-CVV
               MOVE '05' TO WS-AUTH-RESULT
               MOVE 'CVV MISMATCH' TO WS-DECLINE-REASON
               GO TO 2100-EXIT
           END-IF
           
           MOVE '00' TO WS-AUTH-RESULT.
           
       2100-EXIT.
           EXIT.

       2200-CHECK-CREDIT-LIMIT.
           IF WS-AUTH-AMOUNT > CC-AVAILABLE-CREDIT
               MOVE '05' TO WS-AUTH-RESULT
               MOVE 'INSUFFICIENT CREDIT' TO WS-DECLINE-REASON
           END-IF.

       2300-FRAUD-SCREENING.
           MOVE ZERO TO WS-FRAUD-SCORE
           PERFORM 2310-CHECK-VELOCITY
           PERFORM 2320-CHECK-LOCATION-RISK
           PERFORM 2330-CHECK-AMOUNT-PATTERNS
           
           IF WS-FRAUD-SCORE > 75
               MOVE '05' TO WS-AUTH-RESULT
               MOVE 'FRAUD SUSPECTED' TO WS-DECLINE-REASON
           ELSE IF WS-FRAUD-SCORE > 50
               MOVE '01' TO WS-AUTH-RESULT
               MOVE 'REFER TO ISSUER' TO WS-DECLINE-REASON
           END-IF.

       2310-CHECK-VELOCITY.
      *    Check number of transactions in last hour
      *    This would read recent transactions for this card
           MOVE 15 TO WS-FRAUD-SCORE.

       2320-CHECK-LOCATION-RISK.
      *    Assess location risk based on country/merchant
           IF WS-AUTH-COUNTRY = 'USA'
               MOVE 'L' TO WS-LOCATION-RISK
           ELSE
               MOVE 'M' TO WS-LOCATION-RISK
               ADD 25 TO WS-FRAUD-SCORE
           END-IF.

       2330-CHECK-AMOUNT-PATTERNS.
      *    Check for unusual spending patterns
           IF WS-AUTH-AMOUNT > 500.00
               ADD 10 TO WS-FRAUD-SCORE
           END-IF.

       2400-PROCESS-APPROVED-AUTH.
           COMPUTE CC-AVAILABLE-CREDIT = 
               CC-AVAILABLE-CREDIT - WS-AUTH-AMOUNT
           REWRITE CREDIT-CARD-RECORD
           
      *    Generate authorization code
           COMPUTE WS-AUTH-CODE = 
               FUNCTION RANDOM * 900000 + 100000.

       2500-PROCESS-DECLINED-AUTH.
           MOVE '000000' TO WS-AUTH-CODE.

       2600-LOG-AUTHORIZATION.
           COMPUTE WS-TRANSACTION-ID = 
               FUNCTION CURRENT-DATE(9:7) * 100000 +
               FUNCTION RANDOM * 99999
               
           INITIALIZE CC-TRANSACTION-RECORD
           MOVE WS-TRANSACTION-ID TO CC-TRAN-ID
           MOVE WS-AUTH-CARD-NUMBER TO CC-TRAN-CARD-NUMBER
           MOVE 'PUR' TO CC-TRAN-TYPE
           MOVE WS-AUTH-AMOUNT TO CC-TRAN-AMOUNT
           MOVE WS-CURRENT-DATE TO CC-TRAN-DATE
           MOVE WS-CURRENT-TIME TO CC-TRAN-TIME
           MOVE WS-AUTH-MERCHANT-ID TO CC-TRAN-MERCHANT-ID
           MOVE WS-AUTH-MERCHANT-CAT TO CC-TRAN-MERCHANT-CAT
           MOVE WS-AUTH-LOCATION TO CC-TRAN-LOCATION
           MOVE WS-AUTH-COUNTRY TO CC-TRAN-COUNTRY-CODE
           MOVE 'USD' TO CC-TRAN-CURRENCY
           MOVE WS-AUTH-CODE TO CC-TRAN-AUTH-CODE
           MOVE WS-AUTH-RESULT TO CC-TRAN-RESPONSE-CODE
           
           IF AUTH-APPROVED
               MOVE 'C' TO CC-TRAN-STATUS
           ELSE
               MOVE 'D' TO CC-TRAN-STATUS
           END-IF
           
           WRITE CC-TRANSACTION-RECORD.

       3000-FINALIZE.
           CLOSE CC-FILE
           CLOSE CC-TRANSACTION-FILE
           CLOSE FRAUD-RULES-FILE.

      *================================================================
      * BATCH PROCESSING PROGRAMS
      *================================================================

      