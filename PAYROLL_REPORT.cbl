       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO 'EMPLOYEE.DAT'
               ORGANIZATION IS SEQUENTIAL.
           SELECT PAYROLL-REPORT ASSIGN TO 'PAYROLL-REPORT.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEE-FILE.
       01 EMPLOYEE-RECORD.
           05 EMP-ID             PIC X(10).
           05 EMP-NAME           PIC X(30).
           05 GROSS-SALARY       PIC 9(8)V99.
           05 TAX-DEDUCTION      PIC 9(7)V99.
           05 BENEFITS           PIC 9(7)V99.
           05 NET-SALARY         PIC 9(8)V99.

       FD PAYROLL-REPORT.
       01 REPORT-RECORD.
           05 R-EMP-ID           PIC X(10).
           05 R-EMP-NAME         PIC X(30).
           05 R-GROSS-SALARY     PIC 9(8)V99.
           05 R-TAX-DEDUCTION    PIC 9(7)V99.
           05 R-BENEFITS         PIC 9(7)V99.
           05 R-NET-SALARY       PIC 9(8)V99.

       WORKING-STORAGE SECTION.
       01 WS-END-OF-FILE         PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT EMPLOYEE-FILE OUTPUT PAYROLL-REPORT.
           PERFORM PROCESS-EMPLOYEES UNTIL WS-END-OF-FILE = 'Y'.
           CLOSE EMPLOYEE-FILE PAYROLL-REPORT.
           STOP RUN.

       PROCESS-EMPLOYEES.
           READ EMPLOYEE-FILE AT END MOVE 'Y' TO WS-END-OF-FILE
           NOT AT END
               COMPUTE NET-SALARY = GROSS-SALARY - TAX-DEDUCTION + BENEFITS
               MOVE EMP-ID TO R-EMP-ID
               MOVE EMP-NAME TO R-EMP-NAME
               MOVE GROSS-SALARY TO R-GROSS-SALARY
               MOVE TAX-DEDUCTION TO R-TAX-DEDUCTION
               MOVE BENEFITS TO R-BENEFITS
               MOVE NET-SALARY TO R-NET-SALARY
               WRITE REPORT-RECORD
               DISPLAY 'Employee ID: ' EMP-ID
               DISPLAY 'Name       : ' EMP-NAME
               DISPLAY 'Gross Salary: ' GROSS-SALARY
               DISPLAY 'Tax Deduction: ' TAX-DEDUCTION
               DISPLAY 'Benefits    : ' BENEFITS
               DISPLAY 'Net Salary  : ' NET-SALARY
               DISPLAY '---------------------------'.
