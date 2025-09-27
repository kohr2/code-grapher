#!/usr/bin/env python3
"""
Generate a COBOL file with exactly:
- 1 Program
- 4 Divisions
- 16 Sections
- 261 Paragraphs
- 316 Statements
"""

def generate_exact_cobol():
    lines = []
    line_num = 100
    
    # IDENTIFICATION DIVISION
    lines.append(f"{line_num:06d} IDENTIFICATION DIVISION.")
    line_num += 100
    lines.append(f"{line_num:06d} PROGRAM-ID. EXACT-PROGRAM.")
    line_num += 100
    lines.append(f"{line_num:06d} AUTHOR. DEVELOPER.")
    line_num += 100
    lines.append(f"{line_num:06d} DATE-WRITTEN. 2024-01-01.")
    line_num += 100
    lines.append("")
    
    # ENVIRONMENT DIVISION
    lines.append(f"{line_num:06d} ENVIRONMENT DIVISION.")
    line_num += 100
    lines.append(f"{line_num:06d} CONFIGURATION SECTION.")
    line_num += 100
    lines.append(f"{line_num:06d} SOURCE-COMPUTER. IBM-370.")
    line_num += 100
    lines.append(f"{line_num:06d} OBJECT-COMPUTER. IBM-370.")
    line_num += 100
    lines.append(f"{line_num:06d} INPUT-OUTPUT SECTION.")
    line_num += 100
    lines.append(f"{line_num:06d} FILE-CONTROL.")
    line_num += 100
    lines.append(f"{line_num:06d}     SELECT INPUT-FILE ASSIGN TO INPUT.")
    line_num += 100
    lines.append(f"{line_num:06d}     SELECT OUTPUT-FILE ASSIGN TO OUTPUT.")
    line_num += 100
    lines.append("")
    
    # DATA DIVISION
    lines.append(f"{line_num:06d} DATA DIVISION.")
    line_num += 100
    lines.append(f"{line_num:06d} FILE SECTION.")
    line_num += 100
    lines.append(f"{line_num:06d} FD INPUT-FILE.")
    line_num += 100
    lines.append(f"{line_num:06d} 01 INPUT-RECORD.")
    line_num += 100
    lines.append(f"{line_num:06d}    05 CUSTOMER-ID PIC X(10).")
    line_num += 100
    lines.append(f"{line_num:06d}    05 CUSTOMER-NAME PIC X(30).")
    line_num += 100
    lines.append(f"{line_num:06d}    05 BALANCE PIC 9(7)V99.")
    line_num += 100
    lines.append("")
    lines.append(f"{line_num:06d} FD OUTPUT-FILE.")
    line_num += 100
    lines.append(f"{line_num:06d} 01 OUTPUT-RECORD.")
    line_num += 100
    lines.append(f"{line_num:06d}    05 CUSTOMER-ID PIC X(10).")
    line_num += 100
    lines.append(f"{line_num:06d}    05 CUSTOMER-NAME PIC X(30).")
    line_num += 100
    lines.append(f"{line_num:06d}    05 BALANCE PIC 9(7)V99.")
    line_num += 100
    lines.append("")
    lines.append(f"{line_num:06d} WORKING-STORAGE SECTION.")
    line_num += 100
    lines.append(f"{line_num:06d} 01 WS-COUNTERS.")
    line_num += 100
    lines.append(f"{line_num:06d}    05 WS-RECORD-COUNT PIC 9(5) VALUE ZERO.")
    line_num += 100
    lines.append(f"{line_num:06d}    05 WS-TOTAL-BALANCE PIC 9(9)V99 VALUE ZERO.")
    line_num += 100
    lines.append(f"{line_num:06d}    05 WS-AVERAGE-BALANCE PIC 9(7)V99 VALUE ZERO.")
    line_num += 100
    lines.append("")
    lines.append(f"{line_num:06d} 01 WS-FLAGS.")
    line_num += 100
    lines.append(f"{line_num:06d}    05 WS-EOF-FLAG PIC X(1) VALUE 'N'.")
    line_num += 100
    lines.append(f"{line_num:06d}    05 WS-ERROR-FLAG PIC X(1) VALUE 'N'.")
    line_num += 100
    lines.append("")
    
    # PROCEDURE DIVISION with exactly 16 sections
    lines.append(f"{line_num:06d} PROCEDURE DIVISION.")
    line_num += 100
    
    # Generate exactly 16 sections
    sections = [
        "1000-MAIN-PROCESSING", "2000-FILE-OPERATIONS", "3000-DATA-PROCESSING", 
        "4000-VALIDATION", "5000-REPORTING", "6000-UTILITY", "7000-DISPLAY-RESULTS",
        "8000-CLOSE-FILES", "9000-ERROR-HANDLER", "10000-SYSTEM-OPERATIONS",
        "11000-PERFORMANCE", "12000-MAINTENANCE", "13000-SECURITY", "14000-BACKUP",
        "15000-RECOVERY", "16000-SHUTDOWN"
    ]
    
    paragraph_count = 0
    statement_count = 0
    
    for i, section_name in enumerate(sections):
        # Section header
        lines.append(f"{line_num:06d} {section_name} SECTION.")
        line_num += 100
        
        # Calculate paragraphs per section to get exactly 261
        if i < 5:
            paragraphs_per_section = 16
        elif i < 10:
            paragraphs_per_section = 17
        else:
            paragraphs_per_section = 16
        
        for j in range(paragraphs_per_section):
            if paragraph_count >= 261:
                break
                
            paragraph_name = f"{1000 + i * 100 + j * 10:04d}-PARAGRAPH-{j+1:03d}"
            lines.append(f"{line_num:06d} {paragraph_name}.")
            line_num += 100
            
            # Add exactly 1 statement per paragraph to get 316 statements
            if statement_count < 316:
                lines.append(f"{line_num:06d}     DISPLAY 'PROCESSING PARAGRAPH {paragraph_name}'.")
                line_num += 100
                statement_count += 1
            
            paragraph_count += 1
            
            if paragraph_count >= 261:
                break
        
        if paragraph_count >= 261:
            break
    
    # Add remaining statements if needed (should be exactly 55 more)
    remaining_statements = 316 - statement_count
    for i in range(remaining_statements):
        lines.append(f"{line_num:06d}     DISPLAY 'ADDITIONAL STATEMENT {statement_count + i + 1}'.")
        line_num += 100
    
    # Final STOP RUN
    lines.append(f"{line_num:06d} STOP RUN.")
    
    return lines

if __name__ == "__main__":
    lines = generate_exact_cobol()
    
    with open("exact_cobol.cob", "w") as f:
        for line in lines:
            f.write(line + "\n")
    
    print(f"Generated COBOL file with {len(lines)} lines")
    print("Target structure:")
    print("- 1 Program")
    print("- 4 Divisions") 
    print("- 16 Sections")
    print("- 261 Paragraphs")
    print("- 316 Statements")