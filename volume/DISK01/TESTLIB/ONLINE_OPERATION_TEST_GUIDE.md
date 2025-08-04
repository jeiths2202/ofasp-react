# Online Operation Test Guide for OpenASP AX

## Overview
This document provides comprehensive test scenarios for validating converted COBOL to Java programs in OpenASP AX environment, with specific focus on the MSGSample program through the ASP System Command Terminal.

## Test Environment Setup

### Prerequisites
- OpenASP AX environment running at http://localhost:3005
- MSGSample.jar and supporting files in /home/aspuser/app/volume/DISK01/TESTLIB
- SAMDATA file with 10 employee records
- MSGSAMP1 SMED map definition
- catalog.json registration for MSGSample program

### Test Data Verification
```
SAMDATA file contains:
- 10 employee records in FB format (RECLEN=80)
- Fields: Employee ID, Name, Department, Salary, Hire Date
- UTF-8 encoding
```

## Test Categories

### 1. Program Registration and Catalog Validation

#### Test Case 1.1: Catalog Entry Verification
**Objective**: Verify MSGSample program is properly registered in catalog.json

**Test Steps**:
1. Check catalog.json contains MSGSample entry with correct attributes
2. Verify PGMTYPE is "JAVA"
3. Confirm JARFILE points to "MSGSample.jar"
4. Validate DESCRIPTION and VERSION fields

**Expected Results**:
- Program entry exists with TYPE="PGM"
- PGMNAME="MSGSample" 
- JARFILE="MSGSample.jar"
- All required metadata fields populated

#### Test Case 1.2: SMED Map Registration
**Objective**: Verify MSGSAMP1 map is properly registered

**Test Steps**:
1. Confirm MSGSAMP1 entry in catalog.json
2. Verify MAPTYPE is "SMED"
3. Check ROWS=24, COLS=80
4. Validate field definitions exist

**Expected Results**:
- Map entry exists with correct dimensions
- Field definitions match program requirements
- Display format properly configured

### 2. Basic CALL Command Execution

#### Test Case 2.1: Standard CALL Syntax
**Objective**: Execute MSGSample using standard CALL command

**Test Command**:
```
CALL PGM-MSGSAMPLE.TESTLIB,VOL-DISK01
```

**Test Steps**:
1. Access ASP System Command Terminal at http://localhost:3005
2. Enter CALL command with exact syntax
3. Monitor execution progress
4. Verify program completion

**Expected Results**:
- Command accepted without syntax errors
- Program loads successfully
- Execution begins immediately
- No error messages during startup

#### Test Case 2.2: Alternative Program Name Case
**Objective**: Test case-insensitive program name handling

**Test Command**:
```
CALL PGM-MSGSample.TESTLIB,VOL-DISK01
```

**Test Steps**:
1. Execute with mixed-case program name
2. Verify same behavior as uppercase version
3. Check error handling consistency

**Expected Results**:
- Program executes successfully regardless of case
- Same output and behavior as standard syntax
- No case-sensitivity errors

### 3. File I/O Operations Validation

#### Test Case 3.1: SAMDATA File Access
**Objective**: Verify program correctly reads SAMDATA file

**Test Steps**:
1. Execute CALL command
2. Monitor file access operations
3. Verify all 10 records are processed
4. Check for file read errors

**Expected Results**:
- SAMDATA file opens successfully
- All 10 employee records read in sequence
- No file access errors
- Proper file closure after reading

#### Test Case 3.2: Record Processing Validation
**Objective**: Verify correct parsing of employee data

**Test Steps**:
1. Run MSGSample program
2. Examine output for each employee record
3. Verify field parsing accuracy
4. Check data formatting

**Expected Results**:
- Employee ID: 6-digit numeric display
- Name: 20-character left-aligned text
- Department: 10-character department code
- Salary: 8-digit numeric with proper formatting
- Hire Date: 8-character date (YYYYMMDD format)

### 4. SMED Map Display Validation

#### Test Case 4.1: Screen Layout Verification
**Objective**: Verify SMED map displays correctly in terminal

**Test Steps**:
1. Execute MSGSample program
2. Examine terminal display output
3. Verify screen dimensions (24x80)
4. Check field positioning

**Expected Results**:
- Header displays: "EMPLOYEE DATA DISPLAY" at position (1,25)
- Column headers properly aligned at position (3,1)
- Separator line displays at position (4,1)
- Employee data starts at position (5,1)

#### Test Case 4.2: Field Attribute Validation
**Objective**: Verify field attributes are applied correctly

**Test Steps**:
1. Check protected field attributes
2. Verify color assignments (BLUE, GREEN, WHITE)
3. Validate highlight settings
4. Test field boundaries

**Expected Results**:
- HEADER field: Protected, Blue color
- EMPDATA field: Unprotected, Green color
- STATUS field: Protected, White color
- Field lengths match SMED definition

### 5. Program Flow and Logic Testing

#### Test Case 5.1: Sequential Record Processing
**Objective**: Verify records are processed in correct sequence

**Test Steps**:
1. Monitor record processing order
2. Compare with SAMDATA file sequence
3. Verify no records are skipped
4. Check for duplicate processing

**Expected Results**:
- Records processed in file order (100001 through 100010)
- Each record appears exactly once
- No gaps in sequence
- Complete data display for all records

#### Test Case 5.2: Summary Message Display
**Objective**: Verify completion message displays correctly

**Test Steps**:
1. Wait for program completion
2. Check final status message
3. Verify record count accuracy
4. Confirm proper termination

**Expected Results**:
- Summary message displays total count: "10 records processed"
- Message appears in STATUS field area
- Program terminates cleanly
- Return code indicates success

### 6. Error Handling and Edge Cases

#### Test Case 6.1: Missing SAMDATA File
**Objective**: Test behavior when data file is unavailable

**Test Steps**:
1. Temporarily rename SAMDATA file
2. Execute CALL command
3. Monitor error handling
4. Restore file for subsequent tests

**Expected Results**:
- Clear error message about missing file
- Graceful program termination
- No system crashes or hangs
- Appropriate error code returned

#### Test Case 6.2: Corrupted Data File
**Objective**: Test handling of invalid data format

**Test Steps**:
1. Create test file with invalid record format
2. Temporarily replace SAMDATA
3. Execute program
4. Monitor error handling behavior

**Expected Results**:
- Data validation errors detected
- Clear error messages displayed
- Program continues or terminates gracefully
- No data corruption or system errors

### 7. Performance and Resource Testing

#### Test Case 7.1: Execution Time Measurement
**Objective**: Measure program execution performance

**Test Steps**:
1. Record start time before CALL command
2. Monitor execution progress
3. Record completion time
4. Calculate total execution duration

**Expected Results**:
- Program loads within 2 seconds
- File processing completes within 5 seconds
- Total execution time under 10 seconds
- Consistent performance across multiple runs

#### Test Case 7.2: Memory Usage Validation
**Objective**: Verify proper memory management

**Test Steps**:
1. Monitor system memory before execution
2. Track memory usage during execution
3. Verify memory cleanup after completion
4. Check for memory leaks

**Expected Results**:
- No excessive memory consumption
- Proper memory cleanup after completion
- No memory leaks detected
- Stable memory usage pattern

### 8. Integration Testing

#### Test Case 8.1: Concurrent Execution
**Objective**: Test multiple simultaneous program executions

**Test Steps**:
1. Open multiple terminal sessions
2. Execute CALL command simultaneously
3. Monitor each execution independently
4. Verify no resource conflicts

**Expected Results**:
- Each instance executes independently
- No file locking conflicts
- Proper resource isolation
- All instances complete successfully

#### Test Case 8.2: Terminal Session Management
**Objective**: Verify proper session handling

**Test Steps**:
1. Execute program in multiple browser tabs
2. Test session isolation
3. Verify independent state management
4. Check cleanup on session termination

**Expected Results**:
- Each session maintains independent state
- No cross-session interference
- Proper cleanup on tab closure
- Session data properly isolated

## Test Execution Checklist

### Pre-Execution Verification
- [ ] OpenASP AX server running on port 3005
- [ ] MSGSample.jar exists in TESTLIB directory
- [ ] SAMDATA file contains 10 valid records
- [ ] MSGSAMP1 map definition is present
- [ ] catalog.json entries are correctly configured

### During Execution Monitoring
- [ ] Command syntax accepted without errors
- [ ] Program loading progress visible
- [ ] File operations complete successfully
- [ ] Display formatting appears correctly
- [ ] All records processed in sequence

### Post-Execution Validation
- [ ] Summary message displays correct count
- [ ] Program terminates cleanly
- [ ] No error messages in logs
- [ ] Resources properly released
- [ ] Terminal ready for next command

## Expected Test Outputs

### Successful Execution Display
```
                        EMPLOYEE DATA DISPLAY
  
ID      NAME                 DEPT       SALARY   HIRE DATE STATUS
--------------------------------------------------------------------------------
100001  JOHN SMITH           SALES      00035000 20220115  ACTIVE
100002  MARY JOHNSON         MARKETING  00042000 20210308  ACTIVE
100003  ROBERT BROWN         IT         00055000 20200612  ACTIVE
100004  SUSAN DAVIS          HR         00038000 20220820  ACTIVE
100005  MICHAEL WILSON       FINANCE    00048000 20190423  ACTIVE
100006  JENNIFER GARCIA      SALES      00033000 20231201  ACTIVE
100007  DAVID MARTINEZ       IT         00062000 20180905  ACTIVE
100008  LISA ANDERSON        MARKETING  00045000 20210710  ACTIVE
100009  JAMES TAYLOR         FINANCE    00051000 20200228  ACTIVE
100010  PATRICIA THOMAS      HR         00040000 20220415  ACTIVE

10 records processed successfully.
```

### Error Scenarios
- **File Not Found**: "Error: SAMDATA file not accessible"
- **Permission Denied**: "Error: Insufficient file access permissions"
- **Invalid Format**: "Error: Record format validation failed"
- **System Error**: "Error: System resource unavailable"

## Troubleshooting Guide

### Common Issues and Solutions

#### Program Not Found Error
**Symptoms**: "Program MSGSAMPLE not found"
**Solutions**:
1. Verify catalog.json entry exists
2. Check MSGSample.jar file presence
3. Validate file permissions
4. Restart ASP server if needed

#### File Access Errors
**Symptoms**: "Unable to access SAMDATA"
**Solutions**:
1. Check file existence in TESTLIB directory
2. Verify file permissions (read access required)
3. Ensure file is not locked by another process
4. Validate file format and encoding

#### Display Format Issues
**Symptoms**: Misaligned or corrupted display
**Solutions**:
1. Verify MSGSAMP1 map definition
2. Check terminal size settings (24x80)
3. Validate SMED field positions
4. Clear browser cache and reload

#### Performance Problems
**Symptoms**: Slow execution or timeouts
**Solutions**:
1. Check system resource availability
2. Verify no competing processes
3. Monitor disk I/O performance
4. Consider increasing timeout values

### Diagnostic Commands
```bash
# Check file permissions
ls -la /home/aspuser/app/volume/DISK01/TESTLIB/SAMDATA

# Verify JAR file integrity
jar tf MSGSample.jar

# Monitor system resources
top -p `pgrep java`

# Check ASP server logs
tail -f /home/aspuser/app/asp-manager/server/server.log
```

## Test Documentation Requirements

### Test Result Recording
For each test case, document:
- Test execution date/time
- Test environment details
- Actual vs expected results
- Any deviations or issues
- Screenshots of key outputs
- Performance metrics

### Issue Reporting
Include in issue reports:
- Test case reference number
- Detailed steps to reproduce
- Error messages or screenshots
- System environment details
- Suggested resolution steps

### Sign-off Criteria
Tests are considered successful when:
- All mandatory test cases pass
- No critical or high-priority issues remain
- Performance meets specified requirements
- Documentation is complete and accurate
- Stakeholder approval obtained

---

**Document Version**: 1.0  
**Last Updated**: 2025-07-28  
**Prepared By**: QA Director  
**Review Status**: Pending Stakeholder Approval