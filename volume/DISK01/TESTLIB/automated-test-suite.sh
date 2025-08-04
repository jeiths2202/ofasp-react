#!/bin/bash
# automated-test-suite.sh
# Automated test suite for MSGSample program validation
# Author: QA Director
# Version: 1.0
# Date: 2025-07-28

set -e  # Exit on any error

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Test configuration
TEST_DIR="/home/aspuser/app/volume/DISK01/TESTLIB"
SYSTEM_CMDS_DIR="/home/aspuser/app/server/system-cmds"
LOG_FILE="$TEST_DIR/test-execution.log"
RESULTS_FILE="$TEST_DIR/test-results-$(date +%Y%m%d-%H%M%S).json"

# Test counters
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0
WARNINGS=0

# Initialize test results JSON
echo '{
  "test_execution": {
    "start_time": "'$(date -Iseconds)'",
    "environment": "OpenASP AX Development",
    "program_under_test": "MSGSample",
    "test_results": []
  }
}' > "$RESULTS_FILE"

# Logging function
log() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1" | tee -a "$LOG_FILE"
}

# Print colored output
print_status() {
    local status=$1
    local message=$2
    case $status in
        "PASS")
            echo -e "${GREEN}✓ PASS${NC}: $message"
            ;;
        "FAIL")
            echo -e "${RED}✗ FAIL${NC}: $message"
            ;;
        "WARN")
            echo -e "${YELLOW}⚠ WARN${NC}: $message"
            ;;
        "INFO")
            echo -e "${BLUE}ℹ INFO${NC}: $message"
            ;;
    esac
}

# Add test result to JSON
add_test_result() {
    local test_id=$1
    local test_name=$2
    local status=$3
    local details=$4
    local duration=$5
    
    # Create temporary JSON for this test result
    cat << EOF > /tmp/test_result.json
{
  "test_id": "$test_id",
  "test_name": "$test_name",
  "status": "$status",
  "details": "$details",
  "duration_seconds": $duration,
  "timestamp": "$(date -Iseconds)"
}
EOF
    
    # Add to results file (simplified approach)
    log "Test $test_id: $status - $test_name"
}

# Execute CALL command and capture results
execute_call() {
    local call_command=$1
    local timeout_seconds=${2:-30}
    
    cd "$SYSTEM_CMDS_DIR"
    timeout "$timeout_seconds" python3 -c "
from functions.call import CALL
import sys
try:
    result = CALL('$call_command')
    sys.exit(0 if result else 1)
except Exception as e:
    print(f'Exception: {e}')
    sys.exit(2)
" 2>&1
}

# Test prerequisites
test_prerequisites() {
    print_status "INFO" "Testing prerequisites..."
    local test_start=$(date +%s)
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    # Check if OpenASP AX server is running
    if netstat -tlnp | grep -q ":3005"; then
        print_status "PASS" "OpenASP AX server is running on port 3005"
    else
        print_status "WARN" "OpenASP AX server may not be fully accessible"
        WARNINGS=$((WARNINGS + 1))
    fi
    
    # Check required files
    local required_files=(
        "$TEST_DIR/MSGSample.jar"
        "$TEST_DIR/SAMDATA"
        "$TEST_DIR/MSGSAMP1"
        "/home/aspuser/app/asp-manager/public/config/catalog.json"
    )
    
    local missing_files=0
    for file in "${required_files[@]}"; do
        if [[ -f "$file" ]]; then
            print_status "PASS" "Required file exists: $(basename $file)"
        else
            print_status "FAIL" "Missing required file: $file"
            missing_files=$((missing_files + 1))
        fi
    done
    
    local test_end=$(date +%s)
    local duration=$((test_end - test_start))
    
    if [[ $missing_files -eq 0 ]]; then
        PASSED_TESTS=$((PASSED_TESTS + 1))
        add_test_result "PRE-001" "Prerequisites Check" "PASS" "All required files present" "$duration"
        return 0
    else
        FAILED_TESTS=$((FAILED_TESTS + 1))
        add_test_result "PRE-001" "Prerequisites Check" "FAIL" "$missing_files files missing" "$duration"
        return 1
    fi
}

# Test basic CALL functionality
test_basic_call() {
    print_status "INFO" "Testing basic CALL functionality..."
    local test_start=$(date +%s)
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    local output
    if output=$(execute_call "CALL PGM-MSGSAMPLE.TESTLIB,VOL-DISK01" 15); then
        if echo "$output" | grep -q "PROCESSING COMPLETE"; then
            print_status "PASS" "Basic CALL execution successful"
            PASSED_TESTS=$((PASSED_TESTS + 1))
            
            local test_end=$(date +%s)
            local duration=$((test_end - test_start))
            add_test_result "CALL-001" "Basic CALL Execution" "PASS" "Program executed successfully" "$duration"
            return 0
        else
            print_status "FAIL" "Program executed but no completion message found"
            FAILED_TESTS=$((FAILED_TESTS + 1))
            
            local test_end=$(date +%s)
            local duration=$((test_end - test_start))
            add_test_result "CALL-001" "Basic CALL Execution" "FAIL" "No completion message" "$duration"
            return 1
        fi
    else
        print_status "FAIL" "CALL command execution failed"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        
        local test_end=$(date +%s)
        local duration=$((test_end - test_start))
        add_test_result "CALL-001" "Basic CALL Execution" "FAIL" "Command execution failed" "$duration"
        return 1
    fi
}

# Test case sensitivity
test_case_sensitivity() {
    print_status "INFO" "Testing case sensitivity..."
    local test_start=$(date +%s)
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    local commands=(
        "CALL PGM-MSGSAMPLE.TESTLIB,VOL-DISK01"
        "CALL PGM-msgsample.TESTLIB,VOL-DISK01"
        "CALL PGM-MSGSample.TESTLIB,VOL-DISK01"
    )
    
    local success_count=0
    for cmd in "${commands[@]}"; do
        if execute_call "$cmd" 10 >/dev/null 2>&1; then
            success_count=$((success_count + 1))
        fi
    done
    
    local test_end=$(date +%s)
    local duration=$((test_end - test_start))
    
    if [[ $success_count -eq ${#commands[@]} ]]; then
        print_status "PASS" "Case sensitivity handling works correctly"
        PASSED_TESTS=$((PASSED_TESTS + 1))
        add_test_result "CALL-002" "Case Sensitivity Test" "PASS" "All case variations work" "$duration"
        return 0
    else
        print_status "FAIL" "Case sensitivity issues detected ($success_count/${#commands[@]} succeeded)"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        add_test_result "CALL-002" "Case Sensitivity Test" "FAIL" "$success_count of ${#commands[@]} succeeded" "$duration"
        return 1
    fi
}

# Test error handling
test_error_handling() {
    print_status "INFO" "Testing error handling..."
    local test_start=$(date +%s)
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    local error_commands=(
        "CALL PGM-INVALID.TESTLIB,VOL-DISK01"
        "CALL PGM-MSGSAMPLE.INVALID,VOL-DISK01"
        "CALL PGM-MSGSAMPLE.TESTLIB,VOL-INVALID"
    )
    
    local proper_errors=0
    for cmd in "${error_commands[@]}"; do
        local output
        if ! output=$(execute_call "$cmd" 5 2>&1); then
            if echo "$output" | grep -q "ERROR"; then
                proper_errors=$((proper_errors + 1))
            fi
        fi
    done
    
    local test_end=$(date +%s)
    local duration=$((test_end - test_start))
    
    if [[ $proper_errors -eq ${#error_commands[@]} ]]; then
        print_status "PASS" "Error handling works correctly"
        PASSED_TESTS=$((PASSED_TESTS + 1))
        add_test_result "ERR-001" "Error Handling Test" "PASS" "All error cases handled properly" "$duration"
        return 0
    else
        print_status "FAIL" "Error handling issues detected ($proper_errors/${#error_commands[@]} proper errors)"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        add_test_result "ERR-001" "Error Handling Test" "FAIL" "$proper_errors of ${#error_commands[@]} handled properly" "$duration"
        return 1
    fi
}

# Test data processing accuracy
test_data_processing() {
    print_status "INFO" "Testing data processing accuracy..."
    local test_start=$(date +%s)
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    local output
    if output=$(execute_call "CALL PGM-MSGSAMPLE.TESTLIB,VOL-DISK01" 15); then
        # Count processed records
        local processed_count=$(echo "$output" | grep -c "PROCESSED")
        local expected_count=10
        
        # Check for summary message
        local has_summary=$(echo "$output" | grep -c "PROCESSING COMPLETE")
        
        local test_end=$(date +%s)
        local duration=$((test_end - test_start))
        
        if [[ $processed_count -eq $expected_count && $has_summary -eq 1 ]]; then
            print_status "PASS" "Data processing accurate ($processed_count records processed)"
            PASSED_TESTS=$((PASSED_TESTS + 1))
            add_test_result "DATA-001" "Data Processing Test" "PASS" "$processed_count records processed correctly" "$duration"
            return 0
        else
            print_status "FAIL" "Data processing issues (Expected: $expected_count, Got: $processed_count, Summary: $has_summary)"
            FAILED_TESTS=$((FAILED_TESTS + 1))
            add_test_result "DATA-001" "Data Processing Test" "FAIL" "Processing count mismatch" "$duration"
            return 1
        fi
    else
        local test_end=$(date +%s)
        local duration=$((test_end - test_start))
        print_status "FAIL" "Unable to execute program for data processing test"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        add_test_result "DATA-001" "Data Processing Test" "FAIL" "Program execution failed" "$duration"
        return 1
    fi
}

# Test performance benchmarks
test_performance() {
    print_status "INFO" "Testing performance benchmarks..."
    local test_start=$(date +%s)
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    local execution_times=()
    local success_count=0
    
    # Run 5 iterations for performance measurement
    for i in {1..5}; do
        local iter_start=$(date +%s.%N)
        if execute_call "CALL PGM-MSGSAMPLE.TESTLIB,VOL-DISK01" 20 >/dev/null 2>&1; then
            local iter_end=$(date +%s.%N)
            local iter_duration=$(echo "$iter_end - $iter_start" | bc)
            execution_times+=("$iter_duration")
            success_count=$((success_count + 1))
        fi
    done
    
    local test_end=$(date +%s)
    local duration=$((test_end - test_start))
    
    if [[ $success_count -eq 5 ]]; then
        # Calculate average execution time
        local total_time=0
        for time in "${execution_times[@]}"; do
            total_time=$(echo "$total_time + $time" | bc)
        done
        local avg_time=$(echo "scale=2; $total_time / 5" | bc)
        
        # Check if average time is within acceptable limits (< 10 seconds)
        if (( $(echo "$avg_time < 10" | awk) )); then
            print_status "PASS" "Performance within limits (Average: ${avg_time}s)"
            PASSED_TESTS=$((PASSED_TESTS + 1))
            add_test_result "PERF-001" "Performance Test" "PASS" "Average execution time: ${avg_time}s" "$duration"
            return 0
        else
            print_status "WARN" "Performance slower than expected (Average: ${avg_time}s)"
            WARNINGS=$((WARNINGS + 1))
            add_test_result "PERF-001" "Performance Test" "WARN" "Slow execution time: ${avg_time}s" "$duration"
            return 0
        fi
    else
        print_status "FAIL" "Performance test failed ($success_count/5 successful executions)"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        add_test_result "PERF-001" "Performance Test" "FAIL" "Inconsistent execution results" "$duration"
        return 1
    fi
}

# Test file integrity
test_file_integrity() {
    print_status "INFO" "Testing file integrity..."
    local test_start=$(date +%s)
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    # Check JAR file integrity
    if jar tf "$TEST_DIR/MSGSample.jar" >/dev/null 2>&1; then
        print_status "PASS" "JAR file integrity verified"
    else
        print_status "FAIL" "JAR file integrity check failed"
        local test_end=$(date +%s)
        local duration=$((test_end - test_start))
        FAILED_TESTS=$((FAILED_TESTS + 1))
        add_test_result "FILE-001" "File Integrity Test" "FAIL" "JAR file corrupted" "$duration"
        return 1
    fi
    
    # Check SAMDATA file format
    local line_count=$(wc -l < "$TEST_DIR/SAMDATA")
    local expected_lines=10
    
    if [[ $line_count -eq $expected_lines ]]; then
        print_status "PASS" "SAMDATA file has correct number of records ($line_count)"
    else
        print_status "FAIL" "SAMDATA file has incorrect record count (Expected: $expected_lines, Got: $line_count)"
        local test_end=$(date +%s)
        local duration=$((test_end - test_start))
        FAILED_TESTS=$((FAILED_TESTS + 1))
        add_test_result "FILE-001" "File Integrity Test" "FAIL" "SAMDATA record count mismatch" "$duration"
        return 1
    fi
    
    local test_end=$(date +%s)
    local duration=$((test_end - test_start))
    PASSED_TESTS=$((PASSED_TESTS + 1))
    add_test_result "FILE-001" "File Integrity Test" "PASS" "All files verified" "$duration"
    return 0
}

# Generate test report
generate_report() {
    local total_duration=$(($(date +%s) - START_TIME))
    
    echo
    echo "========================================"
    echo "          TEST EXECUTION SUMMARY        "
    echo "========================================"
    echo "Total Tests:    $TOTAL_TESTS"
    echo "Passed:         $PASSED_TESTS"
    echo "Failed:         $FAILED_TESTS"
    echo "Warnings:       $WARNINGS"
    echo "Success Rate:   $(( (PASSED_TESTS * 100) / TOTAL_TESTS ))%"
    echo "Total Duration: ${total_duration}s"
    echo "Timestamp:      $(date)"
    echo "========================================"
    
    # Update JSON results
    cat << EOF > /tmp/final_results.json
{
  "test_execution": {
    "start_time": "$(date -d @$START_TIME -Iseconds)",
    "end_time": "$(date -Iseconds)",
    "duration_seconds": $total_duration,
    "environment": "OpenASP AX Development",
    "program_under_test": "MSGSample",
    "summary": {
      "total_tests": $TOTAL_TESTS,
      "passed": $PASSED_TESTS,
      "failed": $FAILED_TESTS,
      "warnings": $WARNINGS,
      "success_rate": $(( (PASSED_TESTS * 100) / TOTAL_TESTS ))
    }
  }
}
EOF
    
    cp /tmp/final_results.json "$RESULTS_FILE"
    
    echo
    echo "Detailed results saved to: $RESULTS_FILE"
    echo "Execution log saved to: $LOG_FILE"
    
    if [[ $FAILED_TESTS -eq 0 ]]; then
        print_status "PASS" "All tests completed successfully!"
        return 0
    else
        print_status "FAIL" "$FAILED_TESTS test(s) failed. Review results for details."
        return 1
    fi
}

# Main execution
main() {
    START_TIME=$(date +%s)
    
    echo "========================================"
    echo "     MSGSample Automated Test Suite     "
    echo "========================================"
    echo "Start Time: $(date)"
    echo "Log File: $LOG_FILE"
    echo "Results File: $RESULTS_FILE"
    echo
    
    # Initialize log file
    log "Starting automated test suite execution"
    
    # Execute test suites
    test_prerequisites || true
    test_basic_call || true
    test_case_sensitivity || true
    test_error_handling || true
    test_data_processing || true
    test_performance || true
    test_file_integrity || true
    
    # Generate final report
    generate_report
}

# Cleanup function
cleanup() {
    log "Test suite execution completed"
    # Clean up temporary files
    rm -f /tmp/test_result.json /tmp/final_results.json
}

# Set trap for cleanup
trap cleanup EXIT

# Check if bc is available for calculations
if ! command -v bc &> /dev/null; then
    echo "Warning: 'bc' command not found. Installing..."
    apt-get update && sudo apt-get install -y bc || {
        echo "Error: Could not install 'bc'. Some calculations may fail."
        exit 1
    }
fi

# Run main function
main "$@"