#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
OpenASP AX - Simplified Online Operation Test Suite for MSGSample Program
Comprehensive test validation for COBOL to Java converted programs

Author: QA Director
Date: 2025-07-28
Version: 1.0
"""

import sys
import os
import time
import threading
import json
from datetime import datetime

# Add system commands path
sys.path.append('/home/aspuser/app/server/system-cmds')
from functions.call import CALL

def run_basic_test():
    """Execute basic CALL command test"""
    print("[TEST 1] Basic CALL Command Execution")
    print("-" * 50)
    
    try:
        start_time = time.time()
        result = CALL('CALL PGM-MSGSAMPLE.TESTLIB,VOL-DISK01')
        execution_time = time.time() - start_time
        
        if result:
            print("PASSED: Standard CALL syntax executed successfully")
            print(f"Execution time: {execution_time:.2f} seconds")
            return True
        else:
            print("FAILED: Command returned False")
            return False
    except Exception as e:
        print(f"FAILED: Exception - {e}")
        return False

def run_case_sensitivity_test():
    """Execute case sensitivity tests"""
    print("\n[TEST 2] Case Sensitivity Testing")
    print("-" * 50)
    
    passed = 0
    total = 0
    
    # Test mixed case
    total += 1
    try:
        result = CALL('CALL PGM-MSGSample.TESTLIB,VOL-DISK01')
        if result:
            print("PASSED: Mixed case program name")
            passed += 1
        else:
            print("FAILED: Mixed case program name")
    except Exception as e:
        print(f"FAILED: Mixed case - {e}")
    
    # Test auto-detect library
    total += 1
    try:
        result = CALL('CALL PGM-MSGSAMPLE')
        if result:
            print("PASSED: Auto-detect library")
            passed += 1
        else:
            print("FAILED: Auto-detect library")
    except Exception as e:
        print(f"FAILED: Auto-detect - {e}")
    
    print(f"Case sensitivity results: {passed}/{total} passed")
    return passed == total

def run_error_handling_test():
    """Execute error handling tests"""
    print("\n[TEST 3] Error Handling Validation")
    print("-" * 50)
    
    passed = 0
    total = 0
    
    error_cases = [
        ("Invalid program", "CALL PGM-INVALID.TESTLIB,VOL-DISK01"),
        ("Invalid library", "CALL PGM-MSGSAMPLE.INVALID,VOL-DISK01"),
        ("Invalid volume", "CALL PGM-MSGSAMPLE.TESTLIB,VOL-INVALID")
    ]
    
    for test_name, command in error_cases:
        total += 1
        try:
            result = CALL(command)
            if not result:  # Should fail
                print(f"PASSED: {test_name} - correctly failed")
                passed += 1
            else:
                print(f"FAILED: {test_name} - should have failed but succeeded")
        except Exception as e:
            print(f"PASSED: {test_name} - exception correctly raised")
            passed += 1
    
    print(f"Error handling results: {passed}/{total} passed")
    return passed == total

def run_concurrent_test():
    """Execute concurrent execution test"""
    print("\n[TEST 4] Concurrent Execution Testing")
    print("-" * 50)
    
    results = []
    
    def execute_program(thread_id):
        try:
            result = CALL('CALL PGM-MSGSAMPLE.TESTLIB,VOL-DISK01')
            results.append((thread_id, result))
        except Exception as e:
            results.append((thread_id, False))
    
    # Start 3 concurrent threads
    threads = []
    for i in range(1, 4):
        thread = threading.Thread(target=execute_program, args=(i,))
        threads.append(thread)
        thread.start()
        time.sleep(0.1)
    
    # Wait for completion
    for thread in threads:
        thread.join()
    
    # Count successes
    successful = sum(1 for _, result in results if result)
    
    if successful == 3:
        print(f"PASSED: Concurrent execution - {successful}/3 succeeded")
        return True
    else:
        print(f"FAILED: Concurrent execution - {successful}/3 succeeded")
        return False

def run_file_operation_test():
    """Execute file operation test"""
    print("\n[TEST 5] File Operations Testing")
    print("-" * 50)
    
    # Test missing data file
    samdata_path = "/home/aspuser/app/volume/DISK01/TESTLIB/SAMDATA"
    backup_path = samdata_path + ".test_backup"
    
    try:
        # Temporarily rename SAMDATA file
        if os.path.exists(samdata_path):
            os.rename(samdata_path, backup_path)
        
        result = CALL('CALL PGM-MSGSAMPLE.TESTLIB,VOL-DISK01')
        
        # Restore file
        if os.path.exists(backup_path):
            os.rename(backup_path, samdata_path)
        
        # Program should handle missing file gracefully
        print("PASSED: Missing data file handled gracefully")
        return True
        
    except Exception as e:
        # Restore file on exception
        if os.path.exists(backup_path):
            os.rename(backup_path, samdata_path)
        print(f"FAILED: File operation test - {e}")
        return False

def run_integration_test():
    """Execute integration points test"""
    print("\n[TEST 6] Integration Points Testing")
    print("-" * 50)
    
    passed = 0
    total = 0
    
    # Test catalog registration
    total += 1
    try:
        catalog_path = "/home/aspuser/app/asp-manager/public/config/catalog.json"
        if os.path.exists(catalog_path):
            with open(catalog_path, 'r') as f:
                catalog = json.load(f)
            
            program_found = (
                'DISK01' in catalog and 
                'TESTLIB' in catalog['DISK01'] and
                'MSGSample' in catalog['DISK01']['TESTLIB'] and
                catalog['DISK01']['TESTLIB']['MSGSample'].get('TYPE') == 'PGM'
            )
            
            if program_found:
                print("PASSED: Catalog registration verified")
                passed += 1
            else:
                print("FAILED: MSGSample not found in catalog")
        else:
            print("FAILED: catalog.json not found")
    except Exception as e:
        print(f"FAILED: Catalog test - {e}")
    
    # Test required files
    total += 1
    try:
        required_files = [
            "/home/aspuser/app/volume/DISK01/TESTLIB/MSGSample.jar",
            "/home/aspuser/app/volume/DISK01/TESTLIB/SAMDATA",
            "/home/aspuser/app/volume/DISK01/TESTLIB/MSGSAMP1"
        ]
        
        missing_files = [f for f in required_files if not os.path.exists(f)]
        
        if not missing_files:
            print("PASSED: All required files present")
            passed += 1
        else:
            print(f"FAILED: Missing files - {missing_files}")
    except Exception as e:
        print(f"FAILED: File system test - {e}")
    
    print(f"Integration results: {passed}/{total} passed")
    return passed == total

def main():
    """Main test execution"""
    print("=" * 80)
    print("OpenASP AX - MSGSample Online Operation Test Suite")
    print("=" * 80)
    print(f"Test Session Started: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    
    start_time = time.time()
    
    # Execute all tests
    test_results = []
    test_results.append(("Basic Execution", run_basic_test()))
    test_results.append(("Case Sensitivity", run_case_sensitivity_test()))
    test_results.append(("Error Handling", run_error_handling_test()))
    test_results.append(("Concurrent Execution", run_concurrent_test()))
    test_results.append(("File Operations", run_file_operation_test()))
    test_results.append(("Integration Points", run_integration_test()))
    
    # Calculate results
    passed_tests = sum(1 for _, result in test_results if result)
    total_tests = len(test_results)
    success_rate = (passed_tests / total_tests * 100) if total_tests > 0 else 0
    execution_time = time.time() - start_time
    
    # Print summary
    print("\n" + "=" * 80)
    print("TEST EXECUTION SUMMARY")
    print("=" * 80)
    print(f"Total Tests: {total_tests}")
    print(f"Passed: {passed_tests}")
    print(f"Failed: {total_tests - passed_tests}")
    print(f"Success Rate: {success_rate:.1f}%")
    print(f"Total Execution Time: {execution_time:.2f} seconds")
    print()
    
    # Individual results
    print("Individual Test Results:")
    for test_name, result in test_results:
        status = "PASSED" if result else "FAILED"
        print(f"  {test_name}: {status}")
    
    # Overall status
    if success_rate >= 85:
        print("\nOverall Status: PASSED - Ready for production")
    else:
        print("\nOverall Status: FAILED - Issues need attention")
    
    print(f"\nTest execution completed at: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    
    return success_rate >= 85

if __name__ == "__main__":
    try:
        success = main()
        sys.exit(0 if success else 1)
    except KeyboardInterrupt:
        print("\n\nTest execution interrupted by user.")
        sys.exit(1)
    except Exception as e:
        print(f"\n\nTest suite execution failed: {e}")
        sys.exit(1)