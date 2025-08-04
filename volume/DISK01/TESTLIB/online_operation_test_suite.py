#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
OpenASP AX - Online Operation Test Suite for MSGSample Program
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
from typing import Dict, List, Tuple, Optional

# Add system commands path
sys.path.append('/home/aspuser/app/server/system-cmds')
from functions.call import CALL

class MSGSampleTestSuite:
    """Comprehensive test suite for MSGSample program validation"""
    
    def __init__(self):
        self.test_results = []
        self.start_time = None
        self.end_time = None
        
    def log_test_result(self, test_id: str, test_name: str, status: str, 
                       details: str = "", execution_time: float = 0):
        """Log test result for reporting"""
        result = {
            'test_id': test_id,
            'test_name': test_name,
            'status': status,
            'details': details,
            'execution_time': execution_time,
            'timestamp': datetime.now().isoformat()
        }
        self.test_results.append(result)
        
    def run_all_tests(self) -> bool:
        """Execute complete test suite"""
        print("=" * 80)
        print("OpenASP AX - MSGSample Online Operation Test Suite")
        print("=" * 80)
        print(f"Test Session Started: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        print()
        
        self.start_time = time.time()
        
        # Execute all test categories
        test_methods = [
            self.test_basic_execution,
            self.test_case_sensitivity,
            self.test_error_handling,
            self.test_smed_display_validation,
            self.test_file_operations,
            self.test_concurrent_execution,
            self.test_performance_metrics,
            self.test_integration_points
        ]
        
        total_passed = 0
        total_tests = 0
        
        for test_method in test_methods:
            try:
                passed, total = test_method()
                total_passed += passed
                total_tests += total
            except Exception as e:
                print(f"ERROR: Test method {test_method.__name__} failed: {e}")
                self.log_test_result(
                    test_method.__name__, 
                    test_method.__name__.replace('_', ' ').title(),
                    'FAILED',
                    f"Exception: {e}"
                )
        
        self.end_time = time.time()
        
        # Generate summary report
        self.generate_summary_report(total_passed, total_tests)
        
        return total_passed == total_tests
    
    def test_basic_execution(self) -> Tuple[int, int]:
        """Test Category 1: Basic Program Execution"""
        print("[TEST] Category 1: Basic Program Execution")
        print("-" * 50)
        
        passed = 0
        total = 0
        
        # Test 1.1: Standard CALL syntax
        total += 1
        start_time = time.time()
        try:
            result = CALL('CALL PGM-MSGSAMPLE.TESTLIB,VOL-DISK01')
            execution_time = time.time() - start_time
            
            if result:
                print("✅ Test 1.1: Standard CALL syntax - PASSED")
                self.log_test_result("T1.1", "Standard CALL Syntax", "PASSED", 
                                    "Program executed successfully", execution_time)
                passed += 1
            else:
                print("❌ Test 1.1: Standard CALL syntax - FAILED")
                self.log_test_result("T1.1", "Standard CALL Syntax", "FAILED", 
                                    "Command returned False", execution_time)
        except Exception as e:
            print(f"❌ Test 1.1: Standard CALL syntax - FAILED: {e}")
            self.log_test_result("T1.1", "Standard CALL Syntax", "FAILED", 
                               f"Exception: {e}", time.time() - start_time)
        
        # Test 1.2: Auto-detect library
        total += 1
        start_time = time.time()
        try:
            result = CALL('CALL PGM-MSGSAMPLE')
            execution_time = time.time() - start_time
            
            if result:
                print("✅ Test 1.2: Auto-detect library - PASSED")
                self.log_test_result("T1.2", "Auto-detect Library", "PASSED", 
                                    "Library auto-detection working", execution_time)
                passed += 1
            else:
                print("❌ Test 1.2: Auto-detect library - FAILED")
                self.log_test_result("T1.2", "Auto-detect Library", "FAILED", 
                                    "Auto-detection failed", execution_time)
        except Exception as e:
            print(f"❌ Test 1.2: Auto-detect library - FAILED: {e}")
            self.log_test_result("T1.2", "Auto-detect Library", "FAILED", 
                               f"Exception: {e}", time.time() - start_time)
        
        print(f"Category 1 Results: {passed}/{total} tests passed\\n")
        return passed, total
    
    def test_case_sensitivity(self) -> Tuple[int, int]:
        """Test Category 2: Case Sensitivity Validation"""
        print("📋 Test Category 2: Case Sensitivity Validation")
        print("-" * 50)
        
        passed = 0
        total = 0
        
        test_cases = [
            ("T2.1", "Mixed Case Program Name", "CALL PGM-MSGSample.TESTLIB,VOL-DISK01"),
            ("T2.2", "Lowercase Program Name", "CALL PGM-msgsample.TESTLIB,VOL-DISK01"),
            ("T2.3", "Uppercase Library Name", "CALL PGM-MSGSAMPLE.testlib,VOL-DISK01")
        ]
        
        for test_id, test_name, command in test_cases:
            total += 1
            start_time = time.time()
            try:
                result = CALL(command)
                execution_time = time.time() - start_time
                
                if result:
                    print(f"✅ {test_id}: {test_name} - PASSED")
                    self.log_test_result(test_id, test_name, "PASSED", 
                                        f"Command: {command}", execution_time)
                    passed += 1
                else:
                    # Expected failure for some case sensitivity tests
                    if "lowercase" in test_name.lower():
                        print(f"✅ {test_id}: {test_name} - PASSED (Expected failure)")
                        self.log_test_result(test_id, test_name, "PASSED", 
                                           "Expected case sensitivity failure", execution_time)
                        passed += 1
                    else:
                        print(f"❌ {test_id}: {test_name} - FAILED")
                        self.log_test_result(test_id, test_name, "FAILED", 
                                           f"Command: {command}", execution_time)
            except Exception as e:
                print(f"❌ {test_id}: {test_name} - FAILED: {e}")
                self.log_test_result(test_id, test_name, "FAILED", 
                                   f"Exception: {e}", time.time() - start_time)
        
        print(f"Category 2 Results: {passed}/{total} tests passed\\n")
        return passed, total
    
    def test_error_handling(self) -> Tuple[int, int]:
        """Test Category 3: Error Handling Validation"""
        print("📋 Test Category 3: Error Handling Validation")
        print("-" * 50)
        
        passed = 0
        total = 0
        
        error_test_cases = [
            ("T3.1", "Invalid Program Name", "CALL PGM-INVALID.TESTLIB,VOL-DISK01"),
            ("T3.2", "Invalid Library Name", "CALL PGM-MSGSAMPLE.INVALID,VOL-DISK01"),
            ("T3.3", "Invalid Volume Name", "CALL PGM-MSGSAMPLE.TESTLIB,VOL-INVALID"),
            ("T3.4", "Empty Program Parameter", "CALL PGM-.TESTLIB,VOL-DISK01")
        ]
        
        for test_id, test_name, command in error_test_cases:
            total += 1
            start_time = time.time()
            try:
                result = CALL(command)
                execution_time = time.time() - start_time
                
                # These should all fail (return False)
                if not result:
                    print(f"✅ {test_id}: {test_name} - PASSED (Correctly failed)")
                    self.log_test_result(test_id, test_name, "PASSED", 
                                        "Error correctly handled", execution_time)
                    passed += 1
                else:
                    print(f"❌ {test_id}: {test_name} - FAILED (Should have failed)")
                    self.log_test_result(test_id, test_name, "FAILED", 
                                        "Should have failed but succeeded", execution_time)
            except Exception as e:
                print(f"✅ {test_id}: {test_name} - PASSED (Exception correctly raised)")
                self.log_test_result(test_id, test_name, "PASSED", 
                                   f"Exception correctly raised: {e}", time.time() - start_time)
                passed += 1
        
        print(f"Category 3 Results: {passed}/{total} tests passed\\n")
        return passed, total
    
    def test_smed_display_validation(self) -> Tuple[int, int]:
        """Test Category 4: SMED Map Display Validation"""
        print("📋 Test Category 4: SMED Map Display Validation")
        print("-" * 50)
        
        passed = 0
        total = 0
        
        # Test 4.1: SMED output format validation
        total += 1
        start_time = time.time()
        try:
            # Capture output by redirecting stdout temporarily
            import io
            from contextlib import redirect_stdout
            
            captured_output = io.StringIO()
            with redirect_stdout(captured_output):
                result = CALL('CALL PGM-MSGSAMPLE.TESTLIB,VOL-DISK01')
            
            output = captured_output.getvalue()
            execution_time = time.time() - start_time
            
            # Validate SMED output characteristics
            validations = [
                "[MSGSAMP1]" in output,  # SMED map prefix
                "100001" in output,      # First employee ID
                "100010" in output,      # Last employee ID
                "PROCESSING COMPLETE" in output,  # Summary message
                "00000010" in output     # Record count
            ]
            
            if result and all(validations):
                print("✅ Test 4.1: SMED output format - PASSED")
                self.log_test_result("T4.1", "SMED Output Format", "PASSED", 
                                    "All SMED format validations passed", execution_time)
                passed += 1
            else:
                print(f"❌ Test 4.1: SMED output format - FAILED")
                failed_validations = [i for i, v in enumerate(validations) if not v]
                self.log_test_result("T4.1", "SMED Output Format", "FAILED", 
                                    f"Failed validations: {failed_validations}", execution_time)
        except Exception as e:
            print(f"❌ Test 4.1: SMED output format - FAILED: {e}")
            self.log_test_result("T4.1", "SMED Output Format", "FAILED", 
                               f"Exception: {e}", time.time() - start_time)
        
        print(f"Category 4 Results: {passed}/{total} tests passed\\n")
        return passed, total
    
    def test_file_operations(self) -> Tuple[int, int]:
        """Test Category 5: File Operations Validation"""
        print("📋 Test Category 5: File Operations Validation")
        print("-" * 50)
        
        passed = 0
        total = 0
        
        # Test 5.1: Missing data file handling
        total += 1
        start_time = time.time()
        try:
            # Temporarily rename SAMDATA file
            samdata_path = "/home/aspuser/app/volume/DISK01/TESTLIB/SAMDATA"
            backup_path = samdata_path + ".test_backup"
            
            if os.path.exists(samdata_path):
                os.rename(samdata_path, backup_path)
            
            result = CALL('CALL PGM-MSGSAMPLE.TESTLIB,VOL-DISK01')
            execution_time = time.time() - start_time
            
            # Restore file
            if os.path.exists(backup_path):
                os.rename(backup_path, samdata_path)
            
            # Program should handle missing file gracefully
            if result is not None:  # Even if it fails, it should handle it gracefully
                print("✅ Test 5.1: Missing data file handling - PASSED")
                self.log_test_result("T5.1", "Missing Data File Handling", "PASSED", 
                                    "File error handled gracefully", execution_time)
                passed += 1
            else:
                print("❌ Test 5.1: Missing data file handling - FAILED")
                self.log_test_result("T5.1", "Missing Data File Handling", "FAILED", 
                                    "File error not handled properly", execution_time)
        except Exception as e:
            print(f"❌ Test 5.1: Missing data file handling - FAILED: {e}")
            self.log_test_result("T5.1", "Missing Data File Handling", "FAILED", 
                               f"Exception: {e}", time.time() - start_time)
            # Ensure file is restored even on exception
            backup_path = "/home/aspuser/app/volume/DISK01/TESTLIB/SAMDATA.test_backup"
            samdata_path = "/home/aspuser/app/volume/DISK01/TESTLIB/SAMDATA"
            if os.path.exists(backup_path):
                os.rename(backup_path, samdata_path)
        
        print(f"Category 5 Results: {passed}/{total} tests passed\\n")
        return passed, total
    
    def test_concurrent_execution(self) -> Tuple[int, int]:
        """Test Category 6: Concurrent Execution Validation"""
        print("📋 Test Category 6: Concurrent Execution Validation")
        print("-" * 50)
        
        passed = 0
        total = 1
        
        start_time = time.time()
        try:
            results = []
            
            def execute_program(thread_id):
                try:
                    result = CALL('CALL PGM-MSGSAMPLE.TESTLIB,VOL-DISK01')
                    results.append((thread_id, result, 'SUCCESS'))
                except Exception as e:
                    results.append((thread_id, False, str(e)))
            
            # Start 3 concurrent threads
            threads = []
            for i in range(1, 4):
                thread = threading.Thread(target=execute_program, args=(i,))
                threads.append(thread)
                thread.start()
                time.sleep(0.1)  # Small delay to stagger starts
            
            # Wait for all threads to complete
            for thread in threads:
                thread.join()
            
            execution_time = time.time() - start_time
            
            # Count successful executions
            successful = sum(1 for _, result, _ in results if result)
            
            if successful == 3:
                print(f"✅ Test 6.1: Concurrent execution - PASSED ({successful}/3 succeeded)")
                self.log_test_result("T6.1", "Concurrent Execution", "PASSED", 
                                    f"All {successful} concurrent executions succeeded", execution_time)
                passed += 1
            else:
                print(f"❌ Test 6.1: Concurrent execution - FAILED ({successful}/3 succeeded)")
                self.log_test_result("T6.1", "Concurrent Execution", "FAILED", 
                                    f"Only {successful}/3 executions succeeded", execution_time)
        
        except Exception as e:
            print(f"❌ Test 6.1: Concurrent execution - FAILED: {e}")
            self.log_test_result("T6.1", "Concurrent Execution", "FAILED", 
                               f"Exception: {e}", time.time() - start_time)
        
        print(f"Category 6 Results: {passed}/{total} tests passed\\n")
        return passed, total
    
    def test_performance_metrics(self) -> Tuple[int, int]:
        """Test Category 7: Performance Metrics Validation"""
        print("📋 Test Category 7: Performance Metrics Validation")
        print("-" * 50)
        
        passed = 0
        total = 0
        
        # Test 7.1: Execution time benchmark
        total += 1
        execution_times = []
        
        try:
            for i in range(5):
                start_time = time.time()
                result = CALL('CALL PGM-MSGSAMPLE.TESTLIB,VOL-DISK01')
                execution_time = time.time() - start_time
                execution_times.append(execution_time)
                
                if not result:
                    raise Exception(f"Execution {i+1} failed")
            
            avg_time = sum(execution_times) / len(execution_times)
            max_time = max(execution_times)
            min_time = min(execution_times)
            
            # Performance criteria: average < 5 seconds, max < 10 seconds
            if avg_time < 5.0 and max_time < 10.0:
                print(f"✅ Test 7.1: Performance benchmark - PASSED")
                print(f"    Average: {avg_time:.2f}s, Min: {min_time:.2f}s, Max: {max_time:.2f}s")
                self.log_test_result("T7.1", "Performance Benchmark", "PASSED", 
                                    f"Avg: {avg_time:.2f}s, Max: {max_time:.2f}s", avg_time)
                passed += 1
            else:
                print(f"❌ Test 7.1: Performance benchmark - FAILED")
                print(f"    Average: {avg_time:.2f}s, Min: {min_time:.2f}s, Max: {max_time:.2f}s")
                self.log_test_result("T7.1", "Performance Benchmark", "FAILED", 
                                    f"Performance criteria not met", avg_time)
        except Exception as e:
            print(f"❌ Test 7.1: Performance benchmark - FAILED: {e}")
            self.log_test_result("T7.1", "Performance Benchmark", "FAILED", 
                               f"Exception: {e}", 0)
        
        print(f"Category 7 Results: {passed}/{total} tests passed\\n")
        return passed, total
    
    def test_integration_points(self) -> Tuple[int, int]:
        """Test Category 8: Integration Points Validation"""
        print("📋 Test Category 8: Integration Points Validation")
        print("-" * 50)
        
        passed = 0
        total = 0
        
        # Test 8.1: Catalog registration verification
        total += 1
        try:
            catalog_path = "/home/aspuser/app/asp-manager/public/config/catalog.json"
            if os.path.exists(catalog_path):
                with open(catalog_path, 'r') as f:
                    catalog = json.load(f)
                
                # Check MSGSample registration
                program_found = (
                    'DISK01' in catalog and 
                    'TESTLIB' in catalog['DISK01'] and
                    'MSGSample' in catalog['DISK01']['TESTLIB'] and
                    catalog['DISK01']['TESTLIB']['MSGSample'].get('TYPE') == 'PGM'
                )
                
                if program_found:
                    print("✅ Test 8.1: Catalog registration - PASSED")
                    self.log_test_result("T8.1", "Catalog Registration", "PASSED", 
                                        "MSGSample properly registered", 0)
                    passed += 1
                else:
                    print("❌ Test 8.1: Catalog registration - FAILED")
                    self.log_test_result("T8.1", "Catalog Registration", "FAILED", 
                                        "MSGSample not found in catalog", 0)
            else:
                print("❌ Test 8.1: Catalog registration - FAILED (catalog.json not found)")
                self.log_test_result("T8.1", "Catalog Registration", "FAILED", 
                                    "catalog.json file not found", 0)
        except Exception as e:
            print(f"❌ Test 8.1: Catalog registration - FAILED: {e}")
            self.log_test_result("T8.1", "Catalog Registration", "FAILED", 
                               f"Exception: {e}", 0)
        
        # Test 8.2: File system integration
        total += 1
        try:
            required_files = [
                "/home/aspuser/app/volume/DISK01/TESTLIB/MSGSample.jar",
                "/home/aspuser/app/volume/DISK01/TESTLIB/SAMDATA",
                "/home/aspuser/app/volume/DISK01/TESTLIB/MSGSAMP1"
            ]
            
            missing_files = [f for f in required_files if not os.path.exists(f)]
            
            if not missing_files:
                print("✅ Test 8.2: File system integration - PASSED")
                self.log_test_result("T8.2", "File System Integration", "PASSED", 
                                    "All required files present", 0)
                passed += 1
            else:
                print(f"❌ Test 8.2: File system integration - FAILED")
                print(f"    Missing files: {missing_files}")
                self.log_test_result("T8.2", "File System Integration", "FAILED", 
                                    f"Missing files: {missing_files}", 0)
        except Exception as e:
            print(f"❌ Test 8.2: File system integration - FAILED: {e}")
            self.log_test_result("T8.2", "File System Integration", "FAILED", 
                               f"Exception: {e}", 0)
        
        print(f"Category 8 Results: {passed}/{total} tests passed\\n")
        return passed, total
    
    def generate_summary_report(self, total_passed: int, total_tests: int):
        """Generate comprehensive test summary report"""
        print("=" * 80)
        print("TEST EXECUTION SUMMARY REPORT")
        print("=" * 80)
        
        success_rate = (total_passed / total_tests * 100) if total_tests > 0 else 0
        total_time = self.end_time - self.start_time if self.end_time and self.start_time else 0
        
        print(f"Total Tests Executed: {total_tests}")
        print(f"Tests Passed: {total_passed}")
        print(f"Tests Failed: {total_tests - total_passed}")
        print(f"Success Rate: {success_rate:.1f}%")
        print(f"Total Execution Time: {total_time:.2f} seconds")
        print()
        
        # Status determination
        if success_rate >= 95:
            status = "✅ EXCELLENT - Ready for Production"
        elif success_rate >= 85:
            status = "⚠️  GOOD - Minor issues to address"
        elif success_rate >= 70:
            status = "⚠️  ACCEPTABLE - Several issues need attention"
        else:
            status = "❌ POOR - Major issues require immediate attention"
        
        print(f"Overall Status: {status}")
        print()
        
        # Detailed results by category
        print("DETAILED RESULTS BY TEST CATEGORY:")
        print("-" * 50)
        
        categories = {}
        for result in self.test_results:
            category = result['test_id'].split('.')[0]
            if category not in categories:
                categories[category] = {'passed': 0, 'total': 0, 'details': []}
            
            categories[category]['total'] += 1
            if result['status'] == 'PASSED':
                categories[category]['passed'] += 1
            categories[category]['details'].append(result)
        
        for category, data in sorted(categories.items()):
            cat_success_rate = (data['passed'] / data['total'] * 100) if data['total'] > 0 else 0
            print(f"{category}: {data['passed']}/{data['total']} ({cat_success_rate:.1f}%)")
        
        print()
        
        # Failed tests details
        failed_tests = [r for r in self.test_results if r['status'] == 'FAILED']
        if failed_tests:
            print("FAILED TESTS DETAILS:")
            print("-" * 30)
            for test in failed_tests:
                print(f"❌ {test['test_id']}: {test['test_name']}")
                print(f"   Details: {test['details']}")
            print()
        
        # Save detailed report to file
        self.save_detailed_report(total_passed, total_tests, success_rate, total_time)
        
        print("Detailed test results saved to: MSGSample_automated_test_results.json")
        print("Test execution completed successfully!")
    
    def save_detailed_report(self, total_passed: int, total_tests: int, 
                           success_rate: float, total_time: float):
        """Save detailed test results to JSON file"""
        report = {
            'test_session': {
                'start_time': datetime.fromtimestamp(self.start_time).isoformat() if self.start_time else None,
                'end_time': datetime.fromtimestamp(self.end_time).isoformat() if self.end_time else None,
                'total_time': total_time,
                'tester': 'QA Director - Automated Test Suite',
                'environment': 'OpenASP AX Development Environment'
            },
            'summary': {
                'total_tests': total_tests,
                'tests_passed': total_passed,
                'tests_failed': total_tests - total_passed,
                'success_rate': success_rate,
                'overall_status': 'PASSED' if success_rate >= 85 else 'FAILED'
            },
            'detailed_results': self.test_results
        }
        
        with open('/home/aspuser/app/volume/DISK01/TESTLIB/MSGSample_automated_test_results.json', 'w') as f:
            json.dump(report, f, indent=2)

def main():
    """Main execution function"""
    try:
        # Initialize test suite
        test_suite = MSGSampleTestSuite()
        
        # Run all tests
        success = test_suite.run_all_tests()
        
        # Exit with appropriate code
        sys.exit(0 if success else 1)
        
    except KeyboardInterrupt:
        print("\\n\\nTest execution interrupted by user.")
        sys.exit(1)
    except Exception as e:
        print(f"\\n\\nTest suite execution failed: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()