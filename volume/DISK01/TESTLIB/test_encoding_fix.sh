#!/bin/bash

# Test script for MSGSampleBrowserMenuClean encoding fixes
# Tests UTF-8 JSON output with Japanese characters

echo "======================================================================"
echo "  MSGSampleBrowserMenuClean - Encoding Fix Validation Test"
echo "======================================================================"
echo ""

# Check if Java program exists
if [ ! -f "MSGSampleBrowserMenuClean.java" ]; then
    echo "ERROR: MSGSampleBrowserMenuClean.java not found"
    exit 1
fi

# Recompile with UTF-8 support
echo "1. Compiling with UTF-8 encoding support..."
echo "----------------------------------------------------------------------"
javac -encoding UTF-8 MSGSampleBrowserMenuClean.java
if [ $? -ne 0 ]; then
    echo "ERROR: Compilation failed"
    exit 1
fi
echo "✓ Compilation successful"
echo ""

# Test JSON output with UTF-8 encoding
echo "2. Testing JSON output with proper UTF-8 encoding..."
echo "----------------------------------------------------------------------"
echo '{"program": "MSGSAMPLEBROWSERMENU", "library": "TESTLIB", "volume": "DISK01", "user": "system"}' | \
java -Dfile.encoding=UTF-8 -Duser.language=ja -Duser.country=JP MSGSampleBrowserMenuClean > test_output.json 2> test_errors.log

# Check if output was generated
if [ -f "test_output.json" ]; then
    echo "✓ JSON output generated"
    
    # Validate JSON structure
    if command -v python3 &> /dev/null; then
        python3 -c "
import json
import sys
try:
    with open('test_output.json', 'r', encoding='utf-8') as f:
        data = json.load(f)
    print('✓ JSON is valid and UTF-8 readable')
    
    # Check for Japanese characters in employee names
    fields = data.get('fields', {})
    japanese_found = False
    for key, value in fields.items():
        if 'NAME' in key and any(ord(c) > 127 for c in str(value)):
            japanese_found = True
            print(f'✓ Japanese characters found in {key}: {value}')
            break
    
    if not japanese_found:
        print('⚠ Warning: No Japanese characters detected in output')
        
    # Check for mojibake patterns
    mojibake_found = False
    for key, value in fields.items():
        if 'NAME' in key and ('�' in str(value) or '?' in str(value)):
            mojibake_found = True
            print(f'✗ Mojibake detected in {key}: {value}')
    
    if not mojibake_found:
        print('✓ No mojibake characters detected')
        
except json.JSONDecodeError as e:
    print(f'✗ JSON parse error: {e}')
    sys.exit(1)
except UnicodeDecodeError as e:
    print(f'✗ UTF-8 encoding error: {e}')
    sys.exit(1)
except Exception as e:
    print(f'✗ Validation error: {e}')
    sys.exit(1)
"
    else
        echo "⚠ Warning: Python3 not available for JSON validation"
    fi
    
    # Show first few employee records
    echo ""
    echo "3. Sample employee data in JSON output:"
    echo "----------------------------------------------------------------------"
    head -c 500 test_output.json | grep -o '"EMP[0-9]_NAME": "[^"]*"' | head -3
    echo ""
    
else
    echo "✗ No JSON output generated"
    echo "Error log:"
    cat test_errors.log
    exit 1
fi

# Check error logs for encoding issues
echo "4. Checking for encoding errors..."
echo "----------------------------------------------------------------------"
if [ -s "test_errors.log" ]; then
    if grep -q "encoding" test_errors.log; then
        echo "⚠ Warning: Encoding issues detected:"
        grep "encoding" test_errors.log
    else
        echo "✓ No encoding errors in log"
    fi
else
    echo "✓ No errors logged"
fi

echo ""
echo "5. File encoding validation..."
echo "----------------------------------------------------------------------"
if command -v file &> /dev/null; then
    echo "Output file encoding: $(file -bi test_output.json)"
else
    echo "⚠ Warning: 'file' command not available for encoding check"
fi

echo ""
echo "======================================================================"
echo "  Encoding Fix Validation Test Complete"
echo "======================================================================"

# Cleanup
rm -f test_output.json test_errors.log

echo "Test completed. Review output above for encoding validation results."