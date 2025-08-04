#!/bin/bash
# Test script for MAIN001 ClassLoader improvement

echo "=== MAIN001 ClassLoader Test ==="
echo "Testing if MAIN001 can call SUB001 in the same process"
echo ""

# Change to Java directory
cd /home/aspuser/app/volume/DISK01/JAVA

# Run MAIN001 with option 1 (which calls SUB001)
echo "Running: java -Dfile.encoding=UTF-8 -cp . MAIN001 1"
java -Dfile.encoding=UTF-8 -cp . MAIN001 1

echo ""
echo "=== Test Complete ==="