#!/bin/bash
# Test script for MSGSample deployment verification

echo "MSGSample Deployment Test"
echo "========================="
echo ""

# Check if all required files exist
echo "Checking deployment files..."
FILES=(
    "MSGSample.jar"
    "MSGSample.class"
    "MSGSample.java"
    "MSGSample.sh"
    "SAMDATA"
    "MSGSAMP1"
    "TESTMSG_CL"
)

for FILE in "${FILES[@]}"; do
    if [ -f "$FILE" ]; then
        echo "[OK] $FILE exists"
    else
        echo "[NG] $FILE missing!"
    fi
done

echo ""
echo "Testing MSGSample execution..."
echo "------------------------------"

# Run the program
java MSGSample

if [ $? -eq 0 ]; then
    echo ""
    echo "[OK] MSGSample executed successfully"
else
    echo ""
    echo "[NG] MSGSample execution failed"
fi

echo ""
echo "Test complete."