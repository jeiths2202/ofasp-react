#!/bin/bash
# MSGSample execution wrapper for OpenASP AX
# This script executes the MSGSample Java program

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Execute the Java program with proper classpath
cd "$SCRIPT_DIR"
java -cp MSGSample.jar com.cobol2java.generated.MSGSample "$@"