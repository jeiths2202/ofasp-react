#!/bin/bash
echo "=== ASP Shell Program Test ==="
echo "Program: $ASP_PROGRAM"
echo "Library: $ASP_LIBRARY" 
echo "Volume: $ASP_VOLUME"
echo "Parameters: $ASP_PARAMETERS"
echo ""
echo "Shell script executed successfully!"
echo "Environment variables available:"
echo "- ASP_PROGRAM: $ASP_PROGRAM"
echo "- ASP_LIBRARY: $ASP_LIBRARY"
echo "- ASP_VOLUME: $ASP_VOLUME"
if [ -n "$ASP_PARAMETERS" ]; then
    echo "- ASP_PARAMETERS: $ASP_PARAMETERS"
fi
echo ""
echo "Arguments received: $@"