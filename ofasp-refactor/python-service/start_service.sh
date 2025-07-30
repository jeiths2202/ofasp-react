#!/bin/bash
# Production startup script for Python conversion service
# Following CODING_RULES.md - no hardcoded values

# Set environment variables
export FLASK_ENV=production
export FLASK_DEBUG=false

# Check if .env file exists
if [ ! -f .env ]; then
    echo "Error: .env file not found. Please copy .env.example to .env and configure."
    exit 1
fi

# Check if requirements are installed
if ! python -c "import flask" 2>/dev/null; then
    echo "Installing dependencies..."
    pip install -r requirements.txt
fi

# Check if codepage files exist
CODEPAGE_PATH="../public/codepages"
if [ ! -d "$CODEPAGE_PATH" ]; then
    echo "Error: Codepage directory not found at $CODEPAGE_PATH"
    exit 1
fi

# Check if required codepage files exist
for file in "EBCASCUS.txt" "EBCASCJP.txt" "JEFASCK.txt" "KEISASCK.txt" "ASCEBCUS.txt" "ASCEBCJP.txt" "ASCJEFK.txt" "ASCJEISK.txt"; do
    if [ ! -f "$CODEPAGE_PATH/$file" ]; then
        echo "Error: Required codepage file not found: $CODEPAGE_PATH/$file"
        exit 1
    fi
done

echo "Starting Python conversion service..."
echo "Configuration loaded from .env file"
echo "Codepage files verified"

# Start the service
python main.py