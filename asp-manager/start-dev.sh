#!/bin/bash

# Start script for ASP Manager development
# This script starts both the React development server and the file API server

echo "Starting ASP Manager development servers..."

# Function to cleanup on exit
cleanup() {
    echo "Shutting down servers..."
    kill $REACT_PID $API_PID 2>/dev/null
    exit
}

# Set up trap to cleanup on script exit
trap cleanup EXIT INT TERM

# Start the file API server
echo "Starting file API server on port 3008..."
cd server
npm install
node fileApi.js &
API_PID=$!
cd ..

# Give the API server a moment to start
sleep 2

# Start the React development server
echo "Starting React development server on port 3007..."
npm start &
REACT_PID=$!

# Wait for both processes
echo "Servers are running. Press Ctrl+C to stop."
wait $REACT_PID $API_PID