#\!/bin/bash
# Test System API Server on port 3004

echo "=== System API Server Test ==="
echo

# Check if server is running
if netstat -tlnp 2>/dev/null  < /dev/null |  grep -q ":3004"; then
    echo "✓ Server is running on port 3004"
else
    echo "✗ Server is not running on port 3004"
    exit 1
fi

# Test API endpoint
echo "Testing /api/system endpoint..."
response=$(curl -s -w "%{http_code}" http://localhost:3004/api/system)
http_code="${response: -3}"
json_response="${response%???}"

if [ "$http_code" = "200" ]; then
    echo "✓ HTTP Response: 200 OK"
    
    # Parse JSON response (if jq is available)
    if command -v jq >/dev/null 2>&1; then
        hostname=$(echo "$json_response" | jq -r '.system_info.hostname')
        cpu_percent=$(echo "$json_response" | jq -r '.system_info.cpu_percent')
        memory_percent=$(echo "$json_response" | jq -r '.system_info.memory_percent')
        last_update=$(echo "$json_response" | jq -r '.last_update')
        
        echo "  Hostname: $hostname"
        echo "  CPU Usage: ${cpu_percent}%"
        echo "  Memory Usage: ${memory_percent}%"
        echo "  Last Update: $last_update"
    else
        echo "  Response length: ${#json_response} characters"
        echo "  Sample: ${json_response:0:100}..."
    fi
else
    echo "✗ HTTP Response: $http_code"
    echo "Response: $json_response"
    exit 1
fi

echo
echo "=== Test Complete ==="
echo "System API Server is working correctly\!"
