# WebSocket Server Setup Guide

## Overview
This guide explains how to set up and run the WebSocket server for the OpenASP Web Terminal application.

## Quick Start

### 1. Install Dependencies
```bash
# Copy server package.json
cp server-package.json package.json

# Install dependencies
npm install
```

### 2. Start the Server
```bash
# Start the server
npm start

# Or for development with auto-reload
npm run dev
```

### 3. Verify Server is Running
```bash
# Check health endpoint
curl http://localhost:3006/health
```

Expected response:
```json
{
  "status": "healthy",
  "activeConnections": 0,
  "registeredTerminals": 0,
  "timestamp": "2025-07-30T..."
}
```

## Server Features

### WebSocket Endpoints
- **Connection**: `ws://localhost:3006/terminal`
- **Health Check**: `http://localhost:3006/health`
- **SMED API**: `http://localhost:3006/api/smed/send/`

### Supported Messages

#### Terminal Registration
```json
{
  "type": "register_terminal",
  "data": {
    "username": "john_doe",
    "workstation": "WS001",
    "sessionId": "sess_abc123...",
    "timestamp": "2025-07-30T..."
  }
}
```

#### Command Execution
```json
{
  "type": "command",
  "data": "MSGSAMPLEBROWSERMENU",
  "workstation": "WS001",
  "sessionId": "sess_abc123...",
  "timestamp": "2025-07-30T..."
}
```

### HTTP API

#### Send SMED Command
```bash
curl -X POST http://localhost:3006/api/smed/send/ \
  -H "Content-Type: application/json" \
  -d '{
    "command": "MSGSAMPLEBROWSERMENU",
    "workstation": "WS001",
    "username": "john_doe",
    "sessionId": "sess_abc123..."
  }'
```

## Configuration

### Environment Variables
- `PORT`: Server port (default: 3006)

### Customization
Edit `websocket-server-example.js` to:
- Add authentication
- Connect to real databases
- Implement additional commands
- Modify SMED data format

## Troubleshooting

### Common Issues

#### 1. Port Already in Use
```bash
# Check what's using port 3006
lsof -i :3006

# Kill process if needed
kill -9 <PID>
```

#### 2. Connection Refused
- Ensure server is running: `npm start`
- Check firewall settings
- Verify port is not blocked

#### 3. Registration Fails
- Check workstation info is provided
- Verify JSON format
- Look at server logs for error details

#### 4. MSGSAMPLEBROWSERMENU Not Working
- Ensure terminal is registered first
- Check command format
- Verify WebSocket connection is active

### Debug Mode
Add debug logging:
```javascript
// In websocket-server-example.js
const DEBUG = true;

if (DEBUG) {
  console.log('Debug message:', data);
}
```

## Integration with React App

### 1. Update Environment Variables
```bash
# In your React app
export REACT_APP_WEBSOCKET_URL=ws://localhost:3006/terminal
```

### 2. Start Both Services
```bash
# Terminal 1: Start WebSocket server
npm start

# Terminal 2: Start React app
cd .. && npm start
```

### 3. Test Connection
1. Open React app in browser
2. Login with any credentials
3. Click "Connect" in terminal
4. Look for "Terminal registered successfully" message
5. Try "Employee Browser" quick command

## Production Deployment

### Docker Setup
```dockerfile
FROM node:18-alpine
WORKDIR /app
COPY server-package.json package.json
RUN npm install --production
COPY websocket-server-example.js .
EXPOSE 3006
CMD ["npm", "start"]
```

### Security Considerations
- Implement proper authentication
- Use HTTPS/WSS in production
- Add rate limiting
- Validate all inputs
- Use environment variables for secrets

## Sample Data
The server includes sample employee data for testing:
- 8 sample employee records
- Various departments (Engineering, Marketing, Sales, HR, Finance, Operations)
- Realistic salary ranges and hire dates
- All records marked as "Active"

## API Response Examples

### Successful Registration
```json
{
  "type": "registration_response",
  "data": {
    "success": true,
    "message": "Terminal WS001 registered successfully",
    "workstation": "WS001",
    "username": "john_doe",
    "sessionId": "sess_abc123..."
  }
}
```

### SMED Map Data
```json
{
  "type": "smed_map",
  "data": {
    "type": "smed_map",
    "map_name": "EMPLOYEE_BROWSER",
    "title": "Employee Database Browser",
    "subtitle": "SAM File Employee Records",
    "headers": ["ID", "Name", "Department", "Salary", "Hire Date", "Status"],
    "page_info": { "current": 1, "total": 1 },
    "function_keys": "F1=Help F3=Exit F7=Up F8=Down",
    "status": "Ready",
    "data": [...]
  }
}
```

## Next Steps
1. Run the server using the instructions above
2. Test with the React application
3. Customize for your specific needs
4. Deploy to production environment