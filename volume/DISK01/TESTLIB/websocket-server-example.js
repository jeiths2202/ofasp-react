/**
 * WebSocket Server Example for OpenASP Terminal
 * This is a simple Node.js WebSocket server that handles terminal registration and commands
 * 
 * Usage:
 * 1. Install dependencies: npm install ws express
 * 2. Run server: node websocket-server-example.js
 * 3. Server will listen on port 3006
 */

const WebSocket = require('ws');
const express = require('express');
const http = require('http');

const app = express();
const server = http.createServer(app);

// Enable CORS for all routes
app.use((req, res, next) => {
  res.header('Access-Control-Allow-Origin', '*');
  res.header('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, OPTIONS');
  res.header('Access-Control-Allow-Headers', 'Origin, X-Requested-With, Content-Type, Accept, Authorization');
  
  if (req.method === 'OPTIONS') {
    res.sendStatus(200);
  } else {
    next();
  }
});

app.use(express.json());

// Store active connections and registered terminals
const activeConnections = new Map();
const registeredTerminals = new Map();

// WebSocket server
const wss = new WebSocket.Server({ 
  server: server,
  path: '/terminal'
});

// HTTP API endpoint for SMED commands
app.post('/api/smed/send/', (req, res) => {
  console.log('HTTP API request received:', req.body);
  
  const { command, workstation, username, sessionId } = req.body;
  
  if (!workstation) {
    return res.status(400).json({ 
      success: false, 
      message: 'Workstation ID required' 
    });
  }
  
  // Find the WebSocket connection for this workstation
  const connection = Array.from(activeConnections.values())
    .find(conn => conn.workstation === workstation);
  
  if (!connection) {
    return res.status(404).json({ 
      success: false, 
      message: 'No active connections for this workstation' 
    });
  }
  
  // Process MSGSAMPLEBROWSERMENU command
  if (command === 'MSGSAMPLEBROWSERMENU') {
    const sampleData = generateSampleEmployeeData();
    
    connection.ws.send(JSON.stringify({
      type: 'smed_map',
      data: sampleData
    }));
    
    res.json({ 
      success: true, 
      message: 'SMED data sent successfully',
      recordCount: sampleData.data.length
    });
  } else {
    res.json({ 
      success: true, 
      message: 'Command processed via HTTP API' 
    });
  }
});

// WebSocket connection handler
wss.on('connection', (ws, req) => {
  console.log('New WebSocket connection from:', req.socket.remoteAddress);
  
  const connectionId = generateId();
  activeConnections.set(connectionId, { ws, registered: false });
  
  // Send connection acknowledgment
  ws.send(JSON.stringify({
    type: 'status',
    data: 'Server acknowledged connection'
  }));
  
  ws.on('message', (data) => {
    try {
      const message = JSON.parse(data.toString());
      console.log('Received message:', message);
      
      handleWebSocketMessage(connectionId, message);
      
    } catch (error) {
      console.error('Error parsing message:', error);
      ws.send(JSON.stringify({
        type: 'error',
        data: 'Invalid message format'
      }));
    }
  });
  
  ws.on('close', () => {
    console.log('WebSocket connection closed:', connectionId);
    const connection = activeConnections.get(connectionId);
    if (connection && connection.workstation) {
      registeredTerminals.delete(connection.workstation);
    }
    activeConnections.delete(connectionId);
  });
  
  ws.on('error', (error) => {
    console.error('WebSocket error:', error);
    activeConnections.delete(connectionId);
  });
});

function handleWebSocketMessage(connectionId, message) {
  const connection = activeConnections.get(connectionId);
  if (!connection) return;
  
  switch (message.type) {
    case 'register_terminal':
      handleTerminalRegistration(connectionId, message.data);
      break;
      
    case 'command':
      handleCommand(connectionId, message.data, message.workstation);
      break;
      
    default:
      console.log('Unknown message type:', message.type);
      connection.ws.send(JSON.stringify({
        type: 'error',
        data: `Unknown message type: ${message.type}`
      }));
  }
}

function handleTerminalRegistration(connectionId, data) {
  const connection = activeConnections.get(connectionId);
  if (!connection) return;
  
  const { username, workstation, sessionId } = data;
  
  if (!username || !workstation || !sessionId) {
    connection.ws.send(JSON.stringify({
      type: 'registration_response',
      data: {
        success: false,
        message: 'Missing required registration fields'
      }
    }));
    return;
  }
  
  // Update connection info
  connection.registered = true;
  connection.username = username;
  connection.workstation = workstation;
  connection.sessionId = sessionId;
  connection.registrationTime = new Date();
  
  // Store in registered terminals map
  registeredTerminals.set(workstation, connectionId);
  
  console.log(`Terminal registered: ${workstation} (User: ${username}, Session: ${sessionId})`);
  
  // Send success response
  connection.ws.send(JSON.stringify({
    type: 'registration_response',
    data: {
      success: true,
      message: `Terminal ${workstation} registered successfully`,
      workstation: workstation,
      username: username,
      sessionId: sessionId
    }
  }));
}

function handleCommand(connectionId, command, workstation) {
  const connection = activeConnections.get(connectionId);
  if (!connection) return;
  
  if (!connection.registered) {
    connection.ws.send(JSON.stringify({
      type: 'error',
      data: 'Terminal not registered'
    }));
    return;
  }
  
  console.log(`Executing command: ${command} on workstation: ${workstation}`);
  
  // Handle specific commands
  if (command === 'CALL MSGSAMPLEBROWSERMENU' || command === 'MSGSAMPLEBROWSERMENU') {
    const sampleData = generateSampleEmployeeData();
    
    connection.ws.send(JSON.stringify({
      type: 'smed_map',
      data: sampleData
    }));
    
    connection.ws.send(JSON.stringify({
      type: 'terminal_output',
      data: `MSGSAMPLEBROWSERMENU executed successfully. ${sampleData.data.length} records loaded.`
    }));
  } else if (command === 'HELP') {
    connection.ws.send(JSON.stringify({
      type: 'terminal_output',
      data: `Available commands:\n- MSGSAMPLEBROWSERMENU: Browse employee records\n- HELP: Show this help\n- STATUS: Show system status`
    }));
  } else if (command === 'STATUS') {
    connection.ws.send(JSON.stringify({
      type: 'terminal_output',
      data: `System Status:\n- Workstation: ${connection.workstation}\n- User: ${connection.username}\n- Session: ${connection.sessionId}\n- Connected: ${new Date().toISOString()}`
    }));
  } else {
    connection.ws.send(JSON.stringify({
      type: 'terminal_output',
      data: `Command executed: ${command}`
    }));
  }
}

function generateSampleEmployeeData() {
  const employees = [
    { id: 'EMP001', name: 'John Smith', dept: 'Engineering', salary: '$75,000', hire_date: '2020-01-15', status: 'Active' },
    { id: 'EMP002', name: 'Mary Johnson', dept: 'Marketing', salary: '$68,000', hire_date: '2019-03-22', status: 'Active' },
    { id: 'EMP003', name: 'Robert Brown', dept: 'Sales', salary: '$62,000', hire_date: '2021-06-10', status: 'Active' },
    { id: 'EMP004', name: 'Lisa Davis', dept: 'HR', salary: '$58,000', hire_date: '2018-11-05', status: 'Active' },
    { id: 'EMP005', name: 'Michael Wilson', dept: 'Finance', salary: '$72,000', hire_date: '2020-09-12', status: 'Active' },
    { id: 'EMP006', name: 'Sarah Garcia', dept: 'Engineering', salary: '$78,000', hire_date: '2019-07-18', status: 'Active' },
    { id: 'EMP007', name: 'David Miller', dept: 'Operations', salary: '$65,000', hire_date: '2021-02-28', status: 'Active' },
    { id: 'EMP008', name: 'Jennifer Taylor', dept: 'Marketing', salary: '$70,000', hire_date: '2020-05-14', status: 'Active' }
  ];
  
  return {
    type: 'smed_map',
    map_name: 'EMPLOYEE_BROWSER',
    title: 'Employee Database Browser',
    subtitle: 'SAM File Employee Records',
    headers: ['ID', 'Name', 'Department', 'Salary', 'Hire Date', 'Status'],
    page_info: {
      current: 1,
      total: 1
    },
    function_keys: 'F1=Help F3=Exit F7=Up F8=Down',
    status: 'Ready',
    data: employees
  };
}

function generateId() {
  return 'conn_' + Math.random().toString(36).substr(2, 16) + '_' + Date.now();
}

// Health check endpoint
app.get('/health', (req, res) => {
  res.json({
    status: 'healthy',
    activeConnections: activeConnections.size,
    registeredTerminals: registeredTerminals.size,
    timestamp: new Date().toISOString()
  });
});

// Start server
const PORT = process.env.PORT || 3006;
server.listen(PORT, () => {
  console.log(`OpenASP WebSocket Server running on port ${PORT}`);
  console.log(`WebSocket endpoint: ws://localhost:${PORT}/terminal`);
  console.log(`HTTP API endpoint: http://localhost:${PORT}/api/smed/send/`);
  console.log(`Health check: http://localhost:${PORT}/health`);
});

// Graceful shutdown
process.on('SIGINT', () => {
  console.log('Shutting down server...');
  server.close(() => {
    console.log('Server closed.');
    process.exit(0);
  });
});