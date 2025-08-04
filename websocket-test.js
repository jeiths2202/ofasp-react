/**
 * WebSocket Integration Test
 * Tests the connection between React frontend and SocketIO server on port 8000
 */

const io = require('socket.io-client');

console.log('Testing WebSocket connection to http://localhost:8000...');

const socket = io('http://localhost:8000', {
  transports: ['websocket', 'polling'],
  reconnection: true,
  reconnectionAttempts: 3,
  reconnectionDelay: 1000,
  timeout: 10000
});

socket.on('connect', () => {
  console.log('✅ Connected to SocketIO server');
  console.log('Session ID:', socket.id);
  
  // Test terminal registration
  console.log('📝 Registering terminal...');
  socket.emit('register_terminal', {
    terminal_id: 'webui-test',
    user: 'testuser',
    wsname: 'WSNAME00'
  });
});

socket.on('terminal_registered', (data) => {
  console.log('✅ Terminal registered successfully:', data);
  
  // Test MSGSAMPLEBROWSERMENU command
  console.log('📨 Sending MSGSAMPLEBROWSERMENU command...');
  socket.emit('command', {
    type: 'command',
    data: 'CALL MSGSAMPLEBROWSERMENU',
    terminal_id: 'webui-test',
    user: 'testuser',
    wsname: 'WSNAME00',
    timestamp: new Date().toISOString()
  });
});

socket.on('registration_response', (data) => {
  console.log('📋 Registration response:', data);
});

socket.on('smed_display', (data) => {
  console.log('🗂️  SMED display data received:', JSON.stringify(data, null, 2));
});

socket.on('display_map', (data) => {
  console.log('🗺️  Display map data received:', JSON.stringify(data, null, 2));
});

socket.on('terminal_output', (data) => {
  console.log('📺 Terminal output received:', data);
});

socket.on('command_response', (data) => {
  console.log('📝 Command response received:', data);
});

socket.on('disconnect', (reason) => {
  console.log('❌ Disconnected:', reason);
});

socket.on('connect_error', (error) => {
  console.error('❌ Connection error:', error.message);
});

// Test for 30 seconds, then disconnect
setTimeout(() => {
  console.log('🔌 Test completed, disconnecting...');
  socket.disconnect();
  process.exit(0);
}, 30000);

console.log('⏱️  Test running for 30 seconds...');