#!/usr/bin/env python3
"""
Test Web Terminal Registration with WebSocket Hub
Simulates the React web terminal connecting and registering with the hub
"""

import socketio
import time
import json

def test_web_terminal_registration():
    """Test Web Terminal registration with WebSocket Hub"""
    
    # Create SocketIO client to simulate React web terminal
    sio = socketio.Client()
    
    # Event handlers
    @sio.on('connect')
    def on_connect():
        print("[WEB_TERMINAL] Connected to WebSocket Hub")
        
        # Register as web terminal (simulating React app)
        registration_data = {
            'terminal_id': 'webui',
            'user': 'admin',
            'wsname': 'WSNAME00',
            'client_type': 'react_web_terminal',
            'hub_version': 'v2.0'
        }
        
        print(f"[WEB_TERMINAL] Registering with Hub: {registration_data}")
        sio.emit('hub_register', registration_data)
        
    @sio.on('hub_register_confirmation')
    def on_registration_confirmation(data):
        print(f"[WEB_TERMINAL] Registration confirmation: {data}")
        
    @sio.on('smed_data_direct')
    def on_smed_data(data):
        print(f"[WEB_TERMINAL] SMED data received: {json.dumps(data, indent=2)}")
        
    @sio.on('command_confirmation')
    def on_command_confirmation(data):
        print(f"[WEB_TERMINAL] Command confirmation: {data}")
        
    @sio.on('disconnect')
    def on_disconnect():
        print("[WEB_TERMINAL] Disconnected from WebSocket Hub")
    
    try:
        # Connect to the API server
        print("[WEB_TERMINAL] Connecting to WebSocket Hub at http://localhost:8000...")
        sio.connect('http://localhost:8000')
        
        # Wait for registration
        time.sleep(2)
        
        # Now test command execution in a separate client
        print("[WEB_TERMINAL] Testing command execution from registered terminal...")
        
        # Send the CALL command
        command_data = {
            'command': 'CALL PGM-MAIN001.JAVA,VOL-DISK01',
            'terminal_id': 'webui',
            'user': 'admin'
        }
        
        print(f"[WEB_TERMINAL] Sending command: {command_data}")
        sio.emit('hub_command', command_data)
        
        # Wait for response and SMED data
        print("[WEB_TERMINAL] Waiting for response and SMED data...")
        time.sleep(15)
        
    except Exception as e:
        print(f"[WEB_TERMINAL] Error: {e}")
    finally:
        if sio.connected:
            sio.disconnect()
        print("[WEB_TERMINAL] Test completed")

if __name__ == "__main__":
    test_web_terminal_registration()