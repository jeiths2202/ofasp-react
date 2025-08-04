#!/usr/bin/env python3
"""
Test WebSocket Hub Command Execution
Simulates the web terminal sending CALL PGM-MAIN001.JAVA,VOL-DISK01
"""

import socketio
import time
import json

def test_websocket_command():
    """Test WebSocket Hub command execution"""
    
    # Create SocketIO client
    sio = socketio.Client()
    
    # Event handlers
    @sio.on('connect')
    def on_connect():
        print("[TEST] Connected to WebSocket Hub")
        
    @sio.on('command_confirmation')
    def on_command_confirmation(data):
        print(f"[TEST] Command confirmation: {data}")
        
    @sio.on('command_error')
    def on_command_error(data):
        print(f"[TEST] Command error: {data}")
        
    @sio.on('smed_data_direct')
    def on_smed_data(data):
        print(f"[TEST] SMED data received: {json.dumps(data, indent=2)}")
        
    @sio.on('disconnect')
    def on_disconnect():
        print("[TEST] Disconnected from WebSocket Hub")
    
    try:
        # Connect to the API server
        print("[TEST] Connecting to WebSocket Hub at http://localhost:8000...")
        sio.connect('http://localhost:8000')
        
        # Wait for connection
        time.sleep(1)
        
        # Send the CALL command
        command_data = {
            'command': 'CALL PGM-MAIN001.JAVA,VOL-DISK01',
            'terminal_id': 'webui',
            'user': 'testuser'
        }
        
        print(f"[TEST] Sending command: {command_data}")
        sio.emit('hub_command', command_data)
        
        # Wait for response
        print("[TEST] Waiting for response...")
        time.sleep(10)
        
    except Exception as e:
        print(f"[TEST] Error: {e}")
    finally:
        if sio.connected:
            sio.disconnect()
        print("[TEST] Test completed")

if __name__ == "__main__":
    test_websocket_command()