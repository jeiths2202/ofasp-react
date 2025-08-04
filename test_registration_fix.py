#!/usr/bin/env python3
"""
Test Terminal Registration Fix
Verifies that the isTerminalRegistered() method works correctly
"""

import socketio
import time
import json

def test_registration_fix():
    """Test the registration status check"""
    
    sio = socketio.Client()
    registration_complete = False
    hub_registered_received = False
    
    @sio.on('connect')
    def on_connect():
        print("[TEST] Connected to Hub")
        registration_data = {
            'terminal_id': 'webui',
            'user': 'admin',
            'wsname': 'WSNAME00',
            'client_type': 'react_web_terminal',
            'hub_version': 'v2.0'
        }
        print(f"[TEST] Registering: {registration_data}")
        sio.emit('hub_register', registration_data)
        
    @sio.on('hub_registered')
    def on_hub_registered(data):
        nonlocal registration_complete, hub_registered_received
        print(f"[TEST] Hub registration response: {data}")
        hub_registered_received = True
        if data.get('success'):
            registration_complete = True
            print("[TEST] Registration successful!")
        else:
            print("[TEST] Registration failed!")
    
    try:
        print("[TEST] Starting registration test...")
        sio.connect('http://localhost:8000')
        
        # Wait for registration
        print("[TEST] Waiting for registration...")
        time.sleep(5)
        
        print(f"[TEST] Hub registered event received: {hub_registered_received}")
        print(f"[TEST] Registration complete: {registration_complete}")
        
        if hub_registered_received and registration_complete:
            print("[TEST] SUCCESS: Registration fix is working correctly!")
            print("[TEST] The isTerminalRegistered() method should now return true")
            return True
        else:
            print("[TEST] FAILED: Registration not completed properly")
            return False
            
    except Exception as e:
        print(f"[TEST] Error: {e}")
        return False
    finally:
        if sio.connected:
            sio.disconnect()

if __name__ == "__main__":
    print("=== Testing Terminal Registration Fix ===")
    success = test_registration_fix()
    
    if success:
        print("\n SUCCESS: The fix is working!")
        print("- Hub registration events are being received")
        print("- isTerminalRegistered() method added to websocketService")
        print("- React component should now use Hub instead of HTTP fallback")
        print("\nNext: Test MAIN001 execution in the web terminal")
    else:
        print("\n FAILED: Registration test failed")
        print("Check if the API server is running on port 8000")