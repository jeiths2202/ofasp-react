#!/usr/bin/env python3
"""
Simple Hub Registration Test
"""

import socketio
import time

def test_hub_registration():
    """Test Hub registration and hub_registered event"""
    
    sio = socketio.Client()
    events_received = []
    
    @sio.on('connect')
    def on_connect():
        print("[TEST] Connected")
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
        print(f"[TEST] hub_registered event received: {data}")
        events_received.append(('hub_registered', data))
        
    @sio.on('*')
    def catch_all_events(event, *args):
        print(f"[TEST] Event: {event}, Args: {args}")
        events_received.append((event, args))
    
    try:
        sio.connect('http://localhost:8000')
        time.sleep(5)
        
        print(f"[TEST] Total events received: {len(events_received)}")
        for event_name, event_data in events_received:
            print(f"[TEST] - {event_name}: {event_data}")
            
        return len(events_received) > 0
        
    except Exception as e:
        print(f"[TEST] Error: {e}")
        return False
    finally:
        if sio.connected:
            sio.disconnect()

if __name__ == "__main__":
    print("=== Simple Hub Registration Test ===")
    success = test_hub_registration()
    print(f"Test result: {'PASS' if success else 'FAIL'}")