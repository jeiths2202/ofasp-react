#!/usr/bin/env python3
"""
Test MAIN001 execution via WebSocket Hub
"""

import socketio
import time
import json

def test_main001_execution():
    """Test MAIN001 execution via Hub"""
    
    sio = socketio.Client()
    events_received = []
    
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
        print(f"[TEST] Hub registration response: {data}")
        if data.get('success'):
            print("[TEST] Registration successful, sending MAIN001 command...")
            command_data = {
                'command': 'CALL PGM-MAIN001.JAVA,VOL-DISK01',
                'terminal_id': 'webui',
                'user': 'admin',
                'wsname': 'WSNAME00',
                'timestamp': time.strftime('%Y-%m-%dT%H:%M:%S.%fZ')
            }
            print(f"[TEST] Sending command: {command_data}")
            sio.emit('hub_command', command_data)
        else:
            print("[TEST] Registration failed!")
    
    @sio.on('command_confirmation')
    def on_command_confirmation(data):
        print(f"[TEST] Command confirmation: {data}")
        events_received.append(('command_confirmation', data))
    
    @sio.on('smed_data_received')
    def on_smed_data_received(data):
        print(f"[TEST] SMED data received: {data}")
        events_received.append(('smed_data_received', data))
    
    @sio.on('smed_display')
    def on_smed_display(data):
        print(f"[TEST] SMED display: {data}")
        events_received.append(('smed_display', data))
    
    @sio.on('smed_data_direct')
    def on_smed_data_direct(data):
        print(f"[TEST] SMED data direct: {data}")
        events_received.append(('smed_data_direct', data))
    
    try:
        print("[TEST] Connecting to Hub...")
        sio.connect('http://localhost:8000')
        
        print("[TEST] Waiting for MAIN001 execution and SMED data...")
        time.sleep(15)  # Wait longer for MAIN001 to execute
        
        print(f"\n[TEST] Summary:")
        print(f"Events received: {len(events_received)}")
        for event_name, event_data in events_received:
            print(f"- {event_name}: {type(event_data)}")
            if event_name.startswith('smed'):
                print(f"  Map file: {event_data.get('map_file', 'N/A')}")
                print(f"  Fields: {type(event_data.get('fields', {}))}")
        
        if any(event[0].startswith('smed') for event in events_received):
            print("\n✅ SUCCESS: SMED data was received via Hub!")
            return True
        else:
            print("\n❌ FAILED: No SMED data received")
            return False
            
    except Exception as e:
        print(f"[TEST] Error: {e}")
        return False
    finally:
        if sio.connected:
            sio.disconnect()

if __name__ == "__main__":
    print("=== Testing MAIN001 execution via WebSocket Hub ===")
    success = test_main001_execution()
    
    if success:
        print("\n🎉 Hub integration working! WebUI should receive SMED maps.")
    else:
        print("\n❌ Hub integration failed. Need to check server logs.")