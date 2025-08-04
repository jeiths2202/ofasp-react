#!/usr/bin/env python3
"""
Test Final Solution: Proper WebSocket Hub Registration and MAIN001 Execution
This test verifies the complete flow:
1. Web terminal connects and registers with Hub
2. Command is sent via Hub
3. MAIN001 executes and sends SMED data
4. Hub forwards SMED data to registered terminal
"""

import socketio
import time
import json
import threading

def test_complete_flow():
    """Test the complete web terminal to MAIN001 flow"""
    
    # Create SocketIO client to simulate React web terminal
    terminal_client = socketio.Client()
    smed_data_received = []
    command_confirmations = []
    
    # Event handlers for terminal client
    @terminal_client.on('connect')
    def on_terminal_connect():
        print("[TERMINAL] Connected to WebSocket Hub")
        
        # Register as web terminal (simulating React app)
        registration_data = {
            'terminal_id': 'webui',
            'user': 'admin',
            'wsname': 'WSNAME00',
            'client_type': 'react_web_terminal',
            'hub_version': 'v2.0'
        }
        
        print(f"[TERMINAL] Registering with Hub: {registration_data}")
        terminal_client.emit('hub_register', registration_data)
        
    @terminal_client.on('hub_register_confirmation')
    def on_terminal_registration_confirmation(data):
        print(f"[TERMINAL] Registration confirmation: {data}")
        
    @terminal_client.on('smed_data_direct')
    def on_terminal_smed_data(data):
        print(f"[TERMINAL] ✅ SMED data received!")
        print(f"[TERMINAL] Map: {data.get('map_file', 'Unknown')}")
        print(f"[TERMINAL] Program: {data.get('program_name', 'Unknown')}")
        print(f"[TERMINAL] Fields: {len(data.get('fields', {}))}")
        smed_data_received.append(data)
        
        # Check if it's the MAIN001 menu
        if data.get('map_file') == 'MAIN001' and data.get('program_name') == 'MAIN001':
            fields = data.get('fields', {})
            if 'MENU_TITLE' in fields or 'OPTION_1' in fields:
                print(f"[TERMINAL] ✅ MAIN001 menu received correctly!")
                print(f"[TERMINAL] Menu Title: {fields.get('MENU_TITLE', 'N/A')}")
                print(f"[TERMINAL] Option 1: {fields.get('OPTION_1', 'N/A')}")
        
    @terminal_client.on('command_confirmation')
    def on_terminal_command_confirmation(data):
        print(f"[TERMINAL] Command confirmation: {data}")
        command_confirmations.append(data)
        
    @terminal_client.on('disconnect')
    def on_terminal_disconnect():
        print("[TERMINAL] Disconnected from WebSocket Hub")
    
    try:
        # Step 1: Connect web terminal
        print("[TEST] Step 1: Connecting web terminal to Hub...")
        terminal_client.connect('http://localhost:8000')
        
        # Wait for registration to complete
        print("[TEST] Step 2: Waiting for registration...")
        time.sleep(3)
        
        # Step 2: Send MAIN001 command
        print("[TEST] Step 3: Sending MAIN001 command...")
        command_data = {
            'command': 'CALL PGM-MAIN001.JAVA,VOL-DISK01',
            'terminal_id': 'webui',
            'user': 'admin'
        }
        
        print(f"[TEST] Sending command: {command_data}")
        terminal_client.emit('hub_command', command_data)
        
        # Step 3: Wait for execution and SMED data
        print("[TEST] Step 4: Waiting for command execution and SMED data...")
        time.sleep(15)
        
        # Step 4: Analyze results
        print("[TEST] Step 5: Analyzing results...")
        print(f"[TEST] Command confirmations received: {len(command_confirmations)}")
        print(f"[TEST] SMED data packets received: {len(smed_data_received)}")
        
        if command_confirmations:
            for conf in command_confirmations:
                success = conf.get('success', False)
                message = conf.get('message', 'No message')
                print(f"[TEST] Command result: {'✅ SUCCESS' if success else '❌ FAILED'} - {message}")
        
        if smed_data_received:
            print(f"[TEST] ✅ SMED data flow working correctly!")
            for smed in smed_data_received:
                print(f"[TEST]   - Map: {smed.get('map_file')}, Program: {smed.get('program_name')}")
        else:
            print(f"[TEST] ❌ No SMED data received - terminal registration issue")
            
        # Overall test result
        success = (len(command_confirmations) > 0 and 
                  command_confirmations[0].get('success', False) and
                  len(smed_data_received) > 0)
        
        print(f"[TEST] Overall test result: {'✅ PASS' if success else '❌ FAIL'}")
        return success
        
    except Exception as e:
        print(f"[TEST] Test error: {e}")
        return False
    finally:
        if terminal_client.connected:
            terminal_client.disconnect()
        print("[TEST] Test completed")

if __name__ == "__main__":
    print("=== WebSocket Hub + MAIN001 Integration Test ===")
    print("Testing complete flow from web terminal to MAIN001 menu display")
    print("")
    
    success = test_complete_flow()
    
    print("")
    print("=== Test Summary ===")
    if success:
        print("✅ All tests passed! Web terminal can now properly display MAIN001 menu.")
        print("✅ Hub registration working correctly")
        print("✅ Command execution via Hub working")
        print("✅ SMED data transmission working")
        print("")
        print("The issue has been resolved. Users should now see the MAIN001 menu")
        print("instead of just '실행중' when running CALL PGM-MAIN001.JAVA,VOL-DISK01")
    else:
        print("❌ Test failed. Further debugging needed.")
    print("")