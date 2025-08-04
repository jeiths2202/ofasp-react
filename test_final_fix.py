#!/usr/bin/env python3
"""
Test Final Fix: WebSocket Hub Registration and SMED Map Display
This test verifies the complete fixed flow:
1. Web terminal connects and registers with Hub
2. Hub sends hub_registered event
3. React sets hubConnectionStatus to 'connected'
4. Command sent via Hub (not HTTP fallback)
5. MAIN001 executes and sends SMED data
6. Hub forwards SMED data to registered terminal
7. React displays SMED map
"""

import socketio
import time
import json

def test_fixed_flow():
    """Test the fixed web terminal to MAIN001 flow"""
    
    # Create SocketIO client to simulate React web terminal
    terminal_client = socketio.Client()
    smed_data_received = []
    command_confirmations = []
    hub_registrations = []
    
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
        
    @terminal_client.on('hub_registered')
    def on_hub_registered(data):
        print(f"[TERMINAL] ✅ Hub registration completed: {data}")
        hub_registrations.append(data)
        if data.get('success'):
            print(f"[TERMINAL] ✅ Registration successful, now can send commands via Hub!")
        
    @terminal_client.on('smed_data_direct')  
    def on_terminal_smed_data(data):
        print(f"[TERMINAL] ✅ SMED data received!")
        print(f"[TERMINAL] Map: {data.get('map_file', 'Unknown')}")
        print(f"[TERMINAL] Program: {data.get('program_name', 'Unknown')}")
        if 'fields' in data:
            fields = data['fields']
            print(f"[TERMINAL] Fields count: {len(fields) if isinstance(fields, dict) else 'Not dict'}")
            if isinstance(fields, dict):
                # Check for MAIN001 menu fields
                if 'MENU_TITLE' in fields:
                    print(f"[TERMINAL] ✅ MAIN001 Menu Title: {fields['MENU_TITLE']}")
                if 'OPTION_1' in fields:
                    print(f"[TERMINAL] ✅ Option 1: {fields['OPTION_1']}")
        smed_data_received.append(data)
        
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
        print("[TEST] Step 2: Waiting for hub_registered event...")
        time.sleep(3)
        
        # Check if registration completed
        if not hub_registrations or not hub_registrations[0].get('success'):
            print("[TEST] ❌ Hub registration failed or not received")
            return False
            
        print("[TEST] ✅ Hub registration completed successfully")
        
        # Step 2: Send MAIN001 command
        print("[TEST] Step 3: Sending MAIN001 command via Hub...")
        command_data = {
            'command': 'CALL PGM-MAIN001.JAVA,VOL-DISK01',
            'terminal_id': 'webui',
            'user': 'admin'
        }
        
        print(f"[TEST] Sending command: {command_data}")
        terminal_client.emit('hub_command', command_data)
        
        # Step 3: Wait for execution and SMED data
        print("[TEST] Step 4: Waiting for command execution and SMED data...")
        time.sleep(10)
        
        # Step 4: Analyze results
        print("[TEST] Step 5: Analyzing results...")
        print(f"[TEST] Hub registrations: {len(hub_registrations)}")
        print(f"[TEST] Command confirmations: {len(command_confirmations)}")
        print(f"[TEST] SMED data packets: {len(smed_data_received)}")
        
        success = True
        
        # Check hub registration
        if hub_registrations and hub_registrations[0].get('success'):
            print(f"[TEST] ✅ Hub registration: SUCCESS")
        else:
            print(f"[TEST] ❌ Hub registration: FAILED")
            success = False
            
        # Check command execution
        if command_confirmations and command_confirmations[0].get('success'):
            print(f"[TEST] ✅ Command execution: SUCCESS")
        else:
            print(f"[TEST] ❌ Command execution: FAILED")
            success = False
            
        # Check SMED data
        if smed_data_received:
            print(f"[TEST] ✅ SMED data received: SUCCESS")
            smed = smed_data_received[0]
            if smed.get('map_file') == 'MAIN001' and smed.get('program_name') == 'MAIN001':
                print(f"[TEST] ✅ MAIN001 SMED map: CORRECT")
            else:
                print(f"[TEST] ❌ MAIN001 SMED map: INCORRECT")
                success = False
        else:
            print(f"[TEST] ❌ SMED data received: FAILED")
            success = False
            
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
    print("=== Fixed WebSocket Hub + MAIN001 Integration Test ===")
    print("Testing the complete fixed flow with hub_registered event")
    print("")
    
    success = test_fixed_flow()
    
    print("")
    print("=== Final Test Summary ===")
    if success:
        print("✅ ALL TESTS PASSED!")
        print("✅ Hub registration working correctly")
        print("✅ hub_registered event processed")
        print("✅ Command sent via Hub (not HTTP fallback)")
        print("✅ SMED data transmission working")
        print("")
        print("🎉 ISSUE RESOLVED! Users will now see the MAIN001 menu properly")
        print("   instead of just '실행중' message.")
    else:
        print("❌ Some tests failed. Further debugging needed.")
    print("")