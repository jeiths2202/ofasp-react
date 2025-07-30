#!/usr/bin/env python3
"""Launch script for ASP Manager."""

import os
import sys
import signal
import argparse
from pathlib import Path

# Add the aspmgr module to path
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

# Set up environment variables with defaults
if 'ASPMGR_LOG_DIR' not in os.environ:
    os.environ['ASPMGR_LOG_DIR'] = '/tmp/aspmgr_logs'

if 'ASPMGR_CONFIG_DIR' not in os.environ:
    os.environ['ASPMGR_CONFIG_DIR'] = os.path.join(os.path.dirname(__file__), 'config')

if 'ASPMGR_TEMP_DIR' not in os.environ:
    os.environ['ASPMGR_TEMP_DIR'] = '/tmp/aspmgr'

# Create directories if they don't exist
for dir_env in ['ASPMGR_LOG_DIR', 'ASPMGR_CONFIG_DIR', 'ASPMGR_TEMP_DIR']:
    dir_path = os.environ[dir_env]
    Path(dir_path).mkdir(parents=True, exist_ok=True)

def signal_handler(signum, frame):
    """Handle interrupt signals."""
    print("\nReceived signal, exiting...")
    sys.exit(0)

def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description='ASP Manager - Professional System Management Tool',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python run_aspmgr.py                    # Start ASP Manager
  python run_aspmgr.py --demo             # Run in demo mode
  python run_aspmgr.py --version          # Show version
  python run_aspmgr.py --check-config     # Check configuration
        """
    )
    
    parser.add_argument('--version', action='version', version='ASP Manager 1.0.0')
    parser.add_argument('--demo', action='store_true', help='Run in demo mode')
    parser.add_argument('--check-config', action='store_true', help='Check configuration')
    parser.add_argument('--debug', action='store_true', help='Enable debug mode')
    
    args = parser.parse_args()
    
    # Set up signal handlers
    signal.signal(signal.SIGINT, signal_handler)
    signal.signal(signal.SIGTERM, signal_handler)
    
    # Check if running in demo mode
    if args.demo:
        print("Running ASP Manager in demo mode...")
        os.environ['ASPMGR_ENABLE_LOGGING'] = 'false'
        os.environ['ASPMGR_ENABLE_METRICS'] = 'false'
    
    # Enable debug mode if requested
    if args.debug:
        os.environ['ASPMGR_DEBUG'] = 'true'
    
    # Check configuration
    if args.check_config:
        try:
            from aspmgr.config import config
            errors = config.validate()
            if errors:
                print("Configuration errors found:")
                for error in errors:
                    print(f"  - {error}")
                return 1
            else:
                print("Configuration is valid.")
                return 0
        except Exception as e:
            print(f"Error checking configuration: {e}")
            return 1
    
    # Run the application
    try:
        print("Starting ASP Manager...")
        print(f"Log directory: {os.environ.get('ASPMGR_LOG_DIR')}")
        print(f"Config directory: {os.environ.get('ASPMGR_CONFIG_DIR')}")
        print("Press Ctrl+C to exit\n")
        
        # Import and run
        from aspmgr.__main__ import run
        run()
        
    except KeyboardInterrupt:
        print("\nExiting ASP Manager...")
        return 0
    except Exception as e:
        print(f"Error starting ASP Manager: {e}")
        if args.debug:
            import traceback
            traceback.print_exc()
        return 1

if __name__ == '__main__':
    sys.exit(main())