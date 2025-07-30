"""Main entry point for ASP Manager."""

import sys
import curses
import argparse
import logging
from .core import AspManager
from .config import config


def parse_args():
    """Parse command line arguments."""
    parser = argparse.ArgumentParser(
        description='ASP Manager - System Management Tool',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  aspmgr                    # Start in interactive mode
  aspmgr --version          # Show version
  aspmgr --check-config     # Validate configuration
        """
    )
    
    parser.add_argument(
        '--version', '-v',
        action='version',
        version=f'ASP Manager {config.version}'
    )
    
    parser.add_argument(
        '--check-config',
        action='store_true',
        help='Check configuration and exit'
    )
    
    parser.add_argument(
        '--debug',
        action='store_true',
        help='Enable debug logging'
    )
    
    return parser.parse_args()


def check_config():
    """Check configuration validity."""
    print("Checking configuration...")
    
    errors = config.validate()
    if errors:
        print("\nConfiguration errors found:")
        for error in errors:
            print(f"  - {error}")
        return False
    else:
        print("Configuration is valid.")
        return True


def main(stdscr):
    """Main application entry point."""
    try:
        # Create and run the application
        app = AspManager(stdscr)
        app.run()
        
    except ValueError as e:
        # Terminal size error
        curses.endwin()
        print(f"\nError: {e}", file=sys.stderr)
        sys.exit(1)
        
    except KeyboardInterrupt:
        # Clean exit on Ctrl+C
        pass
        
    except Exception as e:
        # Unexpected error
        curses.endwin()
        print(f"\nUnexpected error: {e}", file=sys.stderr)
        
        if '--debug' in sys.argv:
            import traceback
            traceback.print_exc()
            
        sys.exit(1)


def run():
    """Run the application."""
    args = parse_args()
    
    # Set up logging
    if args.debug:
        logging.basicConfig(level=logging.DEBUG)
    else:
        logging.basicConfig(level=logging.WARNING)
        
    # Check config if requested
    if args.check_config:
        if check_config():
            sys.exit(0)
        else:
            sys.exit(1)
            
    # Run the application
    try:
        curses.wrapper(main)
    except Exception as e:
        print(f"\nFailed to start application: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == '__main__':
    run()