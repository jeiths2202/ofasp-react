#!/usr/bin/env python3
"""
OpenASP Manager - CUI-based management tool for OpenASP
"""

import sys
import os
import cmd
import readline
from datetime import datetime

__version__ = "1.0.0"

class ASPManager(cmd.Cmd):
    """OpenASP Manager command line interface"""
    
    intro = """
    ╔══════════════════════════════════════════════════════════════════╗
    ║                      OpenASP Manager v1.0.0                      ║
    ║                                                                  ║
    ║  Type 'help' for available commands or 'help <command>' for     ║
    ║  detailed information about a specific command.                  ║
    ║                                                                  ║
    ║  Type 'quit' or 'exit' to leave the manager.                    ║
    ╚══════════════════════════════════════════════════════════════════╝
    """
    
    prompt = 'aspmgr> '
    
    def __init__(self):
        super().__init__()
        self.startup_time = datetime.now()
        
    def do_help(self, arg):
        """Show available commands or help for a specific command
        
        Usage: help [command]
        
        Examples:
            help        - Show all available commands
            help quit   - Show help for the quit command
        """
        if arg:
            # Show help for specific command
            try:
                func = getattr(self, 'help_' + arg)
                func()
            except AttributeError:
                try:
                    doc = getattr(self, 'do_' + arg).__doc__
                    if doc:
                        print(doc)
                    else:
                        print(f"No help available for command: {arg}")
                except AttributeError:
                    print(f"Unknown command: {arg}")
        else:
            # Show general help
            print("\nAvailable commands:")
            print("=" * 50)
            commands = [
                ("help [command]", "Show help information"),
                ("quit/exit", "Exit the ASP Manager"),
            ]
            
            for cmd, desc in commands:
                print(f"  {cmd:<20} - {desc}")
            print("\nFor detailed help on a specific command, type: help <command>")
            print("=" * 50)
    
    def do_quit(self, arg):
        """Exit the ASP Manager
        
        Usage: quit
        
        This command terminates the ASP Manager session and returns
        to the system shell.
        """
        print("\nExiting OpenASP Manager...")
        uptime = datetime.now() - self.startup_time
        print(f"Session duration: {uptime}")
        print("Goodbye!")
        return True
    
    def do_exit(self, arg):
        """Exit the ASP Manager (alias for quit)
        
        Usage: exit
        
        This command terminates the ASP Manager session and returns
        to the system shell.
        """
        return self.do_quit(arg)
    
    def help_quit(self):
        """Show detailed help for quit command"""
        print("\nQUIT - Exit the ASP Manager")
        print("-" * 40)
        print("Usage: quit")
        print("\nDescription:")
        print("  Terminates the current ASP Manager session and returns")
        print("  to the system shell. All unsaved changes will be lost.")
        print("\nAliases: exit")
        print("-" * 40)
    
    def help_exit(self):
        """Show detailed help for exit command"""
        self.help_quit()
    
    def emptyline(self):
        """Do nothing on empty line"""
        pass
    
    def default(self, line):
        """Handle unknown commands"""
        print(f"Unknown command: {line}")
        print("Type 'help' for a list of available commands.")
    
    def do_EOF(self, arg):
        """Handle Ctrl-D"""
        print()  # New line after ^D
        return self.do_quit(arg)
    
    def cmdloop(self, intro=None):
        """Override cmdloop to handle KeyboardInterrupt"""
        while True:
            try:
                super().cmdloop(intro)
                break
            except KeyboardInterrupt:
                print("\nUse 'quit' or 'exit' to leave the manager.")
                intro = None  # Don't show intro again
                continue

def main():
    """Main entry point for aspmgr"""
    try:
        # Initialize and run the manager
        manager = ASPManager()
        manager.cmdloop()
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    main()