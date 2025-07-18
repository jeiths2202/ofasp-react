#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Main entry point for Python conversion service
Production-ready service with no hardcoding
"""

import sys
from pathlib import Path

# Add project root to Python path
sys.path.insert(0, str(Path(__file__).parent))

if __name__ == '__main__':
    try:
        from src.api.app import api
        api.run()
    except KeyboardInterrupt:
        print("\nService stopped by user")
        sys.exit(0)
    except Exception as e:
        print(f"Failed to start service: {e}")
        sys.exit(1)