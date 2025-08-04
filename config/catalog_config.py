#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Centralized catalog.json configuration management
Environment variable based configuration - no hardcoding
"""

import os
from pathlib import Path

# Environment-based configuration
CONFIG_ROOT = os.getenv('OPENAP_CONFIG_ROOT', '/home/aspuser/app/config')
CATALOG_FILENAME = os.getenv('CATALOG_FILENAME', 'catalog.json')

def get_catalog_path():
    """
    Get catalog.json path from environment variables
    Returns absolute path to the centralized catalog.json
    """
    catalog_path = Path(CONFIG_ROOT) / CATALOG_FILENAME
    
    # Ensure config directory exists
    catalog_path.parent.mkdir(parents=True, exist_ok=True)
    
    return str(catalog_path)

def get_config_root():
    """Get configuration root directory from environment"""
    return CONFIG_ROOT

# Export commonly used paths
CATALOG_FILE = get_catalog_path()