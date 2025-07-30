# ASP Manager - Curses-based System Management Tool

## Overview

ASP Manager is a professional curses-based system management tool designed as a completely new implementation that provides comprehensive system monitoring and management capabilities through a modern terminal user interface.

## Features

### Core Functionality
- **System Dashboard**: Real-time system monitoring with CPU, memory, and disk usage
- **Process Management**: Comprehensive process listing and monitoring
- **Log Viewer**: System log monitoring with filtering capabilities
- **Configuration Management**: Environment-based configuration system
- **Help System**: Comprehensive help and navigation assistance

### Technical Features
- **Professional UI**: Unicode-supported terminal interface with multiple color schemes
- **Responsive Design**: Adaptive layout that works across different terminal sizes
- **Configuration-Driven**: Zero hardcoding with full environment variable support
- **Extensible Architecture**: Modular design ready for plugins and extensions
- **Cross-Platform**: Works on Linux/Unix systems with curses support

## Installation

### Prerequisites
- Python 3.7 or higher
- Linux/Unix system with curses support
- Terminal with minimum 80x24 characters

### Setup
1. Ensure Python 3.7+ is installed
2. Navigate to the aspmgr directory
3. Run the application: `python3 run_aspmgr.py`

## Configuration

ASP Manager uses environment variables for all configuration. Set these variables before running:

### Terminal Configuration
```bash
export ASPMGR_MIN_WIDTH=80              # Minimum terminal width
export ASPMGR_MIN_HEIGHT=24             # Minimum terminal height
export ASPMGR_COLOR_SCHEME=default      # UI color scheme (default/dark/light/blue/green)
export ASPMGR_USE_UNICODE=true          # Enable Unicode characters
export ASPMGR_MOUSE_SUPPORT=true        # Enable mouse support
export ASPMGR_REFRESH_INTERVAL=1.0      # Screen refresh interval in seconds
```

### System Monitoring
```bash
export ASPMGR_LOG_DIR=/var/log/aspmgr           # Log file directory
export ASPMGR_PROCESS_SCAN_INTERVAL=2.0        # Process scanning interval
export ASPMGR_MEMORY_THRESHOLD=80.0            # Memory usage alert threshold
export ASPMGR_CPU_THRESHOLD=80.0               # CPU usage alert threshold
export ASPMGR_DISK_THRESHOLD=90.0              # Disk usage alert threshold
```

### Application Settings
```bash
export ASPMGR_TITLE="ASP Manager"               # Application title
export ASPMGR_VERSION="1.0.0"                  # Version display
export ASPMGR_DEBUG=false                      # Enable debug mode
export ASPMGR_DEMO_MODE=false                  # Enable demo mode
```

## Usage

### Starting the Application
```bash
python3 run_aspmgr.py
```

### Navigation
- **Arrow Keys**: Navigate menus and content
- **Page Up/Down**: Scroll through content
- **Home/End**: Jump to top/bottom of content
- **Tab**: Switch between interface panels
- **Enter**: Select menu items

### Function Keys
- **F1**: Display help screen
- **F2**: Force refresh system data
- **F3**: Exit application
- **ESC**: Exit application

### Menu Options
1. **Dashboard**: System overview with real-time metrics
2. **Processes**: Process list with CPU and memory usage
3. **Logs**: System log viewer (demo mode)
4. **Config**: Configuration display and settings
5. **Help**: Navigation help and key bindings

## Architecture

### Component Structure
```
aspmgr/
├── config.py          # Configuration management
├── aspmgr.py          # Main application logic
├── ui_base.py         # UI base components
├── system_monitor.py  # System monitoring utilities
└── __init__.py        # Package initialization
```

### Key Components
- **ConfigManager**: Handles all configuration loading and validation
- **ASPManager**: Main application controller and UI coordinator
- **SystemMonitor**: System information collection and monitoring
- **BaseWindow**: Base UI component with common functionality
- **ScrollableWindow**: Scrollable content display
- **MenuWindow**: Menu navigation component

## Testing

Run the test suite to verify functionality:
```bash
python3 test_aspmgr.py
```

### Test Coverage
- Configuration loading and validation
- System monitoring functionality
- UI component initialization
- Data formatting and processing
- Error handling and edge cases

## Color Schemes

ASP Manager supports multiple color schemes:

- **default**: Standard terminal colors
- **dark**: Dark theme with muted colors
- **light**: Light theme for bright terminals
- **blue**: Blue-themed interface
- **green**: Green-themed interface

Change schemes using: `export ASPMGR_COLOR_SCHEME=dark`

## Troubleshooting

### Common Issues

1. **Terminal Size Error**
   - Ensure terminal is at least 80x24 characters
   - Resize terminal and restart application

2. **Curses Initialization Error**
   - Check terminal supports curses
   - Verify terminal environment variables are set

3. **Permission Errors**
   - Ensure read access to /proc filesystem
   - Check log directory permissions

4. **Unicode Display Issues**
   - Disable Unicode: `export ASPMGR_USE_UNICODE=false`
   - Check terminal Unicode support

### Debug Mode
Enable debug logging:
```bash
export ASPMGR_DEBUG=true
python3 run_aspmgr.py
```

Debug logs are written to `/tmp/aspmgr.log`.

### Demo Mode
For testing without system access:
```bash
export ASPMGR_DEMO_MODE=true
python3 run_aspmgr.py
```

## Development

### Adding New Features
1. Follow the modular architecture pattern
2. Add configuration options to `config.py`
3. Implement UI components extending `BaseWindow`
4. Add corresponding tests to `test_aspmgr.py`

### Code Standards
- Follow CODING_RULES.md guidelines
- Use type hints for all functions
- Implement proper error handling
- Add comprehensive documentation

## Security

ASP Manager follows security best practices:
- No hardcoded credentials or paths
- Safe file system access patterns
- Input validation and sanitization
- Proper error handling without information disclosure

## Performance

- Efficient system monitoring with configurable intervals
- Lazy loading of system information
- Minimal memory footprint
- Optimized screen refresh cycles

## License

Copyright (C) 2025 ASP Manager Development Team

This software is provided as-is for system management purposes.

## Support

For issues and questions:
1. Check the troubleshooting section
2. Review configuration settings
3. Enable debug mode for detailed logging
4. Check system requirements and permissions

---

*This implementation provides a professional system management tool with a completely different architecture and design from legacy systems, following modern software engineering practices.*