# ASP Manager - Curses-based System Management Tool

A professional curses-based system management tool designed for Unix/Linux systems. ASP Manager provides a comprehensive interface for system monitoring, process management, configuration editing, and performance analysis.

## Features

- **System Status Overview**: Real-time monitoring of CPU, memory, disk usage, and system alerts
- **Process Management**: View, control, and monitor system processes with detailed information
- **Configuration Editor**: Edit system and application configuration files
- **Log Viewer**: Real-time log monitoring with filtering and search capabilities
- **Performance Metrics**: Historical performance data visualization
- **Professional UI**: Modern terminal user interface with color support and mouse interaction
- **Extensible Architecture**: Modular design allowing for easy feature additions

## Design Philosophy

ASP Manager is built with a completely different architecture from traditional system management tools:

- **Component-based UI**: Modular window and panel system
- **Event-driven architecture**: Asynchronous updates and real-time monitoring
- **Configuration-driven**: All settings externalized through environment variables
- **Modern Python practices**: Type hints, dataclasses, and clean code structure
- **Professional appearance**: Consistent color schemes and intuitive navigation

## Installation

### Prerequisites

- Python 3.7 or higher
- Unix/Linux system
- Terminal with curses support

### Quick Start

1. Clone or download the ASP Manager source code
2. Install dependencies:
   ```bash
   pip install -r requirements.txt
   ```
3. Run the application:
   ```bash
   python run_aspmgr.py
   ```

### Alternative Installation

You can also install ASP Manager as a package:

```bash
pip install -e .
aspmgr
```

## Configuration

ASP Manager is configured entirely through environment variables, following the zero-hardcoding principle:

### Core Settings

```bash
# Application Settings
ASPMGR_APP_NAME="ASP Manager"
ASPMGR_VERSION="1.0.0"
ASPMGR_ENV="production"

# UI Configuration
ASPMGR_MIN_WIDTH=80
ASPMGR_MIN_HEIGHT=24
ASPMGR_COLOR_SCHEME=default
ASPMGR_ENABLE_MOUSE=true
ASPMGR_SIDEBAR_WIDTH_PERCENT=25

# System Configuration
ASPMGR_LOG_DIR=/var/log/aspmgr
ASPMGR_CONFIG_DIR=/etc/aspmgr
ASPMGR_TEMP_DIR=/tmp/aspmgr
ASPMGR_ENABLE_LOGGING=true
ASPMGR_ENABLE_METRICS=true

# Performance Settings
ASPMGR_STATUS_REFRESH_MS=5000
ASPMGR_METRICS_REFRESH_MS=3000
ASPMGR_PROCESS_CHECK_INTERVAL=2
```

### Security Settings

```bash
# Authentication
ASPMGR_AUTH_TIMEOUT=300
ASPMGR_MAX_LOGIN_ATTEMPTS=3

# Permissions
ASPMGR_ENABLE_PERMISSION_CHECK=true
ASPMGR_ADMIN_ONLY_FEATURES=shutdown,config_edit
```

## Usage

### Navigation

- **Arrow Keys**: Navigate menus and lists
- **Enter**: Select items or confirm actions
- **Tab**: Switch between panels
- **ESC**: Go back or cancel
- **Page Up/Down**: Scroll through long lists
- **F1**: Help
- **F2**: Refresh current view
- **F5**: Tools menu
- **F10**: Exit

### Main Features

#### System Status
- Real-time CPU, memory, and disk usage
- System uptime and process count
- Alert notifications for resource thresholds

#### Process Manager
- Comprehensive process listing with PID, name, status, and resource usage
- Process control (start, stop, restart)
- Process monitoring and alerting
- Sortable columns

#### Log Viewer
- Real-time log monitoring
- Filter by log level or search text
- Follow mode for continuous updates
- Multiple log file support

#### Performance Metrics
- Historical performance graphs
- CPU, memory, disk I/O, and network monitoring
- Configurable time ranges
- Threshold alerts

## Architecture

ASP Manager follows a modular, component-based architecture:

```
aspmgr/
├── config/          # Configuration management
├── constants/       # Application constants
├── core/           # Main application logic
├── ui/             # User interface components
│   ├── base.py     # Base UI classes
│   ├── dialogs.py  # Dialog windows
│   └── panels.py   # Main panels
├── utils/          # Utility modules
│   ├── system.py   # System monitoring
│   ├── process.py  # Process management
│   └── logger.py   # Logging utilities
└── data/           # Configuration data
```

### Key Components

- **AspManager**: Main application controller
- **Window/Panel System**: Modular UI components
- **SystemMonitor**: Real-time system monitoring
- **ProcessManager**: Process control and monitoring
- **Logger**: Comprehensive logging system

## Development

### Running Tests

```bash
python test_aspmgr.py
```

### Development Mode

```bash
python run_aspmgr.py --debug --demo
```

### Configuration Validation

```bash
python run_aspmgr.py --check-config
```

## Comparison with Traditional Tools

ASP Manager offers several advantages over traditional system management tools:

### Unique Features

- **Modern UI**: Professional curses-based interface with consistent styling
- **Real-time Updates**: Asynchronous monitoring without blocking the UI
- **Configuration Management**: Zero-hardcoding with environment-based configuration
- **Extensible Design**: Plugin-ready architecture for custom features
- **Professional Appearance**: Color-coded status indicators and intuitive navigation

### Technical Advantages

- **Memory Efficient**: Minimal resource usage with smart caching
- **Responsive**: Non-blocking UI with background processing
- **Portable**: Pure Python implementation with minimal dependencies
- **Maintainable**: Clean code structure with comprehensive documentation

## Environment Variables Reference

| Variable | Default | Description |
|----------|---------|-------------|
| `ASPMGR_MIN_WIDTH` | 80 | Minimum terminal width |
| `ASPMGR_MIN_HEIGHT` | 24 | Minimum terminal height |
| `ASPMGR_COLOR_SCHEME` | default | Color scheme (default, dark, light) |
| `ASPMGR_ENABLE_MOUSE` | true | Enable mouse support |
| `ASPMGR_SIDEBAR_WIDTH_PERCENT` | 25 | Sidebar width percentage |
| `ASPMGR_LOG_DIR` | /var/log/aspmgr | Log file directory |
| `ASPMGR_CONFIG_DIR` | /etc/aspmgr | Configuration directory |
| `ASPMGR_TEMP_DIR` | /tmp/aspmgr | Temporary files directory |
| `ASPMGR_ENABLE_LOGGING` | true | Enable file logging |
| `ASPMGR_ENABLE_METRICS` | true | Enable metrics collection |
| `ASPMGR_STATUS_REFRESH_MS` | 5000 | Status refresh interval |
| `ASPMGR_METRICS_REFRESH_MS` | 3000 | Metrics refresh interval |

## Troubleshooting

### Common Issues

1. **Terminal too small**: Ensure terminal is at least 80x24
2. **Permission denied**: Check file permissions for log/config directories
3. **Missing dependencies**: Install required packages with `pip install -r requirements.txt`
4. **Color issues**: Try different color schemes via `ASPMGR_COLOR_SCHEME`

### Debug Mode

Enable debug mode for detailed error information:

```bash
python run_aspmgr.py --debug
```

## License

MIT License - See LICENSE file for details.

## Contributing

1. Fork the repository
2. Create a feature branch
3. Follow the coding standards in `CODING_RULES.md`
4. Run tests with `python test_aspmgr.py`
5. Submit a pull request

## Support

For issues and questions, please create an issue in the project repository.