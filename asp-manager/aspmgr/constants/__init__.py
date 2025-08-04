"""Constants module for ASP Manager CUI application."""

from enum import Enum, IntEnum


class Colors(IntEnum):
    """Color pair definitions for curses."""
    DEFAULT = 1
    HEADER = 2
    FOOTER = 3
    MENU = 4
    MENU_SELECTED = 5
    STATUS_OK = 6
    STATUS_WARNING = 7
    STATUS_ERROR = 8
    INFO = 9
    HIGHLIGHT = 10
    BORDER = 11
    DIALOG = 12
    INPUT = 13
    BUTTON = 14
    BUTTON_SELECTED = 15


class Keys(IntEnum):
    """Special key codes."""
    TAB = 9
    ENTER = 10
    ESC = 27
    SPACE = 32
    UP = 259
    DOWN = 258
    LEFT = 260
    RIGHT = 261
    PAGE_UP = 339
    PAGE_DOWN = 338
    HOME = 262
    END = 360
    F1 = 265
    F2 = 266
    F3 = 267
    F4 = 268
    F5 = 269
    F6 = 270
    F7 = 271
    F8 = 272
    F9 = 273
    F10 = 274
    F11 = 275
    F12 = 276


class MenuItems(Enum):
    """Main menu items."""
    SYSTEM_STATUS = "System Status"
    PROCESS_MANAGER = "Process Manager"
    CONFIGURATION = "Configuration"
    LOG_VIEWER = "Log Viewer"
    PERFORMANCE = "Performance Metrics"
    TOOLS = "Tools"
    HELP = "Help"
    EXIT = "Exit"


class ProcessStatus(Enum):
    """Process status values."""
    RUNNING = "Running"
    STOPPED = "Stopped"
    STARTING = "Starting"
    STOPPING = "Stopping"
    ERROR = "Error"
    UNKNOWN = "Unknown"


class LogLevel(Enum):
    """Log level definitions."""
    DEBUG = "DEBUG"
    INFO = "INFO"
    WARNING = "WARNING"
    ERROR = "ERROR"
    CRITICAL = "CRITICAL"


class Borders:
    """Border characters for windows."""
    # Single line borders
    SINGLE_HORIZONTAL = '─'
    SINGLE_VERTICAL = '│'
    SINGLE_TOP_LEFT = '┌'
    SINGLE_TOP_RIGHT = '┐'
    SINGLE_BOTTOM_LEFT = '└'
    SINGLE_BOTTOM_RIGHT = '┘'
    SINGLE_CROSS = '┼'
    SINGLE_T_LEFT = '├'
    SINGLE_T_RIGHT = '┤'
    SINGLE_T_TOP = '┬'
    SINGLE_T_BOTTOM = '┴'
    
    # Double line borders
    DOUBLE_HORIZONTAL = '═'
    DOUBLE_VERTICAL = '║'
    DOUBLE_TOP_LEFT = '╔'
    DOUBLE_TOP_RIGHT = '╗'
    DOUBLE_BOTTOM_LEFT = '╚'
    DOUBLE_BOTTOM_RIGHT = '╝'


class Symbols:
    """Special symbols for UI elements."""
    ARROW_RIGHT = '▶'
    ARROW_LEFT = '◀'
    ARROW_UP = '▲'
    ARROW_DOWN = '▼'
    CHECK = '✓'
    CROSS = '✗'
    BULLET = '•'
    STAR = '★'
    EMPTY_STAR = '☆'
    PROGRESS_FULL = '█'
    PROGRESS_EMPTY = '░'
    
    
class Limits:
    """System limits and thresholds."""
    MAX_LOG_LINES = 10000
    MAX_HISTORY_ITEMS = 100
    MAX_SEARCH_RESULTS = 500
    MAX_PROCESS_NAME_LENGTH = 50
    MAX_PATH_LENGTH = 255
    MIN_PASSWORD_LENGTH = 8
    SESSION_TIMEOUT = 3600  # 1 hour
    
    # Performance thresholds
    CPU_WARNING_THRESHOLD = 80.0
    CPU_CRITICAL_THRESHOLD = 95.0
    MEMORY_WARNING_THRESHOLD = 85.0
    MEMORY_CRITICAL_THRESHOLD = 95.0
    DISK_WARNING_THRESHOLD = 80.0
    DISK_CRITICAL_THRESHOLD = 90.0


class FileTypes:
    """Supported file types and extensions."""
    CONFIG_EXTENSIONS = ['.conf', '.cfg', '.ini', '.json', '.yaml', '.yml']
    LOG_EXTENSIONS = ['.log', '.txt', '.out', '.err']
    SCRIPT_EXTENSIONS = ['.sh', '.bash', '.py', '.pl', '.rb']
    DATA_EXTENSIONS = ['.csv', '.dat', '.xml', '.json']


class Messages:
    """User interface messages."""
    # Status messages
    LOADING = "Loading..."
    READY = "Ready"
    PROCESSING = "Processing..."
    COMPLETED = "Completed"
    FAILED = "Failed"
    
    # Prompts
    CONFIRM_EXIT = "Are you sure you want to exit? (Y/N)"
    CONFIRM_DELETE = "Are you sure you want to delete? (Y/N)"
    CONFIRM_RESTART = "Are you sure you want to restart? (Y/N)"
    ENTER_COMMAND = "Enter command:"
    ENTER_SEARCH = "Search:"
    
    # Errors
    ERROR_PERMISSION_DENIED = "Permission denied"
    ERROR_FILE_NOT_FOUND = "File not found"
    ERROR_INVALID_INPUT = "Invalid input"
    ERROR_CONNECTION_FAILED = "Connection failed"
    ERROR_TIMEOUT = "Operation timed out"
    
    # Info
    INFO_NO_DATA = "No data available"
    INFO_HELP_KEY = "Press F1 for help"
    INFO_ESC_TO_EXIT = "Press ESC to go back"