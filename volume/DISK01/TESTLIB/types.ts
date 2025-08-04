/**
 * Type definitions for OpenASP Web Terminal
 * Defines interfaces and types used throughout the application
 */

// Employee Record Types
export interface EmployeeRecord {
  id: string;
  name: string;
  dept: string;
  salary: string;
  hire_date: string;
  status: string;
}

// SMED Map Data Structure
export interface SMEDMapData {
  type: 'smed_map' | 'employee_browser';
  map_name: string;
  title: string;
  subtitle?: string;
  headers: string[];
  page_info: {
    current: number;
    total: number;
    total_records?: number;
    records_per_page?: number;
  };
  function_keys: any;
  status?: string;
  messages?: string[];
  data: any[];
}

// WebSocket Message Types
export interface TerminalMessage {
  type: 'terminal_output' | 'smed_map' | 'error' | 'status' | 'command';
  data: any;
  timestamp?: string;
}

export interface CommandMessage extends TerminalMessage {
  type: 'command';
  data: string;
}

export interface SMEDMapMessage extends TerminalMessage {
  type: 'smed_map';
  data: SMEDMapData;
}

export interface ErrorMessage extends TerminalMessage {
  type: 'error';
  data: string;
}

export interface StatusMessage extends TerminalMessage {
  type: 'status';
  data: string;
}

export interface TerminalOutputMessage extends TerminalMessage {
  type: 'terminal_output';
  data: string;
}

// Authentication Types
export interface AuthCredentials {
  username: string;
  workstation: string;
  password: string;
}

export interface AuthState {
  isAuthenticated: boolean;
  username: string;
  workstation: string;
  sessionId: string;
  loginTime?: Date;
}

// Terminal Line Types
export interface TerminalLine {
  id: string;
  content: string;
  timestamp: Date;
  type: 'input' | 'output' | 'error' | 'system';
}

// WebSocket Event Handler Types
export interface WebSocketEventHandlers {
  onTerminalOutput?: (data: string) => void;
  onSMEDMap?: (data: SMEDMapData) => void;
  onError?: (error: string) => void;
  onStatusUpdate?: (status: string) => void;
  onConnect?: () => void;
  onDisconnect?: () => void;
}

// Connection States
export type ConnectionState = 'CONNECTING' | 'CONNECTED' | 'DISCONNECTED' | 'CLOSING' | 'UNKNOWN';

// Component Props Types
export interface EmployeeDataViewerProps {
  smedData: SMEDMapData | null;
  isVisible: boolean;
  onClose?: () => void;
}

export interface AspCliWebTerminalProps {
  websocketUrl: string;
  onConnectionChange?: (connected: boolean) => void;
  workstationInfo?: {
    username: string;
    workstation: string;
    sessionId: string;
  };
}

export interface WorkstationAuthWrapperProps {
  websocketUrl?: string;
  title?: string;
  subtitle?: string;
}

// Application Configuration
export interface AppConfig {
  websocketUrl: string;
  title: string;
  subtitle: string;
  maxReconnectAttempts?: number;
  reconnectDelay?: number;
  terminalBufferSize?: number;
}

// Error Types
export interface TerminalError {
  code: string;
  message: string;
  timestamp: Date;
  context?: string;
}

// Session Information
export interface SessionInfo {
  sessionId: string;
  username: string;
  workstation: string;
  startTime: Date;
  lastActivity: Date;
  duration: string;
}

// Quick Command Types
export interface QuickCommand {
  id: string;
  label: string;
  command: string;
  description?: string;
  category?: 'system' | 'data' | 'utility';
}

// Default Quick Commands
export const DEFAULT_QUICK_COMMANDS: QuickCommand[] = [
  {
    id: 'employee-browser',
    label: 'Employee Browser',
    command: 'MSGSAMPLEBROWSERMENU',
    description: 'Browse employee records from SAM file',
    category: 'data'
  },
  {
    id: 'help',
    label: 'Help',
    command: 'HELP',
    description: 'Show available commands',
    category: 'system'
  },
  {
    id: 'status',
    label: 'Status',
    command: 'STATUS',
    description: 'Show system status',
    category: 'system'
  }
];

// Utility Types
export type Nullable<T> = T | null;
export type Optional<T> = T | undefined;

// Event Types
export type TerminalEventType = 'connect' | 'disconnect' | 'message' | 'error' | 'command';

export interface TerminalEvent<T = any> {
  type: TerminalEventType;
  payload: T;
  timestamp: Date;
}

// Theme Colors (for future theming support)
export interface TerminalTheme {
  background: string;
  foreground: string;
  accent: string;
  error: string;
  success: string;
  warning: string;
  info: string;
  border: string;
}

export const DEFAULT_TERMINAL_THEME: TerminalTheme = {
  background: '#0c0c0c',
  foreground: '#00ff00',
  accent: '#00ffff',
  error: '#ff6060',
  success: '#00ff00',
  warning: '#ffff00',
  info: '#00ffff',
  border: '#333333'
};

// Export utility functions
export const isValidEmployeeRecord = (record: any): record is EmployeeRecord => {
  return (
    record &&
    typeof record.id === 'string' &&
    typeof record.name === 'string' &&
    typeof record.dept === 'string' &&
    typeof record.salary === 'string' &&
    typeof record.hire_date === 'string' &&
    typeof record.status === 'string'
  );
};

export const isValidSMEDMapData = (data: any): data is SMEDMapData => {
  return (
    data &&
    data.type === 'smed_map' &&
    typeof data.map_name === 'string' &&
    typeof data.title === 'string' &&
    Array.isArray(data.headers) &&
    Array.isArray(data.data) &&
    data.page_info &&
    typeof data.page_info.current === 'number' &&
    typeof data.page_info.total === 'number'
  );
};