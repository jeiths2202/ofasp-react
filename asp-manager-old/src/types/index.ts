// Authentication types
export interface LoginFormData {
  username: string;
  password: string;
}

export interface User {
  id: string;
  username: string;
  role: string;
  name?: string;
}

export interface AuthState {
  isAuthenticated: boolean;
  user: User | null;
  token?: string;
}

// Menu and Navigation types
export interface MenuItem {
  id: string;
  label: string;
  icon: React.ReactNode;
  path: string;
  children?: MenuItem[];
  badge?: number;
}

export interface Tab {
  id: string;
  label: string;
  component: React.ComponentType;
  closable: boolean;
  data?: any;
}

// API Response types
export interface ApiResponse<T = any> {
  success: boolean;
  data?: T;
  message?: string;
  error?: string;
}

// System Info types
export interface SystemInfo {
  version: string;
  uptime: string;
  javaAvailable: boolean;
  smedMapsCount: number;
  accountsCount: number;
}

// SMED Map types
export interface SmedMap {
  name: string;
  path: string;
  fields: SmedField[];
  lastModified: string;
}

export interface SmedField {
  name: string;
  type: string;
  position: {
    row: number;
    col: number;
  };
  length: number;
  prompt: string;
  color: string;
}

// Program types
export interface ProgramConfig {
  [key: string]: {
    TYPE: string;
    PGM: string;
    DESCRIPTION: string;
  };
}

// Account types
export interface Account {
  id: string;
  password: string;
  name?: string;
  role?: string;
  pgm?: string;
  lastLogin?: string;
}

export interface AccountsData {
  [key: string]: Omit<Account, 'id'>;
}