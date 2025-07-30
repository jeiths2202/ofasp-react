import { io, Socket } from 'socket.io-client';
import { SmedDisplayData, TerminalRegistrationData, WebSocketEventData } from '../types/workstation';

class WebSocketService {
  private socket: Socket | null = null;
  private listeners: Map<string, Set<Function>> = new Map();
  private connected: boolean = false;
  private terminalId: string = 'webui';
  private sessionId: string | null = null;
  private wsname: string = 'WSNAME00';
  private user: string = 'unknown';
  private reconnectAttempts: number = 0;
  private maxReconnectAttempts: number = 10;
  private reconnectDelay: number = 1000;
  private maxReconnectDelay: number = 30000;
  private connectionHealth: boolean = true;
  private heartbeatInterval: NodeJS.Timeout | null = null;

  connect(apiUrl: string = 'http://localhost:3006') {
    if (this.socket?.connected) {
      console.log('WebSocket already connected');
      return;
    }

    console.log('Connecting to WebSocket server:', apiUrl);
    
    const currentDelay = Math.min(
      this.reconnectDelay * Math.pow(2, this.reconnectAttempts),
      this.maxReconnectDelay
    );
    
    this.socket = io(apiUrl, {
      transports: ['websocket', 'polling'],
      reconnection: true,
      reconnectionAttempts: this.maxReconnectAttempts,
      reconnectionDelay: currentDelay,
      timeout: 10000,
      forceNew: true,
    });

    this.setupEventHandlers();
    this.startHeartbeat();
  }

  private startHeartbeat() {
    if (this.heartbeatInterval) {
      clearInterval(this.heartbeatInterval);
    }
    
    this.heartbeatInterval = setInterval(() => {
      if (this.socket?.connected) {
        this.socket.emit('ping');
        this.connectionHealth = true;
      }
    }, 30000); // 30초마다 heartbeat
  }

  private stopHeartbeat() {
    if (this.heartbeatInterval) {
      clearInterval(this.heartbeatInterval);
      this.heartbeatInterval = null;
    }
  }

  private setupEventHandlers() {
    if (!this.socket) return;

    this.socket.on('connect', () => {
      console.log('WebSocket connected');
      this.connected = true;
      this.reconnectAttempts = 0; // 성공적으로 연결되면 재시도 카운터 리셋
      this.connectionHealth = true;
      this.emit('connected', { connected: true });
    });

    this.socket.on('disconnect', (reason) => {
      console.log('WebSocket disconnected:', reason);
      this.connected = false;
      this.connectionHealth = false;
      this.stopHeartbeat();
      this.emit('disconnected', { connected: false, reason });
    });

    this.socket.on('connect_error', (error) => {
      console.error('WebSocket connection error:', error);
      this.reconnectAttempts++;
      this.connectionHealth = false;
      this.emit('connection_error', { error, attempts: this.reconnectAttempts });
    });

    this.socket.on('pong', () => {
      this.connectionHealth = true;
    });

    this.socket.on('connected', (data: { session_id: string }) => {
      console.log('Server acknowledged connection:', data);
      this.sessionId = data.session_id;
      this.emit('session_established', data);
    });

    this.socket.on('terminal_registered', (data: TerminalRegistrationData) => {
      console.log('Terminal registered:', data);
      this.wsname = data.wsname;
      this.emit('terminal_registered', data);
    });

    this.socket.on('registration_error', (data: { error: string }) => {
      console.error('Terminal registration error:', data.error);
      this.emit('registration_error', data);
    });

    this.socket.on('workstation_status_changed', (data: WebSocketEventData) => {
      console.log('Workstation status changed:', data);
      this.emit('workstation_status_changed', data);
    });

    this.socket.on('workstation_disabled', (data: { wsname: string }) => {
      console.log('Workstation disabled:', data);
      this.emit('workstation_disabled', data);
    });

    this.socket.on('smed_display', (data: SmedDisplayData) => {
      console.log('SMED display data received:', data);
      this.emit('smed_display', data);
    });

    this.socket.on('error', (error: any) => {
      console.error('WebSocket error:', error);
      this.emit('error', error);
    });
  }

  registerTerminal(terminalId: string, user: string, wsname: string = 'WSNAME00') {
    if (!this.socket?.connected) {
      console.error('Cannot register terminal - not connected');
      return false;
    }

    this.terminalId = terminalId;
    this.user = user;
    this.wsname = wsname;
    console.log('Registering terminal:', terminalId, 'for user:', user, 'on workstation:', wsname);
    
    this.socket.emit('register_terminal', {
      terminal_id: terminalId,
      user: user,
      wsname: wsname
    });

    return true;
  }

  sendKeyEvent(key: string, fieldValues: Record<string, string>) {
    if (!this.socket?.connected) {
      console.error('Cannot send key event - not connected');
      return;
    }

    this.socket.emit('smed_key_event', {
      terminal_id: this.terminalId,
      session_id: this.sessionId,
      key: key,
      field_values: fieldValues
    });
  }

  sendTerminalOutput(data: string, type: string = 'text') {
    if (!this.socket?.connected) {
      console.error('Cannot send terminal output - not connected');
      return;
    }

    this.socket.emit('terminal_output', {
      data: data,
      type: type
    });
  }

  on(event: string, callback: Function) {
    if (!this.listeners.has(event)) {
      this.listeners.set(event, new Set());
    }
    this.listeners.get(event)!.add(callback);
  }

  off(event: string, callback: Function) {
    if (this.listeners.has(event)) {
      this.listeners.get(event)!.delete(callback);
    }
  }

  private emit(event: string, data: any) {
    if (this.listeners.has(event)) {
      this.listeners.get(event)!.forEach(callback => {
        try {
          callback(data);
        } catch (error) {
          console.error(`Error in ${event} listener:`, error);
        }
      });
    }
  }

  disconnect() {
    this.stopHeartbeat();
    if (this.socket) {
      this.socket.disconnect();
      this.socket = null;
      this.connected = false;
      this.sessionId = null;
      this.reconnectAttempts = 0;
    }
  }

  getConnectionHealth(): boolean {
    return this.connectionHealth;
  }

  getReconnectAttempts(): number {
    return this.reconnectAttempts;
  }

  forceReconnect(apiUrl: string = 'http://localhost:3006') {
    console.log('Forcing WebSocket reconnection...');
    this.disconnect();
    setTimeout(() => {
      this.connect(apiUrl);
    }, 1000);
  }

  isConnected(): boolean {
    return this.connected;
  }

  getSessionId(): string | null {
    return this.sessionId;
  }

  getTerminalId(): string {
    return this.terminalId;
  }

  getWorkstationName(): string {
    return this.wsname;
  }

  getUser(): string {
    return this.user;
  }

  // Workstation management API methods
  async getWorkstationStatus(wsname: string): Promise<any> {
    try {
      const response = await fetch(`http://localhost:3006/api/workstation/status/${wsname}`);
      
      if (!response.ok) {
        const errorData = await response.json().catch(() => ({ error: 'Unknown error' }));
        throw new Error(`HTTP ${response.status}: ${errorData.error || response.statusText}`);
      }
      
      return await response.json();
    } catch (error) {
      console.error('Error fetching workstation status:', error);
      throw error;
    }
  }

  async registerWorkstation(wsname: string, status: string = 'OFF'): Promise<any> {
    try {
      const response = await fetch('http://localhost:3006/api/workstation/register', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({ wsname, status }),
      });
      
      if (!response.ok) {
        const errorData = await response.json().catch(() => ({ error: 'Unknown error' }));
        throw new Error(`HTTP ${response.status}: ${errorData.error || response.statusText}`);
      }
      
      return await response.json();
    } catch (error) {
      console.error('Error registering workstation:', error);
      throw error;
    }
  }

  async updateWorkstationStatus(wsname: string, status: string): Promise<any> {
    try {
      const response = await fetch(`http://localhost:3006/api/workstation/status/${wsname}`, {
        method: 'PUT',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({ status }),
      });
      
      if (!response.ok) {
        const errorData = await response.json().catch(() => ({ error: 'Unknown error' }));
        throw new Error(`HTTP ${response.status}: ${errorData.error || response.statusText}`);
      }
      
      return await response.json();
    } catch (error) {
      console.error('Error updating workstation status:', error);
      throw error;
    }
  }

  async listWorkstations(): Promise<any> {
    try {
      const response = await fetch('http://localhost:3006/api/workstation/list');
      
      if (!response.ok) {
        const errorData = await response.json().catch(() => ({ error: 'Unknown error' }));
        throw new Error(`HTTP ${response.status}: ${errorData.error || response.statusText}`);
      }
      
      return await response.json();
    } catch (error) {
      console.error('Error listing workstations:', error);
      throw error;
    }
  }

  async sendSmedData(wsname: string, smedData: any): Promise<any> {
    try {
      const response = await fetch(`http://localhost:3006/api/smed/send/${wsname}`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(smedData),
      });
      
      if (!response.ok) {
        const errorData = await response.json().catch(() => ({ error: 'Unknown error' }));
        throw new Error(`HTTP ${response.status}: ${errorData.error || response.statusText}`);
      }
      
      return await response.json();
    } catch (error) {
      console.error('Error sending SMED data:', error);
      throw error;
    }
  }
}

// Create singleton instance
const webSocketService = new WebSocketService();

export default webSocketService;