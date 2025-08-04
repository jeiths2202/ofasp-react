/**
 * WebSocket Service for OpenASP Terminal Communication
 * Handles MSGSAMPLEBROWSERMENU and other terminal events
 */

export interface EmployeeRecord {
  id: string;
  name: string;
  dept: string;
  salary: string;
  hire_date: string;
  status: string;
}

export interface SMEDMapData {
  type: 'smed_map';
  map_name: string;
  title: string;
  subtitle: string;
  headers: string[];
  page_info: {
    current: number;
    total: number;
  };
  function_keys: string;
  status: string;
  data: EmployeeRecord[];
}

export interface TerminalMessage {
  type: 'terminal_output' | 'smed_map' | 'error' | 'status';
  data: any;
  timestamp?: string;
}

export interface WebSocketEventHandlers {
  onTerminalOutput?: (data: string) => void;
  onSMEDMap?: (data: SMEDMapData) => void;
  onError?: (error: string) => void;
  onStatusUpdate?: (status: string) => void;
  onConnect?: () => void;
  onDisconnect?: () => void;
}

export class WebSocketService {
  private ws: WebSocket | null = null;
  private handlers: WebSocketEventHandlers = {};
  private reconnectAttempts = 0;
  private maxReconnectAttempts = 5;
  private reconnectDelay = 1000;
  private isConnecting = false;
  private terminalRegistered = false;
  private workstationInfo: { username: string; workstation: string; sessionId: string } | null = null;

  constructor(private url: string) {}

  public connect(handlers: WebSocketEventHandlers, workstationInfo?: { username: string; workstation: string; sessionId: string }): Promise<void> {
    return new Promise((resolve, reject) => {
      // Check if already connected
      if (this.isConnected()) {
        console.log('WebSocket already connected, skipping connection attempt');
        resolve();
        return;
      }

      // Check if connection is in progress
      if (this.isConnecting || (this.ws && this.ws.readyState === WebSocket.CONNECTING)) {
        console.log('Connection already in progress, rejecting duplicate attempt');
        reject(new Error('Connection already in progress'));
        return;
      }

      this.isConnecting = true;
      this.handlers = handlers;
      this.workstationInfo = workstationInfo || null;

      console.log('Connecting to WebSocket server:', this.url);

      try {
        this.ws = new WebSocket(this.url);

        this.ws.onopen = () => {
          console.log('WebSocket connected successfully');
          this.isConnecting = false;
          this.reconnectAttempts = 0;
          
          // Register terminal after connection
          if (this.workstationInfo) {
            console.log('Attempting to register terminal with workstation info:', {
              username: this.workstationInfo.username,
              workstation: this.workstationInfo.workstation,
              sessionId: this.workstationInfo.sessionId
            });
            
            this.registerTerminal()
              .then(() => {
                console.log('Terminal registration completed successfully');
                this.handlers.onConnect?.();
                resolve();
              })
              .catch((error) => {
                console.error('Terminal registration failed:', error);
                this.handlers.onError?.(`Terminal registration failed: ${error.message}`);
                reject(error);
              });
          } else {
            console.log('No workstation info provided, skipping terminal registration');
            this.handlers.onConnect?.();
            resolve();
          }
        };

        this.ws.onmessage = (event) => {
          this.handleMessage(event.data);
        };

        this.ws.onerror = (error) => {
          console.error('WebSocket error:', error);
          this.isConnecting = false;
          this.handlers.onError?.('WebSocket connection error');
          reject(error);
        };

        this.ws.onclose = (event) => {
          console.log('WebSocket disconnected:', event.code, event.reason);
          this.isConnecting = false;
          this.terminalRegistered = false;
          this.handlers.onDisconnect?.();
          
          if (!event.wasClean && this.reconnectAttempts < this.maxReconnectAttempts) {
            this.scheduleReconnect();
          }
        };

      } catch (error) {
        this.isConnecting = false;
        reject(error);
      }
    });
  }

  private handleMessage(rawData: string): void {
    try {
      // Try to parse as JSON first
      const message: TerminalMessage = JSON.parse(rawData);
      
      switch (message.type) {
        case 'smed_map':
          if (this.isValidSMEDMapData(message.data)) {
            this.handlers.onSMEDMap?.(message.data as SMEDMapData);
          } else {
            console.warn('Invalid SMED map data received:', message.data);
            this.handlers.onError?.('Invalid SMED map data format');
          }
          break;
          
        case 'terminal_output':
          this.handlers.onTerminalOutput?.(message.data);
          break;
          
        case 'error':
          this.handlers.onError?.(message.data);
          break;
          
        case 'status':
          this.handlers.onStatusUpdate?.(message.data);
          break;
          
        default:
          console.warn('Unknown message type:', message.type);
          // Treat as regular terminal output
          this.handlers.onTerminalOutput?.(rawData);
      }
    } catch (error) {
      // Try to parse as OpenASP JSON response (from Java programs)
      try {
        const openAspResponse = JSON.parse(rawData);
        if (this.isOpenAspDisplayMapResponse(openAspResponse)) {
          const smedData = this.convertOpenAspToSMEDMap(openAspResponse);
          this.handlers.onSMEDMap?.(smedData);
          return;
        }
      } catch (parseError) {
        // Ignore second parse error
      }
      
      // If not JSON, treat as regular terminal output
      this.handlers.onTerminalOutput?.(rawData);
    }
  }

  private isValidSMEDMapData(data: any): boolean {
    return (
      data &&
      typeof data.type === 'string' &&
      typeof data.map_name === 'string' &&
      typeof data.title === 'string' &&
      Array.isArray(data.headers) &&
      Array.isArray(data.data) &&
      data.page_info &&
      typeof data.page_info.current === 'number' &&
      typeof data.page_info.total === 'number'
    );
  }

  private isOpenAspDisplayMapResponse(data: any): boolean {
    return (
      data &&
      data.action === 'display_map' &&
      typeof data.map_file === 'string' &&
      data.fields &&
      typeof data.fields === 'object'
    );
  }

  private convertOpenAspToSMEDMap(openAspData: any): SMEDMapData {
    const fields = openAspData.fields;
    const pageInfo = openAspData.page_info || { current: 1, total: 1, total_records: 0 };
    
    // Extract employee data from fields
    const employeeData: any[] = [];
    const recordsPerPage = 10;
    
    for (let i = 1; i <= recordsPerPage; i++) {
      const empId = fields[`EMP${i}_ID`];
      const empName = fields[`EMP${i}_NAME`];
      const empDept = fields[`EMP${i}_DEPT`];
      const empSalary = fields[`EMP${i}_SALARY`];
      const empHireDate = fields[`EMP${i}_HIREDATE`];
      const empStatus = fields[`EMP${i}_STATUS`];
      
      // Only add non-empty records
      if (empId && empId.trim()) {
        employeeData.push({
          id: empId.trim(),
          name: empName?.trim() || '',
          department: empDept?.trim() || '',
          position: empSalary?.trim() || '', // Using salary field as position
          age: empStatus?.trim() || '', // Using status field as age
          hireDate: empHireDate?.trim() || ''
        });
      }
    }
    
    return {
      type: 'employee_browser' as any,
      map_name: openAspData.map_file || 'BROWSE_MENU',
      title: 'Employee Data Browser',
      subtitle: 'OpenASP Browser',
      headers: ['ID', 'Name', 'Department', 'Position', 'Age', 'Hire Date'],
      data: employeeData,
      page_info: {
        current: pageInfo.current || 1,
        total: pageInfo.total || 1,
        total_records: pageInfo.total_records || employeeData.length,
        records_per_page: pageInfo.records_per_page || 10
      } as any,
      messages: openAspData.messages || [],
      function_keys: openAspData.function_keys || {},
      status: `Page ${pageInfo.current || 1} of ${pageInfo.total || 1}`
    } as SMEDMapData;
  }

  private async registerTerminal(): Promise<void> {
    return new Promise((resolve, reject) => {
      if (!this.ws || this.ws.readyState !== WebSocket.OPEN) {
        const error = new Error('WebSocket not connected for terminal registration');
        console.error('Terminal registration failed:', error.message, 'WebSocket state:', this.ws?.readyState);
        reject(error);
        return;
      }

      if (!this.workstationInfo) {
        const error = new Error('Workstation information not provided for terminal registration');
        console.error('Terminal registration failed:', error.message);
        reject(error);
        return;
      }

      const registrationMessage = {
        type: 'register_terminal',
        data: {
          username: this.workstationInfo.username,
          workstation: this.workstationInfo.workstation,
          sessionId: this.workstationInfo.sessionId,
          timestamp: new Date().toISOString()
        }
      };

      // Set up a timeout for registration response
      const timeout = setTimeout(() => {
        reject(new Error('Terminal registration timeout'));
      }, 10000); // 10 second timeout

      // Set up a temporary message handler for registration response
      const originalHandler = this.ws.onmessage;
      this.ws.onmessage = (event) => {
        try {
          const response = JSON.parse(event.data);
          if (response.type === 'registration_response') {
            clearTimeout(timeout);
            this.ws!.onmessage = originalHandler; // Restore original handler
            
            if (response.data.success) {
              this.terminalRegistered = true;
              console.log('Terminal registered successfully:', response.data.message);
              resolve();
            } else {
              reject(new Error(response.data.message || 'Registration failed'));
            }
            return;
          }
        } catch (error) {
          // Not a JSON message or not a registration response, pass to original handler
        }
        
        // Pass non-registration messages to original handler
        if (originalHandler) {
          originalHandler.call(this.ws, event);
        }
      };

      // Send registration message
      try {
        this.ws.send(JSON.stringify(registrationMessage));
        console.log('Sent terminal registration request:', registrationMessage);
      } catch (sendError) {
        clearTimeout(timeout);
        this.ws!.onmessage = originalHandler;
        console.error('Failed to send terminal registration message:', sendError);
        reject(new Error(`Failed to send registration message: ${sendError}`));
      }
    });
  }

  private scheduleReconnect(): void {
    this.reconnectAttempts++;
    const delay = this.reconnectDelay * Math.pow(2, this.reconnectAttempts - 1);
    
    console.log(`Scheduling reconnect attempt ${this.reconnectAttempts} in ${delay}ms`);
    
    setTimeout(() => {
      if (this.handlers.onConnect) {
        this.connect(this.handlers, this.workstationInfo || undefined).catch(() => {
          // Reconnection failed, will try again if under limit
        });
      }
    }, delay);
  }

  public sendCommand(command: string): void {
    if (!this.isConnected()) {
      console.warn('WebSocket not connected, cannot send command:', command);
      this.handlers.onError?.('WebSocket not connected');
      return;
    }

    if (!this.terminalRegistered) {
      console.warn('Terminal not registered, cannot send command:', command);
      this.handlers.onError?.('Terminal not registered with server');
      return;
    }

    const message = {
      type: 'command',
      data: command,
      workstation: this.workstationInfo?.workstation,
      sessionId: this.workstationInfo?.sessionId,
      timestamp: new Date().toISOString()
    };

    this.ws!.send(JSON.stringify(message));
    console.log('Sent command:', message);
  }

  public sendMSGSampleBrowserMenuCommand(): void {
    if (!this.isConnected()) {
      console.warn('WebSocket not connected, cannot send MSGSAMPLEBROWSERMENU command');
      this.handlers.onError?.('WebSocket not connected');
      return;
    }

    if (!this.terminalRegistered) {
      console.warn('Terminal not registered, cannot send MSGSAMPLEBROWSERMENU command');
      this.handlers.onError?.('Terminal not registered with server');
      return;
    }

    // Try both WebSocket and HTTP API approaches
    this.sendCommand('CALL MSGSAMPLEBROWSERMENU');
    
    // Also try HTTP API if available
    this.trySendViaHttpApi('MSGSAMPLEBROWSERMENU');
  }

  private async trySendViaHttpApi(command: string): Promise<void> {
    if (!this.workstationInfo) return;

    try {
      // Extract port from WebSocket URL and try HTTP API
      const wsUrl = new URL(this.url);
      const httpUrl = `http://${wsUrl.hostname}:3006/api/smed/send/`;
      
      const response = await fetch(httpUrl, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          command: command,
          workstation: this.workstationInfo.workstation,
          username: this.workstationInfo.username,
          sessionId: this.workstationInfo.sessionId,
          timestamp: new Date().toISOString()
        })
      });

      if (response.ok) {
        const result = await response.json();
        console.log('HTTP API response:', result);
        this.handlers.onStatusUpdate?.('Command sent via HTTP API');
      } else {
        console.warn('HTTP API request failed:', response.status, response.statusText);
      }
    } catch (error) {
      console.warn('HTTP API not available:', error);
      // This is not a critical error, WebSocket should handle the command
    }
  }

  public disconnect(): void {
    console.log('Disconnecting WebSocket service...');
    
    if (this.ws) {
      this.ws.close(1000, 'Client disconnect');
      this.ws = null;
    }
    
    // Reset all state
    this.handlers = {};
    this.reconnectAttempts = 0;
    this.terminalRegistered = false;
    this.isConnecting = false;
    this.workstationInfo = null;
    
    console.log('WebSocket service disconnected and cleaned up');
  }

  public isConnected(): boolean {
    return this.ws !== null && this.ws.readyState === WebSocket.OPEN;
  }

  public isTerminalRegistered(): boolean {
    return this.terminalRegistered && this.isConnected();
  }

  public getConnectionState(): string {
    if (!this.ws) return 'DISCONNECTED';
    
    switch (this.ws.readyState) {
      case WebSocket.CONNECTING: return 'CONNECTING';
      case WebSocket.OPEN: return 'CONNECTED';
      case WebSocket.CLOSING: return 'CLOSING';
      case WebSocket.CLOSED: return 'DISCONNECTED';
      default: return 'UNKNOWN';
    }
  }
}

export default WebSocketService;