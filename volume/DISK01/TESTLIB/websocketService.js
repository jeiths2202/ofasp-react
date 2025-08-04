"use strict";
/**
 * WebSocket Service for OpenASP Terminal Communication
 * Handles MSGSAMPLEBROWSERMENU and other terminal events
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.WebSocketService = void 0;
class WebSocketService {
    constructor(url) {
        this.url = url;
        this.ws = null;
        this.handlers = {};
        this.reconnectAttempts = 0;
        this.maxReconnectAttempts = 5;
        this.reconnectDelay = 1000;
        this.isConnecting = false;
        this.terminalRegistered = false;
        this.workstationInfo = null;
    }
    connect(handlers, workstationInfo) {
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
                    var _a, _b;
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
                            var _a, _b;
                            console.log('Terminal registration completed successfully');
                            (_b = (_a = this.handlers).onConnect) === null || _b === void 0 ? void 0 : _b.call(_a);
                            resolve();
                        })
                            .catch((error) => {
                            var _a, _b;
                            console.error('Terminal registration failed:', error);
                            (_b = (_a = this.handlers).onError) === null || _b === void 0 ? void 0 : _b.call(_a, `Terminal registration failed: ${error.message}`);
                            reject(error);
                        });
                    }
                    else {
                        console.log('No workstation info provided, skipping terminal registration');
                        (_b = (_a = this.handlers).onConnect) === null || _b === void 0 ? void 0 : _b.call(_a);
                        resolve();
                    }
                };
                this.ws.onmessage = (event) => {
                    this.handleMessage(event.data);
                };
                this.ws.onerror = (error) => {
                    var _a, _b;
                    console.error('WebSocket error:', error);
                    this.isConnecting = false;
                    (_b = (_a = this.handlers).onError) === null || _b === void 0 ? void 0 : _b.call(_a, 'WebSocket connection error');
                    reject(error);
                };
                this.ws.onclose = (event) => {
                    var _a, _b;
                    console.log('WebSocket disconnected:', event.code, event.reason);
                    this.isConnecting = false;
                    this.terminalRegistered = false;
                    (_b = (_a = this.handlers).onDisconnect) === null || _b === void 0 ? void 0 : _b.call(_a);
                    if (!event.wasClean && this.reconnectAttempts < this.maxReconnectAttempts) {
                        this.scheduleReconnect();
                    }
                };
            }
            catch (error) {
                this.isConnecting = false;
                reject(error);
            }
        });
    }
    handleMessage(rawData) {
        var _a, _b, _c, _d, _e, _f, _g, _h, _j, _k, _l, _m, _o, _p, _q, _r;
        try {
            // Try to parse as JSON first
            const message = JSON.parse(rawData);
            switch (message.type) {
                case 'smed_map':
                    if (this.isValidSMEDMapData(message.data)) {
                        (_b = (_a = this.handlers).onSMEDMap) === null || _b === void 0 ? void 0 : _b.call(_a, message.data);
                    }
                    else {
                        console.warn('Invalid SMED map data received:', message.data);
                        (_d = (_c = this.handlers).onError) === null || _d === void 0 ? void 0 : _d.call(_c, 'Invalid SMED map data format');
                    }
                    break;
                case 'terminal_output':
                    (_f = (_e = this.handlers).onTerminalOutput) === null || _f === void 0 ? void 0 : _f.call(_e, message.data);
                    break;
                case 'error':
                    (_h = (_g = this.handlers).onError) === null || _h === void 0 ? void 0 : _h.call(_g, message.data);
                    break;
                case 'status':
                    (_k = (_j = this.handlers).onStatusUpdate) === null || _k === void 0 ? void 0 : _k.call(_j, message.data);
                    break;
                default:
                    console.warn('Unknown message type:', message.type);
                    // Treat as regular terminal output
                    (_m = (_l = this.handlers).onTerminalOutput) === null || _m === void 0 ? void 0 : _m.call(_l, rawData);
            }
        }
        catch (error) {
            // Try to parse as OpenASP JSON response (from Java programs)
            try {
                const openAspResponse = JSON.parse(rawData);
                if (this.isOpenAspDisplayMapResponse(openAspResponse)) {
                    const smedData = this.convertOpenAspToSMEDMap(openAspResponse);
                    (_p = (_o = this.handlers).onSMEDMap) === null || _p === void 0 ? void 0 : _p.call(_o, smedData);
                    return;
                }
            }
            catch (parseError) {
                // Ignore second parse error
            }
            // If not JSON, treat as regular terminal output
            (_r = (_q = this.handlers).onTerminalOutput) === null || _r === void 0 ? void 0 : _r.call(_q, rawData);
        }
    }
    isValidSMEDMapData(data) {
        return (data &&
            typeof data.type === 'string' &&
            typeof data.map_name === 'string' &&
            typeof data.title === 'string' &&
            Array.isArray(data.headers) &&
            Array.isArray(data.data) &&
            data.page_info &&
            typeof data.page_info.current === 'number' &&
            typeof data.page_info.total === 'number');
    }
    isOpenAspDisplayMapResponse(data) {
        return (data &&
            data.action === 'display_map' &&
            typeof data.map_file === 'string' &&
            data.fields &&
            typeof data.fields === 'object');
    }
    convertOpenAspToSMEDMap(openAspData) {
        const fields = openAspData.fields;
        const pageInfo = openAspData.page_info || { current: 1, total: 1, total_records: 0 };
        // Extract employee data from fields
        const employeeData = [];
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
                    name: (empName === null || empName === void 0 ? void 0 : empName.trim()) || '',
                    department: (empDept === null || empDept === void 0 ? void 0 : empDept.trim()) || '',
                    position: (empSalary === null || empSalary === void 0 ? void 0 : empSalary.trim()) || '',
                    age: (empStatus === null || empStatus === void 0 ? void 0 : empStatus.trim()) || '',
                    hireDate: (empHireDate === null || empHireDate === void 0 ? void 0 : empHireDate.trim()) || ''
                });
            }
        }
        return {
            type: 'employee_browser',
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
            },
            messages: openAspData.messages || [],
            function_keys: openAspData.function_keys || {},
            status: `Page ${pageInfo.current || 1} of ${pageInfo.total || 1}`
        };
    }
    async registerTerminal() {
        return new Promise((resolve, reject) => {
            var _a;
            if (!this.ws || this.ws.readyState !== WebSocket.OPEN) {
                const error = new Error('WebSocket not connected for terminal registration');
                console.error('Terminal registration failed:', error.message, 'WebSocket state:', (_a = this.ws) === null || _a === void 0 ? void 0 : _a.readyState);
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
                        this.ws.onmessage = originalHandler; // Restore original handler
                        if (response.data.success) {
                            this.terminalRegistered = true;
                            console.log('Terminal registered successfully:', response.data.message);
                            resolve();
                        }
                        else {
                            reject(new Error(response.data.message || 'Registration failed'));
                        }
                        return;
                    }
                }
                catch (error) {
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
            }
            catch (sendError) {
                clearTimeout(timeout);
                this.ws.onmessage = originalHandler;
                console.error('Failed to send terminal registration message:', sendError);
                reject(new Error(`Failed to send registration message: ${sendError}`));
            }
        });
    }
    scheduleReconnect() {
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
    sendCommand(command) {
        var _a, _b, _c, _d, _e, _f;
        if (!this.isConnected()) {
            console.warn('WebSocket not connected, cannot send command:', command);
            (_b = (_a = this.handlers).onError) === null || _b === void 0 ? void 0 : _b.call(_a, 'WebSocket not connected');
            return;
        }
        if (!this.terminalRegistered) {
            console.warn('Terminal not registered, cannot send command:', command);
            (_d = (_c = this.handlers).onError) === null || _d === void 0 ? void 0 : _d.call(_c, 'Terminal not registered with server');
            return;
        }
        const message = {
            type: 'command',
            data: command,
            workstation: (_e = this.workstationInfo) === null || _e === void 0 ? void 0 : _e.workstation,
            sessionId: (_f = this.workstationInfo) === null || _f === void 0 ? void 0 : _f.sessionId,
            timestamp: new Date().toISOString()
        };
        this.ws.send(JSON.stringify(message));
        console.log('Sent command:', message);
    }
    sendMSGSampleBrowserMenuCommand() {
        var _a, _b, _c, _d;
        if (!this.isConnected()) {
            console.warn('WebSocket not connected, cannot send MSGSAMPLEBROWSERMENU command');
            (_b = (_a = this.handlers).onError) === null || _b === void 0 ? void 0 : _b.call(_a, 'WebSocket not connected');
            return;
        }
        if (!this.terminalRegistered) {
            console.warn('Terminal not registered, cannot send MSGSAMPLEBROWSERMENU command');
            (_d = (_c = this.handlers).onError) === null || _d === void 0 ? void 0 : _d.call(_c, 'Terminal not registered with server');
            return;
        }
        // Try both WebSocket and HTTP API approaches
        this.sendCommand('CALL MSGSAMPLEBROWSERMENU');
        // Also try HTTP API if available
        this.trySendViaHttpApi('MSGSAMPLEBROWSERMENU');
    }
    async trySendViaHttpApi(command) {
        var _a, _b;
        if (!this.workstationInfo)
            return;
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
                (_b = (_a = this.handlers).onStatusUpdate) === null || _b === void 0 ? void 0 : _b.call(_a, 'Command sent via HTTP API');
            }
            else {
                console.warn('HTTP API request failed:', response.status, response.statusText);
            }
        }
        catch (error) {
            console.warn('HTTP API not available:', error);
            // This is not a critical error, WebSocket should handle the command
        }
    }
    disconnect() {
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
    isConnected() {
        return this.ws !== null && this.ws.readyState === WebSocket.OPEN;
    }
    isTerminalRegistered() {
        return this.terminalRegistered && this.isConnected();
    }
    getConnectionState() {
        if (!this.ws)
            return 'DISCONNECTED';
        switch (this.ws.readyState) {
            case WebSocket.CONNECTING: return 'CONNECTING';
            case WebSocket.OPEN: return 'CONNECTED';
            case WebSocket.CLOSING: return 'CLOSING';
            case WebSocket.CLOSED: return 'DISCONNECTED';
            default: return 'UNKNOWN';
        }
    }
}
exports.WebSocketService = WebSocketService;
exports.default = WebSocketService;
