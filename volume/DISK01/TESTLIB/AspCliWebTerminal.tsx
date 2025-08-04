import React, { useState, useEffect, useRef, useCallback } from 'react';
import { WebSocketService, SMEDMapData, WebSocketEventHandlers } from './websocketService';
import EmployeeDataViewer from './EmployeeDataViewer';

interface AspCliWebTerminalProps {
  websocketUrl: string;
  onConnectionChange?: (connected: boolean) => void;
  workstationInfo?: {
    username: string;
    workstation: string;
    sessionId: string;
  };
}

interface TerminalLine {
  id: string;
  content: string;
  timestamp: Date;
  type: 'input' | 'output' | 'error' | 'system';
}

const AspCliWebTerminal: React.FC<AspCliWebTerminalProps> = ({
  websocketUrl,
  onConnectionChange,
  workstationInfo
}) => {
  const [wsService] = useState(() => new WebSocketService(websocketUrl));
  const [isConnected, setIsConnected] = useState(false);
  const [currentInput, setCurrentInput] = useState('');
  const [terminalLines, setTerminalLines] = useState<TerminalLine[]>([]);
  const [smedData, setSmedData] = useState<SMEDMapData | null>(null);
  const [showEmployeeViewer, setShowEmployeeViewer] = useState(false);
  const [connectionStatus, setConnectionStatus] = useState('DISCONNECTED');
  const [isConnecting, setIsConnecting] = useState(false);
  const [hasAutoConnected, setHasAutoConnected] = useState(false);

  const terminalRef = useRef<HTMLDivElement>(null);
  const inputRef = useRef<HTMLInputElement>(null);
  const lineIdCounter = useRef(0);

  const addTerminalLine = useCallback((content: string, type: TerminalLine['type'] = 'output') => {
    const newLine: TerminalLine = {
      id: `line-${++lineIdCounter.current}`,
      content,
      timestamp: new Date(),
      type
    };
    
    setTerminalLines(prev => [...prev, newLine]);
  }, []);

  const scrollToBottom = useCallback(() => {
    if (terminalRef.current) {
      terminalRef.current.scrollTop = terminalRef.current.scrollHeight;
    }
  }, []);

  useEffect(() => {
    scrollToBottom();
  }, [terminalLines, scrollToBottom]);

  // Auto-connect when workstationInfo is provided (but only once to prevent React StrictMode issues)
  useEffect(() => {
    if (workstationInfo && !hasAutoConnected && !isConnected && !isConnecting) {
      setHasAutoConnected(true);
      handleConnect();
    }
  }, [workstationInfo, hasAutoConnected, isConnected, isConnecting, handleConnect]);

  // Cleanup on unmount
  useEffect(() => {
    return () => {
      if (wsService.isConnected()) {
        wsService.disconnect();
      }
    };
  }, [wsService]);

  const handleConnect = useCallback(async () => {
    // Prevent duplicate connection attempts
    if (isConnecting || isConnected) {
      console.log('Connection attempt blocked: already connecting or connected');
      return;
    }
    
    setIsConnecting(true);
    setConnectionStatus('CONNECTING');
    addTerminalLine('Connecting to OpenASP Terminal...', 'system');

    const handlers: WebSocketEventHandlers = {
      onConnect: () => {
        console.log('WebSocket connection established');
        setIsConnected(true);
        setConnectionStatus('CONNECTED');
        setIsConnecting(false);
        addTerminalLine('Connected to OpenASP Terminal', 'system');
        if (workstationInfo) {
          addTerminalLine(`Terminal registered for ${workstationInfo.username}@${workstationInfo.workstation}`, 'system');
        }
        onConnectionChange?.(true);
      },
      
      onDisconnect: () => {
        console.log('WebSocket disconnected');
        setIsConnected(false);
        setConnectionStatus('DISCONNECTED');
        setIsConnecting(false);
        addTerminalLine('Disconnected from OpenASP Terminal', 'system');
        onConnectionChange?.(false);
      },
      
      onTerminalOutput: (data: string) => {
        addTerminalLine(data, 'output');
      },
      
      onSMEDMap: (data: SMEDMapData) => {
        setSmedData(data);
        setShowEmployeeViewer(true);
        addTerminalLine(`SMED Map received: ${data.map_name} (${data.data.length} records)`, 'system');
      },
      
      onError: (error: string) => {
        addTerminalLine(`Error: ${error}`, 'error');
        // Don't change connection status for terminal registration errors
        if (!error.includes('Terminal not registered')) {
          setIsConnecting(false);
        }
      },
      
      onStatusUpdate: (status: string) => {
        addTerminalLine(`Status: ${status}`, 'system');
      }
    };

    try {
      await wsService.connect(handlers, workstationInfo);
    } catch (error) {
      console.error('Connection failed:', error);
      setIsConnecting(false);
      setConnectionStatus('DISCONNECTED');
      const errorMessage = error instanceof Error ? error.message : 'Unknown connection error';
      addTerminalLine(`Connection failed: ${errorMessage}`, 'error');
    }
  }, [isConnecting, isConnected, wsService, addTerminalLine, onConnectionChange, workstationInfo]);

  const handleDisconnect = useCallback(() => {
    console.log('User initiated disconnect');
    wsService.disconnect();
    setIsConnected(false);
    setConnectionStatus('DISCONNECTED');
    setIsConnecting(false);
    setHasAutoConnected(false); // Reset auto-connect flag to allow reconnection
    addTerminalLine('Disconnected by user', 'system');
    onConnectionChange?.(false);
  }, [wsService, addTerminalLine, onConnectionChange]);

  const handleSendCommand = useCallback(() => {
    if (!currentInput.trim() || !isConnected) return;

    const command = currentInput.trim();
    
    // Handle clear command locally
    if (command.toUpperCase() === 'CLEAR' || command.toUpperCase() === 'CLS') {
      setTerminalLines([]);
      setCurrentInput('');
      addTerminalLine('Screen cleared', 'system');
      return;
    }
    
    addTerminalLine(`> ${command}`, 'input');
    
    // Check if terminal is properly registered
    if (!wsService.isTerminalRegistered()) {
      addTerminalLine('Terminal not registered with server. Please reconnect.', 'error');
      return;
    }
    
    // Check for MSGSAMPLEBROWSERMENU command
    if (command.toUpperCase().includes('MSGSAMPLEBROWSERMENU') || 
        command.toUpperCase().includes('CALL MSGSAMPLEBROWSERMENU')) {
      wsService.sendMSGSampleBrowserMenuCommand();
    } else {
      wsService.sendCommand(command);
    }
    
    setCurrentInput('');
  }, [currentInput, isConnected, wsService, addTerminalLine]);

  const handleKeyPress = useCallback((event: React.KeyboardEvent) => {
    if (event.key === 'Enter') {
      handleSendCommand();
    }
  }, [handleSendCommand]);

  const handleInputChange = useCallback((event: React.ChangeEvent<HTMLInputElement>) => {
    setCurrentInput(event.target.value);
  }, []);

  const handleCloseEmployeeViewer = useCallback(() => {
    setShowEmployeeViewer(false);
    setSmedData(null);
  }, []);

  const executeQuickCommand = useCallback((command: string) => {
    if (!isConnected) {
      addTerminalLine('Not connected to terminal', 'error');
      return;
    }
    
    if (!wsService.isTerminalRegistered()) {
      addTerminalLine('Terminal not registered with server. Please reconnect.', 'error');
      return;
    }
    
    addTerminalLine(`> ${command}`, 'input');
    
    if (command === 'MSGSAMPLEBROWSERMENU') {
      wsService.sendMSGSampleBrowserMenuCommand();
    } else {
      wsService.sendCommand(command);
    }
  }, [isConnected, wsService, addTerminalLine]);

  const getStatusColor = (status: string): string => {
    switch (status) {
      case 'CONNECTED': return '#00ff00';
      case 'CONNECTING': return '#ffff00';
      case 'DISCONNECTED': return '#ff6060';
      default: return '#666';
    }
  };

  const formatTimestamp = (timestamp: Date): string => {
    return timestamp.toLocaleTimeString();
  };

  return (
    <div className="asp-cli-terminal" style={{
      display: 'flex',
      flexDirection: 'column',
      height: '100vh',
      backgroundColor: '#0c0c0c',
      color: '#00ff00',
      fontFamily: 'Consolas, Monaco, "Courier New", monospace',
      fontSize: '14px'
    }}>
      {/* Header Bar */}
      <div className="terminal-header" style={{
        display: 'flex',
        justifyContent: 'space-between',
        alignItems: 'center',
        padding: '10px 15px',
        backgroundColor: '#1a1a1a',
        borderBottom: '2px solid #333'
      }}>
        <div style={{ display: 'flex', alignItems: 'center', gap: '15px' }}>
          <h3 style={{ margin: 0, color: '#00ffff' }}>OpenASP Web Terminal</h3>
          <div style={{
            display: 'flex',
            alignItems: 'center',
            gap: '5px',
            fontSize: '12px'
          }}>
            <div style={{
              width: '8px',
              height: '8px',
              borderRadius: '50%',
              backgroundColor: getStatusColor(connectionStatus)
            }}></div>
            <span style={{ color: getStatusColor(connectionStatus) }}>
              {connectionStatus}
            </span>
          </div>
        </div>
        
        <div style={{ display: 'flex', gap: '10px' }}>
          {!isConnected ? (
            <button
              onClick={handleConnect}
              disabled={isConnecting}
              style={{
                padding: '5px 15px',
                backgroundColor: isConnecting ? '#666' : '#006600',
                color: 'white',
                border: 'none',
                borderRadius: '4px',
                cursor: isConnecting ? 'not-allowed' : 'pointer',
                fontSize: '12px'
              }}
            >
              {isConnecting ? 'Connecting...' : 'Connect'}
            </button>
          ) : (
            <button
              onClick={handleDisconnect}
              style={{
                padding: '5px 15px',
                backgroundColor: '#cc3300',
                color: 'white',
                border: 'none',
                borderRadius: '4px',
                cursor: 'pointer',
                fontSize: '12px'
              }}
            >
              Disconnect
            </button>
          )}
        </div>
      </div>

      {/* Quick Commands Bar */}
      {isConnected && (
        <div className="quick-commands" style={{
          padding: '8px 15px',
          backgroundColor: '#2a2a2a',
          borderBottom: '1px solid #333',
          display: 'flex',
          gap: '10px',
          flexWrap: 'wrap'
        }}>
          <span style={{ fontSize: '12px', color: '#999' }}>Quick Commands:</span>
          <button
            onClick={() => executeQuickCommand('MSGSAMPLEBROWSERMENU')}
            style={{
              padding: '3px 8px',
              backgroundColor: '#004400',
              color: '#00ff00',
              border: '1px solid #006600',
              borderRadius: '3px',
              cursor: 'pointer',
              fontSize: '11px'
            }}
          >
            Employee Browser
          </button>
          <button
            onClick={() => executeQuickCommand('HELP')}
            style={{
              padding: '3px 8px',
              backgroundColor: '#004444',
              color: '#00ffff',
              border: '1px solid #006666',
              borderRadius: '3px',
              cursor: 'pointer',
              fontSize: '11px'
            }}
          >
            Help
          </button>
          <button
            onClick={() => {
              setTerminalLines([]);
              addTerminalLine('Screen cleared', 'system');
            }}
            style={{
              padding: '3px 8px',
              backgroundColor: '#442200',
              color: '#ffaa00',
              border: '1px solid #664400',
              borderRadius: '3px',
              cursor: 'pointer',
              fontSize: '11px'
            }}
          >
            Clear
          </button>
        </div>
      )}

      {/* Terminal Output Area */}
      <div
        ref={terminalRef}
        className="terminal-output"
        style={{
          flex: 1,
          padding: '15px',
          overflowY: 'auto',
          backgroundColor: '#0c0c0c',
          lineHeight: '1.4'
        }}
      >
        {terminalLines.map((line) => (
          <div
            key={line.id}
            style={{
              marginBottom: '2px',
              display: 'flex',
              alignItems: 'flex-start',
              gap: '10px'
            }}
          >
            <span style={{
              fontSize: '10px',
              color: '#666',
              minWidth: '60px',
              flexShrink: 0
            }}>
              {formatTimestamp(line.timestamp)}
            </span>
            <span style={{
              color: line.type === 'input' ? '#ffff00' :
                     line.type === 'error' ? '#ff6060' :
                     line.type === 'system' ? '#00ffff' : '#00ff00',
              wordBreak: 'break-word'
            }}>
              {line.content}
            </span>
          </div>
        ))}
        
        {terminalLines.length === 0 && (
          <div style={{ color: '#666', fontStyle: 'italic' }}>
            Welcome to OpenASP Web Terminal. Click "Connect" to start.
          </div>
        )}
      </div>

      {/* Input Area */}
      <div className="terminal-input" style={{
        display: 'flex',
        alignItems: 'center',
        padding: '10px 15px',
        backgroundColor: '#1a1a1a',
        borderTop: '1px solid #333'
      }}>
        <span style={{ marginRight: '10px', color: '#ffff00' }}>{'>'}</span>
        <input
          ref={inputRef}
          type="text"
          value={currentInput}
          onChange={handleInputChange}
          onKeyPress={handleKeyPress}
          disabled={!isConnected}
          placeholder={isConnected ? "Enter command..." : "Connect to start typing"}
          style={{
            flex: 1,
            padding: '8px',
            backgroundColor: '#0c0c0c',
            color: '#00ff00',
            border: '1px solid #333',
            borderRadius: '4px',
            fontSize: '14px',
            fontFamily: 'inherit'
          }}
        />
        <button
          onClick={handleSendCommand}
          disabled={!isConnected || !currentInput.trim()}
          style={{
            marginLeft: '10px',
            padding: '8px 15px',
            backgroundColor: isConnected && currentInput.trim() ? '#006600' : '#666',
            color: 'white',
            border: 'none',
            borderRadius: '4px',
            cursor: isConnected && currentInput.trim() ? 'pointer' : 'not-allowed',
            fontSize: '12px'
          }}
        >
          Send
        </button>
      </div>

      {/* Employee Data Viewer Overlay */}
      <EmployeeDataViewer
        smedData={smedData}
        isVisible={showEmployeeViewer}
        onClose={handleCloseEmployeeViewer}
      />
    </div>
  );
};

export default AspCliWebTerminal;