import React, { useState, useEffect, useCallback } from 'react';
import AspCliWebTerminal from './AspCliWebTerminal';

interface WorkstationAuthWrapperProps {
  websocketUrl?: string;
  title?: string;
  subtitle?: string;
}

interface AuthState {
  isAuthenticated: boolean;
  username: string;
  workstation: string;
  sessionId: string;
}

const WorkstationAuthWrapper: React.FC<WorkstationAuthWrapperProps> = ({
  websocketUrl = 'ws://localhost:3006/terminal',
  title = 'OpenASP Workstation',
  subtitle = 'Secure Terminal Access'
}) => {
  const [authState, setAuthState] = useState<AuthState>({
    isAuthenticated: false,
    username: '',
    workstation: '',
    sessionId: ''
  });
  
  const [loginForm, setLoginForm] = useState({
    username: '',
    workstation: '',
    password: ''
  });
  
  const [isConnected, setIsConnected] = useState(false);
  const [loginError, setLoginError] = useState('');
  const [isLoading, setIsLoading] = useState(false);

  // Generate session ID
  const generateSessionId = useCallback((): string => {
    return 'sess_' + Math.random().toString(36).substr(2, 16) + '_' + Date.now();
  }, []);

  // Handle authentication
  const handleLogin = useCallback(async (event: React.FormEvent) => {
    event.preventDefault();
    
    if (!loginForm.username.trim() || !loginForm.workstation.trim() || !loginForm.password.trim()) {
      setLoginError('All fields are required');
      return;
    }
    
    setIsLoading(true);
    setLoginError('');
    
    try {
      // Simulate authentication process
      await new Promise(resolve => setTimeout(resolve, 1000));
      
      // For demo purposes, accept any non-empty credentials
      // In production, this would make an actual authentication request
      const sessionId = generateSessionId();
      
      setAuthState({
        isAuthenticated: true,
        username: loginForm.username,
        workstation: loginForm.workstation,
        sessionId
      });
      
      // Clear the form
      setLoginForm({ username: '', workstation: '', password: '' });
      
    } catch (error) {
      setLoginError('Authentication failed. Please try again.');
    } finally {
      setIsLoading(false);
    }
  }, [loginForm, generateSessionId]);

  // Handle logout
  const handleLogout = useCallback(() => {
    setAuthState({
      isAuthenticated: false,
      username: '',
      workstation: '',
      sessionId: ''
    });
    setIsConnected(false);
    setLoginForm({ username: '', workstation: '', password: '' });
  }, []);

  // Handle form input changes
  const handleInputChange = useCallback((field: keyof typeof loginForm) => 
    (event: React.ChangeEvent<HTMLInputElement>) => {
      setLoginForm(prev => ({
        ...prev,
        [field]: event.target.value
      }));
      // Clear error when user starts typing
      if (loginError) setLoginError('');
    }, [loginError]);

  // Handle terminal connection change
  const handleConnectionChange = useCallback((connected: boolean) => {
    setIsConnected(connected);
  }, []);

  // Format session duration
  const [sessionDuration, setSessionDuration] = useState('00:00:00');
  const [sessionStartTime] = useState(new Date());

  useEffect(() => {
    if (!authState.isAuthenticated) return;

    const interval = setInterval(() => {
      const now = new Date();
      const diff = now.getTime() - sessionStartTime.getTime();
      const hours = Math.floor(diff / (1000 * 60 * 60));
      const minutes = Math.floor((diff % (1000 * 60 * 60)) / (1000 * 60));
      const seconds = Math.floor((diff % (1000 * 60)) / 1000);
      
      setSessionDuration(
        `${hours.toString().padStart(2, '0')}:${minutes.toString().padStart(2, '0')}:${seconds.toString().padStart(2, '0')}`
      );
    }, 1000);

    return () => clearInterval(interval);
  }, [authState.isAuthenticated, sessionStartTime]);

  // Render login form
  if (!authState.isAuthenticated) {
    return (
      <div className="workstation-auth" style={{
        height: '100vh',
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        backgroundColor: '#0c0c0c',
        color: '#00ff00',
        fontFamily: 'Consolas, Monaco, "Courier New", monospace'
      }}>
        <div className="auth-container" style={{
          backgroundColor: '#1a1a1a',
          border: '2px solid #00ff00',
          borderRadius: '8px',
          padding: '40px',
          minWidth: '400px',
          maxWidth: '500px'
        }}>
          <div className="auth-header" style={{
            textAlign: 'center',
            marginBottom: '30px'
          }}>
            <h1 style={{
              margin: '0 0 10px 0',
              color: '#00ffff',
              fontSize: '24px'
            }}>
              {title}
            </h1>
            <p style={{
              margin: 0,
              color: '#ffff00',
              fontSize: '14px'
            }}>
              {subtitle}
            </p>
          </div>

          <form onSubmit={handleLogin} style={{ width: '100%' }}>
            <div className="form-group" style={{ marginBottom: '20px' }}>
              <label style={{
                display: 'block',
                marginBottom: '5px',
                color: '#00ff00',
                fontSize: '14px'
              }}>
                Username:
              </label>
              <input
                type="text"
                value={loginForm.username}
                onChange={handleInputChange('username')}
                disabled={isLoading}
                style={{
                  width: '100%',
                  padding: '10px',
                  backgroundColor: '#0c0c0c',
                  border: '1px solid #333',
                  borderRadius: '4px',
                  color: '#00ff00',
                  fontSize: '14px',
                  fontFamily: 'inherit'
                }}
                placeholder="Enter username"
              />
            </div>

            <div className="form-group" style={{ marginBottom: '20px' }}>
              <label style={{
                display: 'block',
                marginBottom: '5px',
                color: '#00ff00',
                fontSize: '14px'
              }}>
                Workstation:
              </label>
              <input
                type="text"
                value={loginForm.workstation}
                onChange={handleInputChange('workstation')}
                disabled={isLoading}
                style={{
                  width: '100%',
                  padding: '10px',
                  backgroundColor: '#0c0c0c',
                  border: '1px solid #333',
                  borderRadius: '4px',
                  color: '#00ff00',
                  fontSize: '14px',
                  fontFamily: 'inherit'
                }}
                placeholder="Enter workstation ID"
              />
            </div>

            <div className="form-group" style={{ marginBottom: '20px' }}>
              <label style={{
                display: 'block',
                marginBottom: '5px',
                color: '#00ff00',
                fontSize: '14px'
              }}>
                Password:
              </label>
              <input
                type="password"
                value={loginForm.password}
                onChange={handleInputChange('password')}
                disabled={isLoading}
                style={{
                  width: '100%',
                  padding: '10px',
                  backgroundColor: '#0c0c0c',
                  border: '1px solid #333',
                  borderRadius: '4px',
                  color: '#00ff00',
                  fontSize: '14px',
                  fontFamily: 'inherit'
                }}
                placeholder="Enter password"
              />
            </div>

            {loginError && (
              <div style={{
                padding: '10px',
                backgroundColor: '#330000',
                border: '1px solid #ff6060',
                borderRadius: '4px',
                color: '#ff6060',
                fontSize: '14px',
                marginBottom: '20px'
              }}>
                {loginError}
              </div>
            )}

            <button
              type="submit"
              disabled={isLoading}
              style={{
                width: '100%',
                padding: '12px',
                backgroundColor: isLoading ? '#666' : '#006600',
                color: 'white',
                border: 'none',
                borderRadius: '4px',
                fontSize: '16px',
                cursor: isLoading ? 'not-allowed' : 'pointer',
                fontFamily: 'inherit'
              }}
            >
              {isLoading ? 'Authenticating...' : 'Login'}
            </button>
          </form>

          <div style={{
            marginTop: '30px',
            padding: '15px',
            backgroundColor: '#2a2a2a',
            borderRadius: '4px',
            fontSize: '12px',
            color: '#999'
          }}>
            <strong style={{ color: '#ffff00' }}>Demo Information:</strong><br/>
            • Use any username and workstation ID<br/>
            • Password: any non-empty value<br/>
            • MSGSAMPLEBROWSERMENU command shows employee data
          </div>
        </div>
      </div>
    );
  }

  // Render authenticated terminal interface
  return (
    <div className="workstation-wrapper" style={{ height: '100vh', display: 'flex', flexDirection: 'column' }}>
      {/* Session Info Bar */}
      <div className="session-bar" style={{
        display: 'flex',
        justifyContent: 'space-between',
        alignItems: 'center',
        padding: '8px 15px',
        backgroundColor: '#2a2a2a',
        borderBottom: '1px solid #333',
        color: '#00ff00',
        fontSize: '12px'
      }}>
        <div style={{ display: 'flex', gap: '20px' }}>
          <span><strong>User:</strong> {authState.username}</span>
          <span><strong>Workstation:</strong> {authState.workstation}</span>
          <span><strong>Session:</strong> {sessionDuration}</span>
        </div>
        
        <div style={{ display: 'flex', alignItems: 'center', gap: '15px' }}>
          <div style={{
            display: 'flex',
            alignItems: 'center',
            gap: '5px'
          }}>
            <div style={{
              width: '6px',
              height: '6px',
              borderRadius: '50%',
              backgroundColor: isConnected ? '#00ff00' : '#ff6060'
            }}></div>
            <span style={{ color: isConnected ? '#00ff00' : '#ff6060' }}>
              {isConnected ? 'Connected' : 'Disconnected'}
            </span>
          </div>
          
          <button
            onClick={handleLogout}
            style={{
              padding: '4px 8px',
              backgroundColor: '#cc3300',
              color: 'white',
              border: 'none',
              borderRadius: '3px',
              cursor: 'pointer',
              fontSize: '11px'
            }}
          >
            Logout
          </button>
        </div>
      </div>

      {/* Terminal Component */}
      <div style={{ flex: 1 }}>
        <AspCliWebTerminal 
          websocketUrl={websocketUrl}
          onConnectionChange={handleConnectionChange}
          workstationInfo={{
            username: authState.username,
            workstation: authState.workstation,
            sessionId: authState.sessionId
          }}
        />
      </div>
    </div>
  );
};

export default WorkstationAuthWrapper;