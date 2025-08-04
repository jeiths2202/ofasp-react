import React from 'react';
import WorkstationAuthWrapper from './WorkstationAuthWrapper';

/**
 * Main Application Component for OpenASP Web Terminal
 * 
 * This component serves as the entry point for the OpenASP terminal application,
 * providing a complete terminal interface with authentication and MSGSAMPLEBROWSERMENU support.
 */
const App: React.FC = () => {
  // Configuration - these would typically come from environment variables
  const config = {
    websocketUrl: process.env.REACT_APP_WEBSOCKET_URL || 'ws://localhost:3006/terminal',
    title: 'OpenASP Terminal System',
    subtitle: 'Enterprise Terminal Access with SAM File Browser'
  };

  return (
    <div className="app" style={{
      height: '100vh',
      overflow: 'hidden',
      backgroundColor: '#0c0c0c'
    }}>
      <WorkstationAuthWrapper 
        websocketUrl={config.websocketUrl}
        title={config.title}
        subtitle={config.subtitle}
      />
      
      {/* Global Styles */}
      <style jsx global>{`
        * {
          box-sizing: border-box;
          margin: 0;
          padding: 0;
        }
        
        body {
          font-family: 'Consolas', 'Monaco', 'Courier New', monospace;
          background-color: #0c0c0c;
          color: #00ff00;
          overflow: hidden;
        }
        
        input:focus,
        button:focus,
        div:focus {
          outline: 1px solid #00ff00;
          outline-offset: 2px;
        }
        
        button:hover:not(:disabled) {
          filter: brightness(1.2);
          transition: filter 0.2s;
        }
        
        input {
          transition: border-color 0.2s;
        }
        
        input:focus {
          border-color: #00ff00 !important;
        }
        
        /* Scrollbar styling for terminal areas */
        .terminal-output::-webkit-scrollbar {
          width: 8px;
        }
        
        .terminal-output::-webkit-scrollbar-track {
          background: #1a1a1a;
        }
        
        .terminal-output::-webkit-scrollbar-thumb {
          background: #333;
          border-radius: 4px;
        }
        
        .terminal-output::-webkit-scrollbar-thumb:hover {
          background: #555;
        }
        
        /* Animation keyframes */
        @keyframes pulse {
          0%, 100% { opacity: 1; }
          50% { opacity: 0.5; }
        }
        
        @keyframes slideIn {
          from { transform: translateY(-10px); opacity: 0; }
          to { transform: translateY(0); opacity: 1; }
        }
        
        .slide-in {
          animation: slideIn 0.3s ease-out;
        }
        
        /* Terminal cursor animation */
        .terminal-cursor {
          animation: pulse 1s infinite;
        }
        
        /* Loading spinner */
        @keyframes spin {
          0% { transform: rotate(0deg); }
          100% { transform: rotate(360deg); }
        }
        
        .loading-spinner {
          display: inline-block;
          width: 12px;
          height: 12px;
          border: 2px solid #333;
          border-top: 2px solid #00ff00;
          border-radius: 50%;
          animation: spin 1s linear infinite;
        }
      `}</style>
    </div>
  );
};

export default App;