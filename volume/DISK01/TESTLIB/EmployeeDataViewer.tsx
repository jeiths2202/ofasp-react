import React, { useState, useEffect } from 'react';
import { EmployeeRecord, SMEDMapData } from './websocketService';

interface EmployeeDataViewerProps {
  smedData: SMEDMapData | null;
  isVisible: boolean;
  onClose?: () => void;
}

const EmployeeDataViewer: React.FC<EmployeeDataViewerProps> = ({
  smedData,
  isVisible,
  onClose
}) => {
  const [animationClass, setAnimationClass] = useState('');

  useEffect(() => {
    if (smedData) {
      setAnimationClass('animate-fadeIn');
      const timer = setTimeout(() => setAnimationClass(''), 500);
      return () => clearTimeout(timer);
    }
  }, [smedData]);

  if (!isVisible || !smedData) {
    return null;
  }

  const handleKeyPress = (event: React.KeyboardEvent) => {
    if (event.key === 'Escape' && onClose) {
      onClose();
    }
  };

  return (
    <div 
      className="employee-data-viewer"
      onKeyDown={handleKeyPress}
      tabIndex={0}
      style={{
        position: 'fixed',
        top: '10%',
        left: '10%',
        right: '10%',
        bottom: '10%',
        backgroundColor: '#1a1a1a',
        border: '2px solid #00ff00',
        borderRadius: '8px',
        padding: '20px',
        zIndex: 1000,
        display: 'flex',
        flexDirection: 'column',
        fontFamily: 'monospace',
        color: '#00ff00',
        overflow: 'hidden'
      }}
    >
      {/* Header Section */}
      <div className="header-section" style={{ marginBottom: '15px' }}>
        <div className="title-bar" style={{
          display: 'flex',
          justifyContent: 'space-between',
          alignItems: 'center',
          borderBottom: '1px solid #00ff00',
          paddingBottom: '10px'
        }}>
          <div>
            <h2 style={{ 
              margin: 0, 
              fontSize: '18px', 
              fontWeight: 'bold',
              color: '#00ffff'
            }}>
              {smedData.title}
            </h2>
            <div style={{ fontSize: '14px', color: '#ffff00' }}>
              {smedData.subtitle}
            </div>
          </div>
          <button
            onClick={onClose}
            style={{
              background: '#ff3030',
              color: 'white',
              border: 'none',
              borderRadius: '4px',
              padding: '5px 10px',
              cursor: 'pointer',
              fontSize: '12px'
            }}
          >
            Close
          </button>
        </div>
        
        <div className="status-bar" style={{ 
          marginTop: '10px',
          display: 'flex',
          justifyContent: 'space-between',
          fontSize: '12px'
        }}>
          <span>Page {smedData.page_info.current} of {smedData.page_info.total}</span>
          <span>{smedData.status}</span>
        </div>
      </div>

      {/* Data Table Section */}
      <div className={`data-section ${animationClass}`} style={{ 
        flex: 1,
        overflow: 'auto',
        border: '1px solid #333',
        borderRadius: '4px'
      }}>
        <table style={{
          width: '100%',
          borderCollapse: 'collapse',
          fontSize: '13px'
        }}>
          <thead>
            <tr style={{ backgroundColor: '#333' }}>
              {smedData.headers.map((header, index) => (
                <th key={index} style={{
                  padding: '8px 12px',
                  textAlign: 'left',
                  borderBottom: '2px solid #00ff00',
                  color: '#ffff00',
                  fontWeight: 'bold'
                }}>
                  {header}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {smedData.data.map((record, index) => (
              <tr key={`${record.id}-${index}`} style={{
                backgroundColor: index % 2 === 0 ? '#2a2a2a' : '#1a1a1a',
                transition: 'background-color 0.2s'
              }}
              onMouseEnter={(e) => {
                (e.currentTarget as HTMLElement).style.backgroundColor = '#003300';
              }}
              onMouseLeave={(e) => {
                (e.currentTarget as HTMLElement).style.backgroundColor = 
                  index % 2 === 0 ? '#2a2a2a' : '#1a1a1a';
              }}>
                <td style={{ 
                  padding: '6px 12px', 
                  borderBottom: '1px solid #333',
                  color: '#00ffff'
                }}>
                  {record.id}
                </td>
                <td style={{ 
                  padding: '6px 12px', 
                  borderBottom: '1px solid #333',
                  color: '#ffffff'
                }}>
                  {record.name}
                </td>
                <td style={{ 
                  padding: '6px 12px', 
                  borderBottom: '1px solid #333',
                  color: '#ffff00'
                }}>
                  {record.dept}
                </td>
              </tr>
            ))}
          </tbody>
        </table>
        
        {smedData.data.length === 0 && (
          <div style={{
            textAlign: 'center',
            padding: '40px',
            color: '#666',
            fontSize: '14px'
          }}>
            No employee records found
          </div>
        )}
      </div>

      {/* Footer Section */}
      <div className="footer-section" style={{
        marginTop: '15px',
        padding: '10px',
        backgroundColor: '#333',
        borderRadius: '4px',
        fontSize: '12px',
        textAlign: 'center',
        color: '#ffff00'
      }}>
        {smedData.function_keys} | ESC=Close
      </div>

      {/* Custom CSS for animations */}
      <style jsx>{`
        @keyframes fadeIn {
          from { opacity: 0; transform: translateY(10px); }
          to { opacity: 1; transform: translateY(0); }
        }
        
        .animate-fadeIn {
          animation: fadeIn 0.5s ease-out;
        }
        
        .employee-data-viewer:focus {
          outline: 2px solid #00ff00;
          outline-offset: -2px;
        }
        
        table tr:hover {
          transform: scale(1.01);
          transition: transform 0.1s ease;
        }
      `}</style>
    </div>
  );
};

export default EmployeeDataViewer;