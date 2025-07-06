import React, { useState } from 'react';
import { MenuItem } from '../types';
import './Sidebar.css';

interface ClaudeSidebarProps {
  menuItems: MenuItem[];
  onMenuSelect: (item: MenuItem) => void;
  activeMenuId: string | null;
  currentUser?: {
    username: string;
    role?: string;
  };
  onLogout?: () => void;
  onExpandedChange?: (expanded: boolean) => void;
}

const ClaudeSidebar: React.FC<ClaudeSidebarProps> = ({
  menuItems,
  onMenuSelect,
  activeMenuId,
  currentUser,
  onLogout,
  onExpandedChange
}) => {
  const [isExpanded, setIsExpanded] = useState(true);
  const [isHovered, setIsHovered] = useState(false);

  const handleToggle = () => {
    console.log('[DEBUG] Sidebar toggle clicked, current state:', isExpanded);
    const newExpanded = !isExpanded;
    setIsExpanded(newExpanded);
    onExpandedChange?.(newExpanded);
  };

  const effectiveExpanded = isExpanded || isHovered;

  return (
    <div 
      className={`sidebar-container fixed left-0 top-0 h-full bg-white border-r border-gray-200 shadow-sm z-40 ${
        effectiveExpanded ? 'sidebar-expanded' : 'sidebar-collapsed'
      }`}
      onMouseEnter={() => setIsHovered(true)}
      onMouseLeave={() => setIsHovered(false)}
    >
      {/* Header */}
      <div className="h-16 border-b border-gray-200 flex items-center justify-between px-3">
        <div className="flex items-center">
          <div className="sidebar-icon-container">
            <div className="h-8 w-8 bg-gradient-to-br from-blue-500 to-indigo-600 rounded-lg flex items-center justify-center">
              <svg className="h-5 w-5 text-white" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M13 10V3L4 14h7v7l9-11h-7z" />
              </svg>
            </div>
          </div>
          <span className={`sidebar-content ml-3 font-semibold text-gray-900 ${!effectiveExpanded ? 'hidden' : ''}`}>
            ASP Manager
          </span>
        </div>
        
        {/* Toggle button */}
        <button
          onClick={handleToggle}
          className={`sidebar-content p-1.5 rounded-lg hover:bg-gray-100 transition-colors ${!effectiveExpanded ? 'hidden' : ''}`}
        >
          <svg className="h-5 w-5 text-gray-500" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} 
              d={isExpanded ? "M11 19l-7-7 7-7m8 14l-7-7 7-7" : "M13 5l7 7-7 7M5 5l7 7-7 7"} />
          </svg>
        </button>
      </div>

      {/* User info */}
      {currentUser && (
        <div className="border-b border-gray-200 p-3">
          <div className="flex items-center">
            <div className="sidebar-icon-container">
              <div className="h-8 w-8 bg-gray-200 rounded-full flex items-center justify-center">
                <svg className="h-5 w-5 text-gray-600" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                  <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M16 7a4 4 0 11-8 0 4 4 0 018 0zM12 14a7 7 0 00-7 7h14a7 7 0 00-7-7z" />
                </svg>
              </div>
            </div>
            <div className={`sidebar-content ml-3 ${!effectiveExpanded ? 'hidden' : ''}`}>
              <div className="text-sm font-medium text-gray-900">{currentUser.username}</div>
              <div className="text-xs text-gray-500">{currentUser.role || '管理者'}</div>
            </div>
          </div>
        </div>
      )}

      {/* Navigation */}
      <nav className="sidebar-nav flex-1 overflow-y-auto p-2">
        {menuItems.map((item) => (
          <div key={item.id} className="relative mb-1">
            <button
              onClick={() => {
                console.log('[DEBUG] Menu item clicked:', item.id, item.label);
                onMenuSelect(item);
              }}
              className={`menu-item w-full flex items-center px-3 py-2.5 rounded-lg transition-all duration-200 ${
                activeMenuId === item.id ? 'active' : ''
              }`}
            >
              <div className="sidebar-icon-container">
                {item.icon}
              </div>
              <span className={`sidebar-content flex-1 text-left text-sm font-medium text-gray-700 ml-3 ${
                !effectiveExpanded ? 'hidden' : ''
              }`}>
                {item.label}
              </span>
              {item.badge && effectiveExpanded && (
                <span className="sidebar-content ml-2 inline-flex items-center justify-center px-2 py-0.5 text-xs font-bold text-white bg-red-500 rounded-full">
                  {item.badge}
                </span>
              )}
              
              {/* Tooltip for collapsed state */}
              {!effectiveExpanded && (
                <div className="sidebar-tooltip">
                  {item.label}
                  {item.badge && (
                    <span className="ml-2 inline-flex items-center justify-center px-1.5 py-0.5 text-xs font-bold text-white bg-red-500 rounded-full">
                      {item.badge}
                    </span>
                  )}
                </div>
              )}
            </button>

            {/* Submenu */}
            {item.children && activeMenuId === item.id && effectiveExpanded && (
              <div className="mt-1 ml-4 space-y-1">
                {item.children.map((child) => (
                  <button
                    key={child.id}
                    onClick={() => onMenuSelect(child)}
                    className="w-full flex items-center px-3 py-2 text-sm text-gray-600 hover:text-gray-900 hover:bg-gray-50 rounded-lg transition-colors duration-200"
                  >
                    <div className="sidebar-icon-container">
                      {child.icon}
                    </div>
                    <span className="ml-3">{child.label}</span>
                  </button>
                ))}
              </div>
            )}
          </div>
        ))}
      </nav>

      {/* Footer */}
      <div className="border-t border-gray-200 p-3">
        {onLogout && (
          <button
            onClick={onLogout}
            className="menu-item w-full flex items-center px-3 py-2.5 rounded-lg transition-all duration-200 hover:bg-red-50 group"
          >
            <div className="sidebar-icon-container">
              <svg className="h-5 w-5 text-gray-500 group-hover:text-red-600" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M17 16l4-4m0 0l-4-4m4 4H7m6 4v1a3 3 0 01-3 3H6a3 3 0 01-3-3V7a3 3 0 013-3h4a3 3 0 013 3v1" />
              </svg>
            </div>
            <span className={`sidebar-content flex-1 text-left text-sm font-medium text-gray-700 group-hover:text-red-600 ml-3 ${
              !effectiveExpanded ? 'hidden' : ''
            }`}>
              ログアウト
            </span>
            
            {/* Tooltip for collapsed state */}
            {!effectiveExpanded && (
              <div className="sidebar-tooltip">
                ログアウト
              </div>
            )}
          </button>
        )}
        
        <div className={`sidebar-content text-xs text-gray-400 text-center mt-3 ${!effectiveExpanded ? 'hidden' : ''}`}>
          OpenASP v0.5
        </div>
      </div>
    </div>
  );
};

export default ClaudeSidebar;