import React, { useState, useCallback } from 'react';
import LoginPage from './pages/LoginPage';
import ClaudeSidebar from './components/ClaudeSidebar';
import TabSystem from './components/TabSystem';
import DashboardPage from './components/DashboardPage';
import { AuthState, LoginFormData, MenuItem, Tab, User } from './types';

// Menu icons (using SVG for simplicity)
const DashboardIcon = (
  <svg className="h-5 w-5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M3 7v10a2 2 0 002 2h14a2 2 0 002-2V9a2 2 0 00-2-2H5a2 2 0 00-2-2z" />
    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M8 5a2 2 0 012-2h4a2 2 0 012 2v14l-4-2-4 2V5z" />
  </svg>
);

const UsersIcon = (
  <svg className="h-5 w-5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M12 4.354a4 4 0 110 5.292M15 21H3v-1a6 6 0 0112 0v1zm0 0h6v-1a6 6 0 00-9-5.197m13.5-9a2.5 2.5 0 11-5 0 2.5 2.5 0 015 0z" />
  </svg>
);

const MapsIcon = (
  <svg className="h-5 w-5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z" />
  </svg>
);

const ProgramsIcon = (
  <svg className="h-5 w-5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M10 20l4-16m4 4l4 4-4 4M6 16l-4-4 4-4" />
  </svg>
);

const SettingsIcon = (
  <svg className="h-5 w-5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M10.325 4.317c.426-1.756 2.924-1.756 3.35 0a1.724 1.724 0 002.573 1.066c1.543-.94 3.31.826 2.37 2.37a1.724 1.724 0 001.065 2.572c1.756.426 1.756 2.924 0 3.35a1.724 1.724 0 00-1.066 2.573c.94 1.543-.826 3.31-2.37 2.37a1.724 1.724 0 00-2.572 1.065c-.426 1.756-2.924 1.756-3.35 0a1.724 1.724 0 00-2.573-1.066c-1.543.94-3.31-.826-2.37-2.37a1.724 1.724 0 00-1.065-2.572c-1.756-.426-1.756-2.924 0-3.35a1.724 1.724 0 001.066-2.573c-.94-1.543.826-3.31 2.37-2.37.996.608 2.296.07 2.572-1.065z" />
    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M15 12a3 3 0 11-6 0 3 3 0 016 0z" />
  </svg>
);

// Placeholder components for other pages
const AccountsPage: React.FC = () => (
  <div className="h-full flex items-center justify-center">
    <div className="text-center">
      <h3 className="text-lg font-medium text-gray-900">アカウント管理</h3>
      <p className="mt-2 text-sm text-gray-500">アカウント管理機能は準備中です。</p>
    </div>
  </div>
);

const SmedMapsPage: React.FC = () => (
  <div className="h-full flex items-center justify-center">
    <div className="text-center">
      <h3 className="text-lg font-medium text-gray-900">SMEDマップ管理</h3>
      <p className="mt-2 text-sm text-gray-500">SMEDマップ管理機能は準備中です。</p>
    </div>
  </div>
);

const ProgramsPage: React.FC = () => (
  <div className="h-full flex items-center justify-center">
    <div className="text-center">
      <h3 className="text-lg font-medium text-gray-900">プログラム管理</h3>
      <p className="mt-2 text-sm text-gray-500">プログラム管理機能は準備中です。</p>
    </div>
  </div>
);

const SettingsPage: React.FC = () => (
  <div className="h-full flex items-center justify-center">
    <div className="text-center">
      <h3 className="text-lg font-medium text-gray-900">システム設定</h3>
      <p className="mt-2 text-sm text-gray-500">システム設定機能は準備中です。</p>
    </div>
  </div>
);

function App() {
  // Development mode - set to true to bypass login
  const DEV_MODE = true;
  
  const [authState, setAuthState] = useState<AuthState>({
    isAuthenticated: DEV_MODE,
    user: DEV_MODE ? { id: 'admin', username: 'admin', role: 'admin' } : null
  });
  const [loginLoading, setLoginLoading] = useState(false);
  const [loginError, setLoginError] = useState<string | null>(null);
  const [activeMenuId, setActiveMenuId] = useState<string | null>(null);
  const [tabs, setTabs] = useState<Tab[]>(
    DEV_MODE ? [{ id: 'dashboard', label: 'ダッシュボード', component: DashboardPage, closable: false }] : []
  );
  const [activeTabId, setActiveTabId] = useState<string | null>(DEV_MODE ? 'dashboard' : null);
  const [sidebarExpanded, setSidebarExpanded] = useState(true);

  // Menu items configuration
  const menuItems: MenuItem[] = [
    {
      id: 'dashboard',
      label: 'ダッシュボード',
      icon: DashboardIcon,
      path: '/dashboard'
    },
    {
      id: 'accounts',
      label: 'アカウント管理',
      icon: UsersIcon,
      path: '/accounts'
    },
    {
      id: 'smed-maps',
      label: 'SMEDマップ',
      icon: MapsIcon,
      path: '/smed-maps'
    },
    {
      id: 'programs',
      label: 'プログラム管理',
      icon: ProgramsIcon,
      path: '/programs'
    },
    {
      id: 'settings',
      label: 'システム設定',
      icon: SettingsIcon,
      path: '/settings'
    }
  ];

  // Get component for menu item
  const getComponentForMenuItem = (menuId: string) => {
    switch (menuId) {
      case 'dashboard': return DashboardPage;
      case 'accounts': return AccountsPage;
      case 'smed-maps': return SmedMapsPage;
      case 'programs': return ProgramsPage;
      case 'settings': return SettingsPage;
      default: return DashboardPage;
    }
  };

  // Login handler
  const handleLogin = useCallback(async (credentials: LoginFormData) => {
    setLoginLoading(true);
    setLoginError(null);

    try {
      // Simulate API call to validate credentials
      const response = await fetch('http://localhost:8000/api/login', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          user_id: credentials.username,
          password: credentials.password
        })
      });

      const result = await response.json();

      if (result.success) {
        const user: User = {
          id: result.user_id,
          username: result.user_id,
          role: 'admin'
        };

        setAuthState({
          isAuthenticated: true,
          user
        });

        // Open dashboard tab by default
        const dashboardTab: Tab = {
          id: 'dashboard',
          label: 'ダッシュボード',
          component: DashboardPage,
          closable: false
        };
        setTabs([dashboardTab]);
        setActiveTabId('dashboard');
        setActiveMenuId('dashboard');
      } else {
        setLoginError(result.error || 'ログインに失敗しました。');
      }
    } catch (error) {
      console.error('Login error:', error);
      setLoginError('サーバーに接続できません。');
    } finally {
      setLoginLoading(false);
    }
  }, []);

  // Menu selection handler
  const handleMenuSelect = useCallback((menuItem: MenuItem) => {
    console.log('[DEBUG] handleMenuSelect called:', menuItem.id, menuItem.label);
    console.log('[DEBUG] Current tabs:', tabs.map(t => t.id));
    
    setActiveMenuId(menuItem.id);

    // Check if tab already exists
    const existingTab = tabs.find(tab => tab.id === menuItem.id);
    
    if (existingTab) {
      console.log('[DEBUG] Tab already exists, switching to:', menuItem.id);
      setActiveTabId(menuItem.id);
    } else {
      console.log('[DEBUG] Creating new tab:', menuItem.id);
      // Create new tab
      const newTab: Tab = {
        id: menuItem.id,
        label: menuItem.label,
        component: getComponentForMenuItem(menuItem.id),
        closable: menuItem.id !== 'dashboard' // Dashboard tab is not closable
      };

      setTabs(prev => {
        const newTabs = [...prev, newTab];
        console.log('[DEBUG] New tabs after adding:', newTabs.map(t => t.id));
        return newTabs;
      });
      setActiveTabId(menuItem.id);
      console.log('[DEBUG] Set active tab ID to:', menuItem.id);
    }
  }, [tabs]);

  // Tab selection handler
  const handleTabSelect = useCallback((tabId: string) => {
    setActiveTabId(tabId);
    setActiveMenuId(tabId);
  }, []);

  // Tab close handler
  const handleTabClose = useCallback((tabId: string) => {
    setTabs(prev => {
      const newTabs = prev.filter(tab => tab.id !== tabId);
      
      // If the closed tab was active, switch to another tab
      if (activeTabId === tabId) {
        const newActiveTab = newTabs.length > 0 ? newTabs[newTabs.length - 1] : null;
        setActiveTabId(newActiveTab?.id || null);
        setActiveMenuId(newActiveTab?.id || null);
      }
      
      return newTabs;
    });
  }, [activeTabId]);

  // Logout handler
  const handleLogout = useCallback(() => {
    setAuthState({
      isAuthenticated: false,
      user: null
    });
    setTabs([]);
    setActiveTabId(null);
    setActiveMenuId(null);
  }, []);

  // Show login page if not authenticated
  if (!authState.isAuthenticated) {
    return (
      <LoginPage
        onLogin={handleLogin}
        isLoading={loginLoading}
        error={loginError}
      />
    );
  }

  // Main application layout
  return (
    <div className="h-screen flex bg-gray-100">
      {/* Claude-style Sidebar */}
      <ClaudeSidebar
        menuItems={menuItems}
        onMenuSelect={handleMenuSelect}
        activeMenuId={activeMenuId}
        currentUser={authState.user ? {
          username: authState.user.username,
          role: authState.user.role
        } : undefined}
        onLogout={handleLogout}
        onExpandedChange={setSidebarExpanded}
      />

      {/* Main Content */}
      <div 
        className="fixed inset-0 flex flex-col transition-all duration-300"
        style={{ paddingLeft: sidebarExpanded ? '256px' : '56px' }}
      >
        {/* Tab System */}
        <TabSystem
          tabs={tabs}
          activeTabId={activeTabId}
          onTabSelect={handleTabSelect}
          onTabClose={handleTabClose}
        />
      </div>
    </div>
  );
}

export default App;