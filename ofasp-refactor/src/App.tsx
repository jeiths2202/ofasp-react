import React, { useState, useEffect, useCallback } from 'react';
import {
  HomeIcon,
  CodeBracketIcon,
  CommandLineIcon,
  DocumentTextIcon,
  CogIcon,
  BookOpenIcon,
  ChatBubbleLeftRightIcon,
} from '@heroicons/react/24/outline';
import Sidebar from './components/Sidebar';
import TabSystem from './components/TabSystem';
import CobolRefactorPage from './pages/CobolRefactorPage';
import ClRefactorPage from './pages/ClRefactorPage';
import MarkdownRenderer from './components/MarkdownRenderer';
import { MenuItem, Tab, Theme } from './types';

function App() {
  const [theme, setTheme] = useState<Theme>({ mode: 'dark' });
  const [activeMenuId, setActiveMenuId] = useState<string | null>('dashboard');
  const [tabs, setTabs] = useState<Tab[]>([
    {
      id: 'dashboard',
      title: 'ダッシュボード',
      content: (
        <div className="p-8">
          <h1 className="text-2xl font-bold text-gray-900 dark:text-white mb-2">
            OpenASP Refactor Tool
          </h1>
          <p className="text-gray-600 dark:text-gray-400 mb-8">
            Fujitsu ASP 애플리케이션을 Open 환경으로 마이그레이션하는 리팩토링 도구입니다.
          </p>
          
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            <div className="bg-white dark:bg-gray-800 rounded-lg shadow-sm p-6 border border-gray-200 dark:border-gray-700">
              <div className="flex items-center mb-4">
                <CodeBracketIcon className="w-8 h-8 text-blue-600 dark:text-blue-400 mr-3" />
                <h3 className="text-lg font-semibold text-gray-900 dark:text-white">
                  COBOL Refactoring
                </h3>
              </div>
              <p className="text-gray-600 dark:text-gray-400 mb-4">
                COBOL 프로그램을 Java, C, Shell Script, Python으로 변환합니다.
              </p>
              <div className="text-sm text-gray-500 dark:text-gray-400">
                • 데이터 구조 변환
                • 비즈니스 로직 마이그레이션
                • 파일 처리 로직 변환
              </div>
            </div>

            <div className="bg-white dark:bg-gray-800 rounded-lg shadow-sm p-6 border border-gray-200 dark:border-gray-700">
              <div className="flex items-center mb-4">
                <CommandLineIcon className="w-8 h-8 text-green-600 dark:text-green-400 mr-3" />
                <h3 className="text-lg font-semibold text-gray-900 dark:text-white">
                  CL Refactoring
                </h3>
              </div>
              <p className="text-gray-600 dark:text-gray-400 mb-4">
                CL(Control Language) 스크립트를 Shell, JavaScript, Python으로 변환합니다.
              </p>
              <div className="text-sm text-gray-500 dark:text-gray-400">
                • 파일 작업 명령어 변환
                • 프로그램 호출 로직 변환
                • 배치 작업 스크립트 변환
              </div>
            </div>
          </div>

          <div className="mt-8 bg-blue-50 dark:bg-blue-900/20 rounded-lg p-6">
            <h3 className="text-lg font-semibold text-blue-900 dark:text-blue-100 mb-2">
              마이그레이션 프로세스
            </h3>
            <div className="grid grid-cols-1 md:grid-cols-3 gap-4 mt-4">
              <div className="text-center">
                <div className="w-12 h-12 bg-blue-600 text-white rounded-full flex items-center justify-center mx-auto mb-2 text-lg font-bold">
                  1
                </div>
                <h4 className="font-medium text-blue-900 dark:text-blue-100">소스 분석</h4>
                <p className="text-sm text-blue-700 dark:text-blue-300 mt-1">
                  기존 ASP 소스코드 구조 분석
                </p>
              </div>
              <div className="text-center">
                <div className="w-12 h-12 bg-blue-600 text-white rounded-full flex items-center justify-center mx-auto mb-2 text-lg font-bold">
                  2
                </div>
                <h4 className="font-medium text-blue-900 dark:text-blue-100">변환 실행</h4>
                <p className="text-sm text-blue-700 dark:text-blue-300 mt-1">
                  대상 언어로 자동 변환
                </p>
              </div>
              <div className="text-center">
                <div className="w-12 h-12 bg-blue-600 text-white rounded-full flex items-center justify-center mx-auto mb-2 text-lg font-bold">
                  3
                </div>
                <h4 className="font-medium text-blue-900 dark:text-blue-100">검증 배포</h4>
                <p className="text-sm text-blue-700 dark:text-blue-300 mt-1">
                  변환된 코드 검증 및 배포
                </p>
              </div>
            </div>
          </div>
        </div>
      ),
      type: 'document',
      timestamp: new Date(),
    },
  ]);
  const [activeTabId, setActiveTabId] = useState<string>('dashboard');
  const [sidebarWidth, setSidebarWidth] = useState(256);

  const handleMenuSelect = useCallback((item: MenuItem) => {
    setActiveMenuId(item.id);

    // 既存のタブを確認
    const existingTab = tabs.find((tab) => tab.id === item.id);
    if (existingTab) {
      setActiveTabId(item.id);
      return;
    }

    // 新しいタブを作成
    let content: React.ReactNode;
    switch (item.id) {
      case 'dashboard':
        return;
      case 'cobol-refactor':
        content = <CobolRefactorPage isDarkMode={theme.mode === 'dark'} />;
        break;
      case 'cl-refactor':
        content = <ClRefactorPage isDarkMode={theme.mode === 'dark'} />;
        break;
      default:
        content = (
          <div className="p-8">
            <h2 className="text-xl font-bold text-gray-900 dark:text-white mb-4">
              {item.label}
            </h2>
            <div className="bg-gray-100 dark:bg-gray-800 rounded-lg p-8 text-center">
              <p className="text-gray-600 dark:text-gray-400">
                この機能は現在開発中です
              </p>
            </div>
          </div>
        );
    }

    const newTab: Tab = {
      id: item.id,
      title: item.label,
      content: content,
      type: 'document',
      timestamp: new Date(),
    };

    setTabs([...tabs, newTab]);
    setActiveTabId(item.id);
  }, [tabs, theme.mode]);

  // テーマの初期化
  useEffect(() => {
    const savedTheme = localStorage.getItem('theme') as 'light' | 'dark' | null;
    if (savedTheme) {
      setTheme({ mode: savedTheme });
    } else {
      setTheme({ mode: 'dark' });
      localStorage.setItem('theme', 'dark');
    }
  }, []);

  // テーマ変更時の処理
  useEffect(() => {
    if (theme.mode === 'dark') {
      document.documentElement.classList.add('dark');
    } else {
      document.documentElement.classList.remove('dark');
    }
    localStorage.setItem('theme', theme.mode);
  }, [theme]);

  // サイドバー幅の監視
  useEffect(() => {
    const checkSidebarWidth = () => {
      const sidebar = document.querySelector('.fixed.left-0');
      if (sidebar) {
        const width = sidebar.getBoundingClientRect().width;
        setSidebarWidth(width);
      }
    };

    checkSidebarWidth();
    const interval = setInterval(checkSidebarWidth, 100);
    return () => clearInterval(interval);
  }, []);

  const menuItems: MenuItem[] = [
    { id: 'dashboard', label: 'ダッシュボード', icon: <HomeIcon /> },
    { id: 'cobol-refactor', label: 'COBOL Refactoring', icon: <CodeBracketIcon /> },
    { id: 'cl-refactor', label: 'CL Refactoring', icon: <CommandLineIcon /> },
    { id: 'docs', label: 'ドキュメント', icon: <BookOpenIcon /> },
    { id: 'settings', label: '設定', icon: <CogIcon /> },
    { id: 'chat', label: 'チャット', icon: <ChatBubbleLeftRightIcon /> },
  ];

  const handleTabSelect = (tabId: string) => {
    setActiveTabId(tabId);
    setActiveMenuId(tabId);
  };

  const handleTabClose = (tabId: string) => {
    const newTabs = tabs.filter((tab) => tab.id !== tabId);
    setTabs(newTabs);

    if (activeTabId === tabId) {
      const newActiveTab = newTabs[newTabs.length - 1];
      if (newActiveTab) {
        setActiveTabId(newActiveTab.id);
        setActiveMenuId(newActiveTab.id);
      }
    }
  };

  const handleThemeToggle = () => {
    setTheme({ mode: theme.mode === 'light' ? 'dark' : 'light' });
  };

  return (
    <div className="h-screen bg-gray-50 dark:bg-gray-950">
      <Sidebar
        menuItems={menuItems}
        activeMenuId={activeMenuId}
        onMenuSelect={handleMenuSelect}
        theme={theme}
        onThemeToggle={handleThemeToggle}
        user={{ name: 'Admin', role: 'リファクタリング管理者' }}
      />
      <TabSystem
        tabs={tabs}
        activeTabId={activeTabId}
        onTabSelect={handleTabSelect}
        onTabClose={handleTabClose}
        sidebarWidth={sidebarWidth}
      />
    </div>
  );
}

export default App;
