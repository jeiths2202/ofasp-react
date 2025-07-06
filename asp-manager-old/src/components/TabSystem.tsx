import React from 'react';
import { Tab } from '../types';

interface TabSystemProps {
  tabs: Tab[];
  activeTabId: string | null;
  onTabSelect: (tabId: string) => void;
  onTabClose: (tabId: string) => void;
}

const TabSystem: React.FC<TabSystemProps> = ({
  tabs,
  activeTabId,
  onTabSelect,
  onTabClose
}) => {
  const activeTab = tabs.find(tab => tab.id === activeTabId);

  console.log('[DEBUG] TabSystem render:', {
    tabsCount: tabs.length,
    tabs: tabs.map(t => t.id),
    activeTabId,
    activeTab: activeTab?.id
  });

  return (
    <div className="flex-1 flex flex-col h-full">
      {/* Tab Bar */}
      {tabs.length > 0 && (
        <div className="flex items-center bg-white border-b border-gray-200 overflow-x-auto">
          <div className="flex space-x-0">
            {tabs.map((tab) => (
              <div
                key={tab.id}
                className={`flex items-center min-w-0 ${
                  activeTabId === tab.id
                    ? 'bg-blue-50 border-b-2 border-blue-500'
                    : 'bg-gray-50 hover:bg-gray-100'
                } transition-colors duration-200`}
              >
                <button
                  onClick={() => onTabSelect(tab.id)}
                  className="flex items-center px-4 py-3 text-sm font-medium min-w-0 max-w-xs"
                >
                  <span className="truncate">{tab.label}</span>
                </button>
                {tab.closable && (
                  <button
                    onClick={(e) => {
                      e.stopPropagation();
                      onTabClose(tab.id);
                    }}
                    className="p-1 mr-2 text-gray-400 hover:text-gray-600 hover:bg-gray-200 rounded"
                  >
                    <svg className="h-4 w-4" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                      <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M6 18L18 6M6 6l12 12" />
                    </svg>
                  </button>
                )}
              </div>
            ))}
          </div>
        </div>
      )}

      {/* Tab Content */}
      <div className="flex-1 bg-gray-50">
        {activeTab ? (
          <div className="h-full">
            <activeTab.component {...(activeTab.data || {})} />
          </div>
        ) : (
          <div className="h-full flex items-center justify-center">
            <div className="text-center">
              <svg className="mx-auto h-24 w-24 text-gray-300" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1} d="M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z" />
              </svg>
              <h3 className="mt-4 text-lg font-medium text-gray-900">ASP Managerへようこそ</h3>
              <p className="mt-2 text-sm text-gray-500">
                左側のメニューから管理項目を選択してください。
              </p>
            </div>
          </div>
        )}
      </div>
    </div>
  );
};

export default TabSystem;