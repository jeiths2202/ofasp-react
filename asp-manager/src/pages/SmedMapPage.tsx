import React, { useState, useEffect } from 'react';
import { DocumentTextIcon, EyeIcon, CogIcon } from '@heroicons/react/24/outline';

interface SmedMapPageProps {
  isDarkMode: boolean;
}

const SmedMapPage: React.FC<SmedMapPageProps> = ({ isDarkMode }) => {
  const [activeSubTab, setActiveSubTab] = useState<'view' | 'manage'>('view');
  const [smedFiles, setSmedFiles] = useState<string[]>([]);
  const [selectedFile, setSelectedFile] = useState<string>('');
  const [fileContent, setFileContent] = useState<string>('');
  const [loading, setLoading] = useState(false);

  const subTabs = [
    { id: 'view', label: 'SMED定義ファイルの選択と確認', icon: <EyeIcon className="w-4 h-4" /> },
    { id: 'manage', label: 'SMEDマップ管理', icon: <CogIcon className="w-4 h-4" /> },
  ];

  // SMEDファイル一覧を取得
  useEffect(() => {
    fetchSmedFiles();
  }, []);

  const fetchSmedFiles = async () => {
    try {
      const response = await fetch('http://localhost:8000/api/smed/files');
      if (response.ok) {
        const files = await response.json();
        setSmedFiles(files.files || []);
      } else {
        console.error('SMEDファイル取得エラー:', response.statusText);
      }
    } catch (error) {
      console.error('SMEDファイル取得エラー:', error);
      // フォールバック用のサンプルファイル
      setSmedFiles(['LOGO.smed', 'MENU.smed', 'PGM1.smed', 'PGM2.smed']);
    }
  };

  const fetchFileContent = async (filename: string) => {
    setLoading(true);
    try {
      const response = await fetch(`http://localhost:8000/api/smed/content/${filename}`);
      if (response.ok) {
        const content = await response.text();
        setFileContent(content);
      } else {
        // サンプルコンテンツ
        const sampleContent = generateSampleContent(filename);
        setFileContent(sampleContent);
      }
    } catch (error) {
      console.error('ファイル内容取得エラー:', error);
      const sampleContent = generateSampleContent(filename);
      setFileContent(sampleContent);
    } finally {
      setLoading(false);
    }
  };

  const generateSampleContent = (filename: string) => {
    const lines = [];
    for (let row = 0; row < 24; row++) {
      let line = '';
      for (let col = 0; col < 80; col++) {
        if (row === 0 || row === 23) {
          line += '+';
        } else if (col === 0 || col === 79) {
          line += '|';
        } else if (row === 2 && col >= 30 && col <= 50) {
          const titleMap: { [key: string]: string } = {
            'LOGO.smed': 'OpenASP LOGO画面',
            'MENU.smed': 'メインメニュー画面',
            'PGM1.smed': 'プログラム1画面',
            'PGM2.smed': 'プログラム2画面'
          };
          const title = titleMap[filename] || filename;
          if (col - 30 < title.length) {
            line += title[col - 30];
          } else {
            line += ' ';
          }
        } else if (row === 10 && filename === 'MENU.smed') {
          const menuItems = '1.営業管理 2.在庫管理 3.レポート F3.終了';
          if (col >= 10 && col - 10 < menuItems.length) {
            line += menuItems[col - 10];
          } else {
            line += ' ';
          }
        } else {
          line += ' ';
        }
      }
      lines.push(line);
    }
    return lines.join('\n');
  };

  const handleFileSelect = (filename: string) => {
    setSelectedFile(filename);
    fetchFileContent(filename);
  };

  const renderViewTab = () => (
    <div className="h-full flex bg-gray-50 dark:bg-gray-950">
      {/* 左側：ファイルリスト */}
      <div className="w-80 bg-white dark:bg-gray-900 border-r border-gray-200 dark:border-gray-800 p-6">
        <div className="mb-6">
          <h2 className="text-xl font-bold text-gray-900 dark:text-white mb-2">
            SMEDマップ管理
          </h2>
          <p className="text-sm text-gray-600 dark:text-gray-400">
            SMED定義ファイルの選択と確認
          </p>
        </div>

        <div className="mb-4">
          <h3 className="text-sm font-medium text-gray-900 dark:text-white mb-3">
            利用可能なSMEDファイル
          </h3>
          <div className="space-y-2">
            {smedFiles.map((file) => (
              <button
                key={file}
                onClick={() => handleFileSelect(file)}
                className={`w-full text-left p-3 rounded-lg border transition-colors ${
                  selectedFile === file
                    ? 'bg-blue-50 dark:bg-blue-900/20 border-blue-200 dark:border-blue-800'
                    : 'bg-gray-50 dark:bg-gray-800 border-gray-200 dark:border-gray-700 hover:bg-gray-100 dark:hover:bg-gray-700'
                }`}
              >
                <div className="flex items-center">
                  <DocumentTextIcon 
                    className={`w-4 h-4 mr-2 ${
                      selectedFile === file 
                        ? 'text-blue-600 dark:text-blue-400' 
                        : 'text-gray-400'
                    }`} 
                  />
                  <span className={`text-sm font-medium ${
                    selectedFile === file
                      ? 'text-blue-900 dark:text-blue-100'
                      : 'text-gray-900 dark:text-white'
                  }`}>
                    {file}
                  </span>
                </div>
              </button>
            ))}
          </div>
        </div>

        {selectedFile && (
          <div className="bg-blue-50 dark:bg-blue-900/20 rounded-lg p-4">
            <h4 className="text-sm font-medium text-blue-900 dark:text-blue-100 mb-2">
              選択中のファイル
            </h4>
            <p className="text-sm text-blue-700 dark:text-blue-300">{selectedFile}</p>
          </div>
        )}
      </div>

      {/* 右側：プレビューエリア */}
      <div className="flex-1 p-6">
        {selectedFile ? (
          <div>
            <div className="mb-4">
              <h3 className="text-lg font-semibold text-gray-900 dark:text-white mb-2">
                {selectedFile} - プレビュー
              </h3>
              <p className="text-sm text-gray-600 dark:text-gray-400">
                SMEDマップの内容 (24行 × 80列)
              </p>
            </div>

            <div className="bg-white dark:bg-gray-900 border border-gray-200 dark:border-gray-800 rounded-lg p-4">
              {loading ? (
                <div className="flex items-center justify-center h-96">
                  <div className="text-center">
                    <div className="animate-spin h-8 w-8 border-2 border-blue-500 border-t-transparent rounded-full mx-auto mb-2"></div>
                    <p className="text-gray-600 dark:text-gray-400">読み込み中...</p>
                  </div>
                </div>
              ) : (
                <div className="font-mono text-sm bg-black text-green-400 p-4 rounded overflow-auto">
                  <pre 
                    className="whitespace-pre" 
                    style={{ 
                      fontSize: '12px', 
                      lineHeight: '1.2',
                      fontFamily: 'Consolas, "Courier New", monospace'
                    }}
                  >
                    {fileContent}
                  </pre>
                </div>
              )}
            </div>
          </div>
        ) : (
          <div className="flex items-center justify-center h-full">
            <div className="text-center">
              <DocumentTextIcon className="w-16 h-16 text-gray-400 mx-auto mb-4" />
              <h3 className="text-lg font-medium text-gray-900 dark:text-white mb-2">
                SMEDファイルを選択してください
              </h3>
              <p className="text-gray-600 dark:text-gray-400">
                左側のリストからファイルを選択すると、プレビューが表示されます
              </p>
            </div>
          </div>
        )}
      </div>
    </div>
  );

  const renderManageTab = () => (
    <div className="h-full p-8">
      <div className="max-w-4xl mx-auto">
        <h3 className="text-xl font-bold text-gray-900 dark:text-white mb-6">
          SMEDマップ管理
        </h3>
        <div className="bg-white dark:bg-gray-900 rounded-lg shadow-sm border border-gray-200 dark:border-gray-800 p-6">
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
            <div className="bg-blue-50 dark:bg-blue-900/20 rounded-lg p-6 text-center">
              <DocumentTextIcon className="w-12 h-12 text-blue-600 dark:text-blue-400 mx-auto mb-4" />
              <h4 className="text-lg font-semibold text-gray-900 dark:text-white mb-2">
                新規作成
              </h4>
              <p className="text-sm text-gray-600 dark:text-gray-400 mb-4">
                新しいSMEDマップを作成します
              </p>
              <button className="bg-blue-600 hover:bg-blue-700 text-white px-4 py-2 rounded-lg text-sm font-medium transition-colors">
                作成開始
              </button>
            </div>

            <div className="bg-green-50 dark:bg-green-900/20 rounded-lg p-6 text-center">
              <EyeIcon className="w-12 h-12 text-green-600 dark:text-green-400 mx-auto mb-4" />
              <h4 className="text-lg font-semibold text-gray-900 dark:text-white mb-2">
                編集
              </h4>
              <p className="text-sm text-gray-600 dark:text-gray-400 mb-4">
                既存のSMEDマップを編集します
              </p>
              <button className="bg-green-600 hover:bg-green-700 text-white px-4 py-2 rounded-lg text-sm font-medium transition-colors">
                編集開始
              </button>
            </div>

            <div className="bg-orange-50 dark:bg-orange-900/20 rounded-lg p-6 text-center">
              <CogIcon className="w-12 h-12 text-orange-600 dark:text-orange-400 mx-auto mb-4" />
              <h4 className="text-lg font-semibold text-gray-900 dark:text-white mb-2">
                設定
              </h4>
              <p className="text-sm text-gray-600 dark:text-gray-400 mb-4">
                SMEDマップの設定を管理します
              </p>
              <button className="bg-orange-600 hover:bg-orange-700 text-white px-4 py-2 rounded-lg text-sm font-medium transition-colors">
                設定開始
              </button>
            </div>
          </div>

          <div className="mt-8">
            <h4 className="text-lg font-semibold text-gray-900 dark:text-white mb-4">
              マップファイル一覧
            </h4>
            <div className="space-y-2">
              {smedFiles.map((file) => (
                <div key={file} className="flex items-center justify-between p-4 bg-gray-50 dark:bg-gray-800 rounded-lg">
                  <div className="flex items-center">
                    <DocumentTextIcon className="w-5 h-5 text-gray-400 mr-3" />
                    <span className="font-medium text-gray-900 dark:text-white">{file}</span>
                  </div>
                  <div className="flex space-x-2">
                    <button className="text-blue-600 dark:text-blue-400 hover:text-blue-800 dark:hover:text-blue-300 text-sm font-medium">
                      編集
                    </button>
                    <button className="text-red-600 dark:text-red-400 hover:text-red-800 dark:hover:text-red-300 text-sm font-medium">
                      削除
                    </button>
                  </div>
                </div>
              ))}
            </div>
          </div>
        </div>
      </div>
    </div>
  );

  return (
    <div className="h-full flex flex-col bg-gray-50 dark:bg-gray-950">
      {/* サブタブナビゲーション */}
      <div className="bg-white dark:bg-gray-900 border-b border-gray-200 dark:border-gray-800">
        <div className="px-6">
          <div className="flex space-x-8">
            {subTabs.map((tab) => (
              <button
                key={tab.id}
                onClick={() => setActiveSubTab(tab.id as 'view' | 'manage')}
                className={`flex items-center py-4 px-1 border-b-2 font-medium text-sm transition-colors ${
                  activeSubTab === tab.id
                    ? 'border-blue-500 text-blue-600 dark:text-blue-400'
                    : 'border-transparent text-gray-500 dark:text-gray-400 hover:text-gray-700 dark:hover:text-gray-300 hover:border-gray-300'
                }`}
              >
                {tab.icon}
                <span className="ml-2">{tab.label}</span>
              </button>
            ))}
          </div>
        </div>
      </div>

      {/* サブタブコンテンツ */}
      <div className="flex-1">
        {activeSubTab === 'view' ? renderViewTab() : renderManageTab()}
      </div>
    </div>
  );
};

export default SmedMapPage;