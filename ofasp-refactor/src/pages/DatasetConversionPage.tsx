import React, { useState, useRef } from 'react';
import {
  FolderOpenIcon,
  DocumentIcon,
  ArrowRightIcon,
  DocumentArrowDownIcon,
  CheckIcon,
} from '@heroicons/react/24/outline';
import { pythonConverter } from '../utils/pythonConverter';

interface FileInfo {
  name: string;
  size: number;
  lastModified: Date;
  path: string;
  type: 'file' | 'directory';
  selected: boolean;
}

interface DatasetConversionPageProps {
  isDarkMode: boolean;
}

const DatasetConversionPage: React.FC<DatasetConversionPageProps> = ({ isDarkMode }) => {
  const [selectedFiles, setSelectedFiles] = useState<FileInfo[]>([]);
  const [convertedResults, setConvertedResults] = useState<string>('');
  const [isConverting, setIsConverting] = useState(false);
  const [selectedLayout, setSelectedLayout] = useState<string>('');
  const [layoutOptions] = useState<string[]>([
    'FIXED-LENGTH',
    'VARIABLE-LENGTH', 
    'DELIMITED-CSV',
    'COBOL-COPYBOOK',
    'MAINFRAME-EBCDIC'
  ]);
  const fileInputRef = useRef<HTMLInputElement>(null);
  const dirInputRef = useRef<HTMLInputElement>(null);

  const handleFileSelect = () => {
    fileInputRef.current?.click();
  };

  const handleDirectorySelect = () => {
    dirInputRef.current?.click();
  };

  const handleFileChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    const files = event.target.files;
    if (files) {
      const fileInfos: FileInfo[] = Array.from(files).map(file => ({
        name: file.name,
        size: file.size,
        lastModified: new Date(file.lastModified),
        path: file.name,
        type: 'file' as const,
        selected: false
      }));
      setSelectedFiles(fileInfos);
    }
  };

  const handleFileSelection = (index: number) => {
    const updatedFiles = [...selectedFiles];
    updatedFiles[index].selected = !updatedFiles[index].selected;
    setSelectedFiles(updatedFiles);
  };

  const handleSelectAll = () => {
    const allSelected = selectedFiles.every(file => file.selected);
    const updatedFiles = selectedFiles.map(file => ({
      ...file,
      selected: !allSelected
    }));
    setSelectedFiles(updatedFiles);
  };

  const formatFileSize = (bytes: number): string => {
    if (bytes === 0) return '0 B';
    const k = 1024;
    const sizes = ['B', 'KB', 'MB', 'GB'];
    const i = Math.floor(Math.log(bytes) / Math.log(k));
    return parseFloat((bytes / Math.pow(k, i)).toFixed(2)) + ' ' + sizes[i];
  };

  const formatDate = (date: Date): string => {
    return date.toLocaleDateString('ja-JP') + ' ' + date.toLocaleTimeString('ja-JP');
  };

  const handleConvert = async () => {
    const selectedFilesList = selectedFiles.filter(file => file.selected);
    if (selectedFilesList.length === 0 || !selectedLayout) return;
    
    setIsConverting(true);
    
    try {
      // Tools ページの変換機能を使用 (実際の実装に合わせて調整)
      // ここでは簡単な例としてデータセット変換をシミュレート
      const conversionResults = selectedFilesList.map(file => {
        return `変換完了: ${file.name}\nレイアウト: ${selectedLayout}\nサイズ: ${formatFileSize(file.size)}\n変換時刻: ${new Date().toLocaleString('ja-JP')}\n---`;
      }).join('\n\n');
      
      setConvertedResults(conversionResults);
    } catch (error) {
      console.error('変換エラー:', error);
      setConvertedResults('変換中にエラーが発生しました。');
    } finally {
      setIsConverting(false);
    }
  };

  const downloadResults = () => {
    if (!convertedResults) return;
    
    const blob = new Blob([convertedResults], { type: 'text/plain' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = `dataset_conversion_results_${new Date().toISOString().slice(0, 10)}.txt`;
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    URL.revokeObjectURL(url);
  };

  const selectedCount = selectedFiles.filter(file => file.selected).length;

  return (
    <div className="h-full p-6 bg-gray-50 dark:bg-gray-900">
      <h1 className="text-2xl font-bold text-gray-900 dark:text-white mb-6">
        データセット変換
      </h1>

      {/* ファイル選択エリア */}
      <div className="bg-white dark:bg-gray-800 rounded-lg shadow-sm p-6 mb-6">
        <h2 className="text-lg font-semibold text-gray-900 dark:text-white mb-4">
          データセットファイル選択
        </h2>
        <div className="flex space-x-4">
          <button
            onClick={handleFileSelect}
            className="flex items-center px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors"
          >
            <DocumentIcon className="w-5 h-5 mr-2" />
            ファイル選択
          </button>
          <button
            onClick={handleDirectorySelect}
            className="flex items-center px-4 py-2 bg-green-600 text-white rounded-lg hover:bg-green-700 transition-colors"
          >
            <FolderOpenIcon className="w-5 h-5 mr-2" />
            ディレクトリ選択
          </button>
        </div>
        
        <input
          ref={fileInputRef}
          type="file"
          multiple
          onChange={handleFileChange}
          className="hidden"
          accept=".dat,.txt,.csv,.fix,.vsam,.seq,.ps,.ksds"
        />
        <input
          ref={dirInputRef}
          type="file"
          {...({ webkitdirectory: '' } as any)}
          onChange={handleFileChange}
          className="hidden"
        />
      </div>

      {/* レイアウト/コピーブック選択 */}
      <div className="bg-white dark:bg-gray-800 rounded-lg shadow-sm p-6 mb-6">
        <h2 className="text-lg font-semibold text-gray-900 dark:text-white mb-4">
          レイアウト/コピーブック選択
        </h2>
        <select
          value={selectedLayout}
          onChange={(e) => setSelectedLayout(e.target.value)}
          className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white focus:ring-2 focus:ring-blue-500 focus:border-blue-500"
        >
          <option value="">レイアウトを選択してください</option>
          {layoutOptions.map((layout) => (
            <option key={layout} value={layout}>
              {layout}
            </option>
          ))}
        </select>
      </div>

      {/* ファイルリスト */}
      {selectedFiles.length > 0 && (
        <div className="bg-white dark:bg-gray-800 rounded-lg shadow-sm p-6 mb-6">
          <div className="flex items-center justify-between mb-4">
            <h2 className="text-lg font-semibold text-gray-900 dark:text-white">
              データセットファイル ({selectedFiles.length}件)
            </h2>
            <div className="flex items-center space-x-4">
              <span className="text-sm text-gray-600 dark:text-gray-400">
                選択: {selectedCount}件
              </span>
              <button
                onClick={handleSelectAll}
                className="flex items-center px-3 py-1 text-sm bg-gray-100 dark:bg-gray-700 text-gray-700 dark:text-gray-300 rounded-md hover:bg-gray-200 dark:hover:bg-gray-600 transition-colors"
              >
                <CheckIcon className="w-4 h-4 mr-1" />
                全選択
              </button>
            </div>
          </div>
          <div className="max-h-64 overflow-y-auto border border-gray-200 dark:border-gray-700 rounded-lg">
            {selectedFiles.map((file, index) => (
              <div
                key={index}
                className={`p-3 border-b border-gray-200 dark:border-gray-700 transition-colors ${
                  file.selected ? 'bg-blue-50 dark:bg-blue-900/20' : 'hover:bg-gray-50 dark:hover:bg-gray-700'
                }`}
              >
                <div className="flex items-center justify-between">
                  <div className="flex items-center space-x-3">
                    <input
                      type="checkbox"
                      checked={file.selected}
                      onChange={() => handleFileSelection(index)}
                      className="w-4 h-4 text-blue-600 border-gray-300 rounded focus:ring-blue-500"
                    />
                    <DocumentIcon className="w-5 h-5 text-gray-500 dark:text-gray-400" />
                    <div>
                      <p className="font-medium text-gray-900 dark:text-white">{file.name}</p>
                      <p className="text-sm text-gray-500 dark:text-gray-400">
                        {formatFileSize(file.size)} • {formatDate(file.lastModified)}
                      </p>
                    </div>
                  </div>
                </div>
              </div>
            ))}
          </div>
        </div>
      )}

      {/* 変換エリア */}
      {selectedFiles.length > 0 && (
        <div className="bg-white dark:bg-gray-800 rounded-lg shadow-sm p-6">
          <h2 className="text-lg font-semibold text-gray-900 dark:text-white mb-4">
            データセット変換実行
          </h2>
          
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
            {/* 変換設定・実行 */}
            <div>
              <h3 className="text-sm font-medium text-gray-700 dark:text-gray-300 mb-4">
                変換設定
              </h3>
              <div className="space-y-4">
                <div>
                  <label className="block text-sm text-gray-600 dark:text-gray-400 mb-2">
                    選択されたファイル: {selectedCount}件
                  </label>
                  <label className="block text-sm text-gray-600 dark:text-gray-400 mb-4">
                    レイアウト: {selectedLayout || '未選択'}
                  </label>
                </div>
                
                <button
                  onClick={handleConvert}
                  disabled={selectedCount === 0 || !selectedLayout || isConverting}
                  className="w-full flex items-center justify-center px-6 py-3 bg-purple-600 text-white rounded-lg hover:bg-purple-700 disabled:bg-gray-400 disabled:cursor-not-allowed transition-colors"
                >
                  {isConverting ? (
                    <>
                      <div className="animate-spin rounded-full h-5 w-5 border-b-2 border-white mr-2"></div>
                      変換中...
                    </>
                  ) : (
                    <>
                      <ArrowRightIcon className="w-5 h-5 mr-2" />
                      変換実行
                    </>
                  )}
                </button>
                
                {convertedResults && (
                  <button
                    onClick={downloadResults}
                    className="w-full flex items-center justify-center px-4 py-2 bg-green-600 text-white rounded-lg hover:bg-green-700 transition-colors"
                  >
                    <DocumentArrowDownIcon className="w-5 h-5 mr-2" />
                    結果ダウンロード
                  </button>
                )}
              </div>
            </div>

            {/* 変換結果表示 */}
            <div>
              <h3 className="text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                変換結果
              </h3>
              <div className="h-80 overflow-auto border border-gray-200 dark:border-gray-700 rounded-lg p-4 bg-gray-50 dark:bg-gray-900">
                <pre className="font-mono text-xs text-gray-800 dark:text-gray-200 whitespace-pre-wrap">
                  {convertedResults || 'ファイルを選択し、レイアウトを選択して変換ボタンを押してください。'}
                </pre>
              </div>
            </div>
          </div>
        </div>
      )}
    </div>
  );
};

export default DatasetConversionPage;