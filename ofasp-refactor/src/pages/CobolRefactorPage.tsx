import React, { useState } from 'react';
import { 
  CodeBracketIcon,
  ArrowPathIcon,
  DocumentTextIcon,
  PlayIcon,
  CloudArrowUpIcon,
  CloudArrowDownIcon
} from '@heroicons/react/24/outline';
import { useI18n } from '../hooks/useI18n';

interface CobolRefactorPageProps {
  isDarkMode: boolean;
}

const CobolRefactorPage: React.FC<CobolRefactorPageProps> = ({ isDarkMode }) => {
  const { t } = useI18n();
  const [sourceCode, setSourceCode] = useState('');
  const [targetLanguage, setTargetLanguage] = useState<'java' | 'c' | 'shell' | 'python'>('java');
  const [refactoredCode, setRefactoredCode] = useState('');
  const [isRefactoring, setIsRefactoring] = useState(false);

  const targetLanguages = [
    { value: 'java', label: t('languages.java'), icon: '☕' },
    { value: 'c', label: t('languages.c'), icon: '🔧' },
    { value: 'shell', label: t('languages.shell'), icon: '📜' },
    { value: 'python', label: t('languages.python'), icon: '🐍' },
  ];

  const handleRefactor = async () => {
    if (!sourceCode.trim()) return;
    
    setIsRefactoring(true);
    
    // 실제 refactoring API 호출이 들어갈 부분
    setTimeout(() => {
      setRefactoredCode(`// Refactored from COBOL to ${targetLanguage.toUpperCase()}
// Original COBOL source converted to ${targetLanguage}

${targetLanguage === 'java' ? `public class RefactoredProgram {
    public static void main(String[] args) {
        // Converted COBOL logic
        System.out.println("Refactored from COBOL");
    }
}` : targetLanguage === 'python' ? `#!/usr/bin/env python3
# Converted COBOL logic
def main():
    print("Refactored from COBOL")

if __name__ == "__main__":
    main()` : targetLanguage === 'c' ? `#include <stdio.h>
int main() {
    // Converted COBOL logic
    printf("Refactored from COBOL\\n");
    return 0;
}` : `#!/bin/bash
# Converted COBOL logic
echo "Refactored from COBOL"`}
`);
      setIsRefactoring(false);
    }, 2000);
  };

  const sampleCobolCode = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAMPLE-PROGRAM.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NAME PIC X(20).
       01 WS-COUNT PIC 9(3) VALUE 0.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Enter name: ".
           ACCEPT WS-NAME.
           DISPLAY "Hello " WS-NAME.
           STOP RUN.`;

  const loadSample = () => {
    setSourceCode(sampleCobolCode);
  };

  return (
    <div className="h-full p-8">
      <div className="mb-8">
        <h1 className="text-2xl font-bold text-gray-900 dark:text-white mb-2">
          {t('cobolRefactor.title')}
        </h1>
        <p className="text-gray-600 dark:text-gray-400">
          {t('cobolRefactor.subtitle')}
        </p>
      </div>

      {/* 설정 패널 */}
      <div className="bg-white dark:bg-gray-800 rounded-lg shadow-sm p-6 mb-6 border border-gray-200 dark:border-gray-700">
        <h3 className="text-lg font-semibold text-gray-900 dark:text-white mb-4">
          {t('cobolRefactor.settingsTitle')}
        </h3>
        
        <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
          <div>
            <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
              {t('cobolRefactor.targetLanguage')}
            </label>
            <div className="grid grid-cols-2 gap-2">
              {targetLanguages.map((lang) => (
                <button
                  key={lang.value}
                  onClick={() => setTargetLanguage(lang.value as any)}
                  className={`p-3 rounded-lg border transition-colors ${
                    targetLanguage === lang.value
                      ? 'bg-blue-50 dark:bg-blue-900/20 border-blue-200 dark:border-blue-800'
                      : 'bg-gray-50 dark:bg-gray-700 border-gray-200 dark:border-gray-600 hover:bg-gray-100 dark:hover:bg-gray-600'
                  }`}
                >
                  <div className="text-center">
                    <div className="text-2xl mb-1">{lang.icon}</div>
                    <div className={`text-sm font-medium ${
                      targetLanguage === lang.value
                        ? 'text-blue-900 dark:text-blue-100'
                        : 'text-gray-900 dark:text-white'
                    }`}>
                      {lang.label}
                    </div>
                  </div>
                </button>
              ))}
            </div>
          </div>
          
          <div>
            <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
              {t('cobolRefactor.quickActions')}
            </label>
            <div className="space-y-2">
              <button
                onClick={loadSample}
                className="w-full flex items-center justify-center px-4 py-2 bg-gray-100 dark:bg-gray-700 text-gray-700 dark:text-gray-300 rounded-lg hover:bg-gray-200 dark:hover:bg-gray-600 transition-colors"
              >
                <DocumentTextIcon className="w-4 h-4 mr-2" />
                {t('cobolRefactor.loadSample')}
              </button>
              <button
                className="w-full flex items-center justify-center px-4 py-2 bg-gray-100 dark:bg-gray-700 text-gray-700 dark:text-gray-300 rounded-lg hover:bg-gray-200 dark:hover:bg-gray-600 transition-colors"
              >
                <CloudArrowUpIcon className="w-4 h-4 mr-2" />
                {t('common.upload')}
              </button>
            </div>
          </div>
        </div>
      </div>

      {/* 코드 편집 영역 */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        {/* 소스 코드 */}
        <div className="bg-white dark:bg-gray-800 rounded-lg shadow-sm border border-gray-200 dark:border-gray-700">
          <div className="flex items-center justify-between p-4 border-b border-gray-200 dark:border-gray-700">
            <h3 className="text-lg font-semibold text-gray-900 dark:text-white">
              {t('cobolRefactor.sourceCode')}
            </h3>
            <div className="flex space-x-2">
              <button
                onClick={handleRefactor}
                disabled={!sourceCode.trim() || isRefactoring}
                className="flex items-center px-4 py-2 bg-blue-600 hover:bg-blue-700 disabled:bg-blue-400 text-white rounded-lg transition-colors"
              >
                {isRefactoring ? (
                  <ArrowPathIcon className="w-4 h-4 mr-2 animate-spin" />
                ) : (
                  <PlayIcon className="w-4 h-4 mr-2" />
                )}
                {isRefactoring ? t('cobolRefactor.refactoring') : t('cobolRefactor.executeRefactor')}
              </button>
            </div>
          </div>
          <div className="p-4">
            <textarea
              value={sourceCode}
              onChange={(e) => setSourceCode(e.target.value)}
              placeholder={t('cobolRefactor.sourcePlaceholder')}
              className="w-full h-96 font-mono text-sm bg-gray-50 dark:bg-gray-900 border border-gray-200 dark:border-gray-600 rounded-lg p-4 text-gray-900 dark:text-white resize-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500"
            />
          </div>
        </div>

        {/* 리팩토링된 코드 */}
        <div className="bg-white dark:bg-gray-800 rounded-lg shadow-sm border border-gray-200 dark:border-gray-700">
          <div className="flex items-center justify-between p-4 border-b border-gray-200 dark:border-gray-700">
            <h3 className="text-lg font-semibold text-gray-900 dark:text-white">
              {t('cobolRefactor.refactoredCode', { language: targetLanguage.toUpperCase() })}
            </h3>
            {refactoredCode && (
              <button className="flex items-center px-4 py-2 bg-green-600 hover:bg-green-700 text-white rounded-lg transition-colors">
                <CloudArrowDownIcon className="w-4 h-4 mr-2" />
                {t('common.download')}
              </button>
            )}
          </div>
          <div className="p-4">
            {isRefactoring ? (
              <div className="flex items-center justify-center h-96">
                <div className="text-center">
                  <ArrowPathIcon className="w-8 h-8 text-blue-500 animate-spin mx-auto mb-4" />
                  <p className="text-gray-600 dark:text-gray-400">
                    {t('cobolRefactor.converting', { language: targetLanguage.toUpperCase() })}
                  </p>
                </div>
              </div>
            ) : (
              <textarea
                value={refactoredCode}
                readOnly
                placeholder={t('cobolRefactor.refactoredPlaceholder', { language: targetLanguage.toUpperCase() })}
                className="w-full h-96 font-mono text-sm bg-gray-50 dark:bg-gray-900 border border-gray-200 dark:border-gray-600 rounded-lg p-4 text-gray-900 dark:text-white resize-none"
              />
            )}
          </div>
        </div>
      </div>
    </div>
  );
};

export default CobolRefactorPage;