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

interface ClRefactorPageProps {
  isDarkMode: boolean;
}

const ClRefactorPage: React.FC<ClRefactorPageProps> = ({ isDarkMode }) => {
  const { t, tn } = useI18n();
  const [sourceCode, setSourceCode] = useState('');
  const [targetLanguage, setTargetLanguage] = useState<'shell' | 'javascript' | 'python'>('shell');
  const [refactoredCode, setRefactoredCode] = useState('');
  const [isRefactoring, setIsRefactoring] = useState(false);

  const targetLanguages = [
    { value: 'shell', label: t('languages.shell'), icon: '📜' },
    { value: 'javascript', label: t('languages.javascript'), icon: '🟨' },
    { value: 'python', label: t('languages.python'), icon: '🐍' },
  ];

  const handleRefactor = async () => {
    if (!sourceCode.trim()) return;
    
    setIsRefactoring(true);
    
    // 실제 refactoring API 호출이 들어갈 부분
    setTimeout(() => {
      setRefactoredCode(`// Refactored from CL to ${targetLanguage.toUpperCase()}
// Original CL commands converted to ${targetLanguage}

${targetLanguage === 'shell' ? `#!/bin/bash
# Converted CL commands
echo "Starting batch job..."
# File operations
cp source.txt target.txt
# Program execution
./converted_program
echo "Batch job completed"` : targetLanguage === 'javascript' ? `#!/usr/bin/env node
// Converted CL commands
const fs = require('fs');
const { execSync } = require('child_process');

console.log("Starting batch job...");
// File operations
fs.copyFileSync('source.txt', 'target.txt');
// Program execution
execSync('./converted_program');
console.log("Batch job completed");` : `#!/usr/bin/env python3
import os
import shutil
import subprocess

# Converted CL commands
print("Starting batch job...")
# File operations
shutil.copy('source.txt', 'target.txt')
# Program execution
subprocess.run(['./converted_program'])
print("Batch job completed")`}
`);
      setIsRefactoring(false);
    }, 2000);
  };

  const sampleClCode = `PGM
DCL VAR(&FILE) TYPE(*CHAR) LEN(50)
DCL VAR(&COUNT) TYPE(*DEC) LEN(5 0)

CHGVAR VAR(&FILE) VALUE('SAMPLE.DAT')
CHGVAR VAR(&COUNT) VALUE(0)

CPYF FROMFILE(LIBRARY/SRCFILE) TOFILE(LIBRARY/TGTFILE) +
     MBROPT(*REPLACE) CRTFILE(*YES)

CALL PGM(PROCESS_DATA) PARM(&FILE &COUNT)

SNDPGMMSG MSG('Processing completed successfully')

ENDPGM`;

  const loadSample = () => {
    setSourceCode(sampleClCode);
  };

  return (
    <div className="h-full p-8">
      <div className="mb-8">
        <h1 className="text-2xl font-bold text-gray-900 dark:text-white mb-2">
          {t('clRefactor.title')}
        </h1>
        <p className="text-gray-600 dark:text-gray-400">
          {t('clRefactor.subtitle')}
        </p>
      </div>

      {/* 설정 패널 */}
      <div className="bg-white dark:bg-gray-800 rounded-lg shadow-sm p-6 mb-6 border border-gray-200 dark:border-gray-700">
        <h3 className="text-lg font-semibold text-gray-900 dark:text-white mb-4">
          리팩토링 설정
        </h3>
        
        <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
          <div>
            <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
              대상 언어 선택
            </label>
            <div className="grid grid-cols-3 gap-2">
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
              빠른 작업
            </label>
            <div className="space-y-2">
              <button
                onClick={loadSample}
                className="w-full flex items-center justify-center px-4 py-2 bg-gray-100 dark:bg-gray-700 text-gray-700 dark:text-gray-300 rounded-lg hover:bg-gray-200 dark:hover:bg-gray-600 transition-colors"
              >
                <DocumentTextIcon className="w-4 h-4 mr-2" />
                샘플 CL 로드
              </button>
              <button
                className="w-full flex items-center justify-center px-4 py-2 bg-gray-100 dark:bg-gray-700 text-gray-700 dark:text-gray-300 rounded-lg hover:bg-gray-200 dark:hover:bg-gray-600 transition-colors"
              >
                <CloudArrowUpIcon className="w-4 h-4 mr-2" />
                파일 업로드
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
              CL 소스 코드
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
                {isRefactoring ? '리팩토링 중...' : '리팩토링 실행'}
              </button>
            </div>
          </div>
          <div className="p-4">
            <textarea
              value={sourceCode}
              onChange={(e) => setSourceCode(e.target.value)}
              placeholder="CL 소스 코드를 입력하세요..."
              className="w-full h-96 font-mono text-sm bg-gray-50 dark:bg-gray-900 border border-gray-200 dark:border-gray-600 rounded-lg p-4 text-gray-900 dark:text-white resize-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500"
            />
          </div>
        </div>

        {/* 리팩토링된 코드 */}
        <div className="bg-white dark:bg-gray-800 rounded-lg shadow-sm border border-gray-200 dark:border-gray-700">
          <div className="flex items-center justify-between p-4 border-b border-gray-200 dark:border-gray-700">
            <h3 className="text-lg font-semibold text-gray-900 dark:text-white">
              {targetLanguage.toUpperCase()} 코드
            </h3>
            {refactoredCode && (
              <button className="flex items-center px-4 py-2 bg-green-600 hover:bg-green-700 text-white rounded-lg transition-colors">
                <CloudArrowDownIcon className="w-4 h-4 mr-2" />
                다운로드
              </button>
            )}
          </div>
          <div className="p-4">
            {isRefactoring ? (
              <div className="flex items-center justify-center h-96">
                <div className="text-center">
                  <ArrowPathIcon className="w-8 h-8 text-blue-500 animate-spin mx-auto mb-4" />
                  <p className="text-gray-600 dark:text-gray-400">
                    CL 코드를 {targetLanguage.toUpperCase()}로 변환 중...
                  </p>
                </div>
              </div>
            ) : (
              <textarea
                value={refactoredCode}
                readOnly
                placeholder={`리팩토링된 ${targetLanguage.toUpperCase()} 코드가 여기에 표시됩니다...`}
                className="w-full h-96 font-mono text-sm bg-gray-50 dark:bg-gray-900 border border-gray-200 dark:border-gray-600 rounded-lg p-4 text-gray-900 dark:text-white resize-none"
              />
            )}
          </div>
        </div>
      </div>

      {/* CL 명령어 참조 */}
      <div className="mt-6 bg-white dark:bg-gray-800 rounded-lg shadow-sm p-6 border border-gray-200 dark:border-gray-700">
        <h3 className="text-lg font-semibold text-gray-900 dark:text-white mb-4">
          주요 CL 명령어 변환 참조
        </h3>
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
          <div className="bg-gray-50 dark:bg-gray-700 rounded-lg p-4">
            <h4 className="font-medium text-gray-900 dark:text-white mb-2">파일 작업</h4>
            <div className="text-sm text-gray-600 dark:text-gray-300 space-y-1">
              <div>CPYF → cp/copy</div>
              <div>DLTF → rm/unlink</div>
              <div>CRTPF → touch/create</div>
            </div>
          </div>
          <div className="bg-gray-50 dark:bg-gray-700 rounded-lg p-4">
            <h4 className="font-medium text-gray-900 dark:text-white mb-2">프로그램 실행</h4>
            <div className="text-sm text-gray-600 dark:text-gray-300 space-y-1">
              <div>CALL → function call</div>
              <div>SBMJOB → background exec</div>
              <div>EVOKE → subprocess</div>
            </div>
          </div>
          <div className="bg-gray-50 dark:bg-gray-700 rounded-lg p-4">
            <h4 className="font-medium text-gray-900 dark:text-white mb-2">변수 처리</h4>
            <div className="text-sm text-gray-600 dark:text-gray-300 space-y-1">
              <div>DCL VAR → variable declaration</div>
              <div>CHGVAR → assignment</div>
              <div>RCVF → input/read</div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default ClRefactorPage;