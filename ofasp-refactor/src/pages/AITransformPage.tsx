import React, { useState, useRef } from 'react';
import {
  DocumentArrowUpIcon,
  DocumentArrowDownIcon,
  TableCellsIcon,
  ChartPieIcon,
  FolderOpenIcon,
  ArrowPathIcon,
  ExclamationTriangleIcon,
  CpuChipIcon,
  MagnifyingGlassIcon,
} from '@heroicons/react/24/outline';
import { useI18n } from '../hooks/useI18n';
import { CobolCallTreeAnalyzer } from '../utils/cobolCallTreeAnalyzer';

interface AITransformPageProps {
  isDarkMode: boolean;
}

interface FileInfo {
  name: string;
  type: 'COBOL' | 'COPYBOOK' | 'CL' | 'SMED' | 'UNKNOWN';
  size: number;
  originalEncoding: 'EBCDIC' | 'ASCII';
  converted: boolean;
  content?: string;
  convertedContent?: string;
}

interface AnalysisResult {
  summary: {
    totalFiles: number;
    byType: Record<string, number>;
    dependencies: {
      totalCalls: number;
      missingPrograms: number;
      circularDependencies: number;
    };
  };
  clPrograms: Array<{
    name: string;
    type: 'CL' | 'COBOL';
    calls: string[];
    libraries: string[];
  }>;
  allPrograms: Set<string>;
  detailedResults: any[];
}

interface CallTreeNode {
  name: string;
  type: string;
  isRepeated: boolean;
  isNotFound: boolean;
  children: CallTreeNode[];
  calls?: string[];
  cyclic?: boolean;
}

interface NeuralNetworkProps {
  isVisible: boolean;
  onComplete: () => void;
}

// Neural Network Animation Component
const NeuralNetworkAnimation: React.FC<NeuralNetworkProps> = ({ isVisible, onComplete }) => {
  const { t } = useI18n();
  React.useEffect(() => {
    if (isVisible) {
      // Auto-complete after 3 seconds (simulating analysis time)
      const timer = setTimeout(() => {
        onComplete();
      }, 3000);
      return () => clearTimeout(timer);
    }
  }, [isVisible, onComplete]);

  if (!isVisible) return null;

  return (
    <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50">
      <div className="bg-white dark:bg-gray-800 rounded-lg p-8 max-w-md w-full mx-4 text-center">
        {/* Neural Network SVG Animation */}
        <div className="mb-6">
          <svg 
            width="200" 
            height="150" 
            viewBox="0 0 200 150" 
            className="mx-auto"
          >
            {/* Neurons */}
            {[...Array(12)].map((_, i) => {
              const layer = Math.floor(i / 4);
              const position = i % 4;
              const x = 30 + layer * 70;
              const y = 20 + position * 30;
              
              return (
                <circle
                  key={i}
                  cx={x}
                  cy={y}
                  r="8"
                  fill="#3B82F6"
                  className="animate-pulse"
                  style={{
                    animationDelay: `${i * 0.1}s`,
                    animationDuration: '1.5s'
                  }}
                />
              );
            })}
            
            {/* Connections */}
            {[...Array(16)].map((_, i) => {
              const fromLayer = Math.floor(i / 8);
              const toLayer = fromLayer + 1;
              const fromPos = (i % 8) % 4;
              const toPos = Math.floor((i % 8) / 2);
              
              const x1 = 30 + fromLayer * 70;
              const y1 = 20 + fromPos * 30;
              const x2 = 30 + toLayer * 70;
              const y2 = 20 + toPos * 30;
              
              return (
                <line
                  key={i}
                  x1={x1}
                  y1={y1}
                  x2={x2}
                  y2={y2}
                  stroke="#60A5FA"
                  strokeWidth="2"
                  opacity="0.6"
                  className="animate-pulse"
                  style={{
                    animationDelay: `${i * 0.05}s`,
                    animationDuration: '2s'
                  }}
                />
              );
            })}
            
            {/* Data flow animation */}
            <circle r="3" fill="#F59E0B" className="animate-ping">
              <animateMotion
                dur="2s"
                repeatCount="indefinite"
                path="M30,50 L100,50 L170,50"
              />
            </circle>
          </svg>
        </div>
        
        <div className="flex items-center justify-center mb-4">
          <CpuChipIcon className="h-6 w-6 text-blue-500 mr-2 animate-spin" />
          <h3 className="text-lg font-semibold text-gray-900 dark:text-white">
            {t('aiTransform.aiAnalyzing')}
          </h3>
        </div>
        
        <p className="text-gray-600 dark:text-gray-300 mb-4">
          {t('aiTransform.neuralNetworkAnalyzing')}
        </p>
        
        <div className="flex items-center justify-center space-x-2">
          <div className="w-2 h-2 bg-blue-500 rounded-full animate-bounce" style={{ animationDelay: '0s' }}></div>
          <div className="w-2 h-2 bg-blue-500 rounded-full animate-bounce" style={{ animationDelay: '0.1s' }}></div>
          <div className="w-2 h-2 bg-blue-500 rounded-full animate-bounce" style={{ animationDelay: '0.2s' }}></div>
        </div>
      </div>
    </div>
  );
};

const AITransformPage: React.FC<AITransformPageProps> = ({ isDarkMode }) => {
  const { t } = useI18n();
  const [files, setFiles] = useState<FileInfo[]>([]);
  const [isProcessing, setIsProcessing] = useState(false);
  const [selectedFiles, setSelectedFiles] = useState<string[]>([]);
  const [showNeuralNetwork, setShowNeuralNetwork] = useState(false);
  const [analysisResult, setAnalysisResult] = useState<AnalysisResult | null>(null);
  const [selectedCLProgram, setSelectedCLProgram] = useState<string | null>(null);
  const [callTree, setCallTree] = useState<CallTreeNode | null>(null);
  const [isDragOver, setIsDragOver] = useState(false);
  const [directoryPath, setDirectoryPath] = useState('');
  const [isLoadingDirectory, setIsLoadingDirectory] = useState(false);
  const fileInputRef = useRef<HTMLInputElement>(null);

  // EBCDIC to ASCII character mapping (simplified)
  const ebcdicToAscii = (ebcdicString: string): string => {
    // Basic EBCDIC to ASCII conversion
    // This is a simplified mapping for demonstration
    const ebcdicMap: { [key: string]: string } = {
      '\u00C1': 'A', '\u00C2': 'B', '\u00C3': 'C', '\u00C4': 'D', '\u00C5': 'E',
      '\u00C6': 'F', '\u00C7': 'G', '\u00C8': 'H', '\u00C9': 'I', '\u00D1': 'J',
      '\u00D2': 'K', '\u00D3': 'L', '\u00D4': 'M', '\u00D5': 'N', '\u00D6': 'O',
      '\u00D7': 'P', '\u00D8': 'Q', '\u00D9': 'R', '\u00E2': 'S', '\u00E3': 'T',
      '\u00E4': 'U', '\u00E5': 'V', '\u00E6': 'W', '\u00E7': 'X', '\u00E8': 'Y',
      '\u00E9': 'Z', '\u0081': 'a', '\u0082': 'b', '\u0083': 'c', '\u0084': 'd',
      '\u0085': 'e', '\u0086': 'f', '\u0087': 'g', '\u0088': 'h', '\u0089': 'i',
      '\u0091': 'j', '\u0092': 'k', '\u0093': 'l', '\u0094': 'm', '\u0095': 'n',
      '\u0096': 'o', '\u0097': 'p', '\u0098': 'q', '\u0099': 'r', '\u00A2': 's',
      '\u00A3': 't', '\u00A4': 'u', '\u00A5': 'v', '\u00A6': 'w', '\u00A7': 'x',
      '\u00A8': 'y', '\u00A9': 'z', '\u00F0': '0', '\u00F1': '1', '\u00F2': '2',
      '\u00F3': '3', '\u00F4': '4', '\u00F5': '5', '\u00F6': '6', '\u00F7': '7',
      '\u00F8': '8', '\u00F9': '9', '\u004B': '.', '\u004C': '<',
      '\u004D': '(', '\u004E': '+', '\u007C': '|', '\u0026': '&', '\u0021': '!',
      '\u0024': '$', '\u002A': '*', '\u0029': ')', '\u003B': ';', '\u005E': '^',
      '\u002D': '-', '\u002F': '/', '\u002C': ',', '\u0025': '%',
      '\u005F': '_', '\u003E': '>', '\u003F': '?', '\u0060': '`', '\u003A': ':',
      '\u0023': '#', '\u0040': '@', '\u0027': "'", '\u003D': '=', '\u0022': '"',
    };

    let result = '';
    for (let i = 0; i < ebcdicString.length; i++) {
      const char = ebcdicString[i];
      result += ebcdicMap[char] || char;
    }
    return result;
  };

  // Classify file type based on extension and content
  const classifyFile = (fileName: string, content: string): FileInfo['type'] => {
    const ext = fileName.toLowerCase().split('.').pop();
    const upperContent = content.toUpperCase();

    if (ext === 'cob' || ext === 'cobol' || upperContent.includes('IDENTIFICATION DIVISION')) {
      return 'COBOL';
    }
    if (ext === 'cpy' || ext === 'copy' || upperContent.includes('COPY ')) {
      return 'COPYBOOK';
    }
    if (ext === 'cl' || ext === 'cle' || upperContent.includes('PGM ') || upperContent.includes('CALL ')) {
      return 'CL';
    }
    if (ext === 'smed' || upperContent.includes('SMED')) {
      return 'SMED';
    }
    return 'UNKNOWN';
  };

  // Detect if content is EBCDIC encoded
  const isEBCDIC = (content: string): boolean => {
    // Simple heuristic: check for EBCDIC-specific byte patterns
    const hasEBCDICChars = /[\u0081-\u0089\u0091-\u0099\u00A2-\u00A9\u00C1-\u00C9\u00D1-\u00D9\u00E2-\u00E9\u00F0-\u00F9]/.test(content);
    return hasEBCDICChars;
  };

  const processFiles = (fileList: FileList | File[]) => {
    const supportedExtensions = ['.cob', '.cobol', '.cpy', '.copy', '.cl', '.cle', '.smed', '.txt'];
    
    Array.from(fileList).forEach(file => {
      // Check if file has supported extension
      const hasValidExtension = supportedExtensions.some(ext => 
        file.name.toLowerCase().endsWith(ext)
      );
      
      // Also allow files without extension (for CL files)
      const hasNoExtension = !file.name.includes('.');
      
      if (hasValidExtension || hasNoExtension) {
        const reader = new FileReader();
        reader.onload = (e) => {
          const content = e.target?.result as string;
          const fileType = classifyFile(file.name, content);
          const encoding = isEBCDIC(content) ? 'EBCDIC' : 'ASCII';
          
          const newFile: FileInfo = {
            name: file.name,
            type: fileType,
            size: file.size,
            originalEncoding: encoding,
            converted: false,
            content: content,
          };
          
          setFiles(prev => {
            // Avoid duplicates
            if (prev.some(f => f.name === file.name)) {
              return prev;
            }
            return [...prev, newFile];
          });
        };
        reader.readAsText(file);
      }
    });
  };

  const handleFileUpload = (event: React.ChangeEvent<HTMLInputElement>) => {
    const uploadedFiles = event.target.files;
    if (uploadedFiles) {
      processFiles(uploadedFiles);
    }

    if (event.target) {
      event.target.value = '';
    }
  };


  const handleDragOver = (e: React.DragEvent<HTMLDivElement>) => {
    e.preventDefault();
    e.stopPropagation();
    setIsDragOver(true);
  };

  const handleDragLeave = (e: React.DragEvent<HTMLDivElement>) => {
    e.preventDefault();
    e.stopPropagation();
    setIsDragOver(false);
  };

  const handleDrop = (e: React.DragEvent<HTMLDivElement>) => {
    e.preventDefault();
    e.stopPropagation();
    setIsDragOver(false);

    const items = Array.from(e.dataTransfer.items);
    const files: File[] = [];

    // Handle dropped items
    const promises = items.map(async (item) => {
      if (item.kind === 'file') {
        const entry = item.webkitGetAsEntry();
        if (entry) {
          await traverseFileTree(entry, files);
        }
      }
    });

    Promise.all(promises).then(() => {
      if (files.length > 0) {
        processFiles(files);
      }
    });
  };

  const traverseFileTree = async (item: any, files: File[]): Promise<void> => {
    return new Promise((resolve) => {
      if (item.isFile) {
        item.file((file: File) => {
          files.push(file);
          resolve();
        });
      } else if (item.isDirectory) {
        const dirReader = item.createReader();
        dirReader.readEntries(async (entries: any[]) => {
          const promises = entries.map(entry => traverseFileTree(entry, files));
          await Promise.all(promises);
          resolve();
        });
      } else {
        resolve();
      }
    });
  };

  const handleDirectoryPathLoad = async () => {
    if (!directoryPath.trim()) {
      alert(t('aiTransform.directoryPathRequired'));
      return;
    }

    setIsLoadingDirectory(true);
    
    try {
      // Load actual files from directory path
      const realFiles = await loadRealDirectoryFiles(directoryPath.trim());
      
      if (realFiles.length > 0) {
        const newFiles: FileInfo[] = [];
        
        realFiles.forEach((fileData: any) => {
          const fileType = classifyFile(fileData.name, fileData.content);
          const encoding = isEBCDIC(fileData.content) ? 'EBCDIC' : 'ASCII';
          
          const newFile: FileInfo = {
            name: fileData.name,
            type: fileType,
            size: fileData.content.length,
            originalEncoding: encoding,
            converted: false,
            content: fileData.content,
          };
          
          newFiles.push(newFile);
        });
        
        // Add all new files at once to avoid duplicates
        setFiles(prev => {
          const existingNames = new Set(prev.map(f => f.name));
          const filteredNewFiles = newFiles.filter(f => !existingNames.has(f.name));
          return [...prev, ...filteredNewFiles];
        });
        
        alert(t('aiTransform.pathAdded', { path: directoryPath.trim(), count: newFiles.length.toString() }));
      } else {
        alert(t('aiTransform.noSupportedFiles', { path: directoryPath.trim() }));
      }
    } catch (error) {
      console.error('Directory scan error:', error);
      alert(t('aiTransform.directoryLoadError', { error: String(error) }));
    } finally {
      setIsLoadingDirectory(false);
    }
  };
  
  const loadRealDirectoryFiles = async (directoryPath: string): Promise<any[]> => {
    // Load actual files from server via Python Flask API
    try {
      const response = await fetch('http://localhost:3003/scan-directory', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({ path: directoryPath })
      });
      
      if (response.ok) {
        const data = await response.json();
        return data.files || [];
      } else {
        const errorData = await response.json();
        throw new Error(errorData.error || 'Failed to scan directory');
      }
    } catch (error) {
      console.error('Error loading directory files:', error);
      throw error;
    }
  };

  const loadMockDirectoryFiles = async (directoryPath: string): Promise<any[]> => {
    // DEPRECATED: This function should not be used - use loadRealDirectoryFiles instead
    // Keeping for backward compatibility only
    
    // Mock implementation - return sample files based on directory path
    const sampleFiles: { [key: string]: any[] } = {
      '/data/assets/SRC1.COBLIB': [
        {
          name: 'ASPAC00',
          content: `       IDENTIFICATION DIVISION.\n       PROGRAM-ID. ASPAC00.\n       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n       01  WS-MESSAGE PIC X(50).\n       PROCEDURE DIVISION.\n           DISPLAY 'プログラム開始　Program Start'.\n           MOVE 'ASPAC00 Processing' TO WS-MESSAGE.\n           DISPLAY WS-MESSAGE.\n           GOBACK.`
        },
        {
          name: 'ASPAC01', 
          content: `       IDENTIFICATION DIVISION.\n       PROGRAM-ID. ASPAC01.\n       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n           COPY CT1511MS.\n       01  WS-RETCODE PIC 9(4).\n       PROCEDURE DIVISION.\n           DISPLAY 'データ処理中　Data　Processing'.\n           CALL 'ASPAC02' USING WS-RETCODE.\n           CALL 'CLEANUP' USING WS-RETCODE.\n           GOBACK.`
        },
        {
          name: 'ASPAC02',
          content: `       IDENTIFICATION DIVISION.\n       PROGRAM-ID. ASPAC02.\n       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n           COPY CT1510MS.\n       01  WS-STATUS PIC X(10).\n       PROCEDURE DIVISION.\n           DISPLAY 'トランザクション開始　Transaction　Start'.\n           MOVE 'ACTIVE' TO WS-STATUS.\n           CALL 'ASPAC03'.\n           GOBACK.`
        },
        {
          name: 'ASPAC03',
          content: `       IDENTIFICATION DIVISION.\n       PROGRAM-ID. ASPAC03.\n       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n       01  WS-COUNT PIC 9(5).\n       PROCEDURE DIVISION.\n           DISPLAY 'バックアップ実行　Backup　Execution'.\n           MOVE 100 TO WS-COUNT.\n           GOBACK.`
        },
        {
          name: 'CLEANUP',
          content: `       IDENTIFICATION DIVISION.\n       PROGRAM-ID. CLEANUP.\n       PROCEDURE DIVISION.\n           DISPLAY 'システム終了　System　Shutdown'.\n           GOBACK.`
        }
      ],
      '/data/assets/SRC.CLLIB': [
        {
          name: 'BATCH000',
          content: `PGM (BATCH000)\nVAR RETCODE,INTEGER\nVAR MSG,STRING\nVAR STATUS,INTEGER\n\n* CL Program: BATCH000 - バッチ処理　Batch　Processing\n\nDEFLIBL LIBL-TESTLIB\n\n* COBOLプログラム呼び出し　COBOL　Program　Call\nCALL PGM-ASPAC03.DEVLIB\nIF &RETCODE NE 0\n  GOTO ERROR_LABEL\nENDIF\n\nCALL PGM-ASPAC00.USERLIB\nCALL PGM-ASPAC02.DEVLIB\n\n* 条件処理　Conditional　Processing\nWHILE &STATUS EQ 0\n  CALL PGM-CHECK.TESTLIB\nENDWHILE\n\nERROR_LABEL:\nCALL PGM-ERRORLOG.TESTLIB\nCALL PGM-CLEANUP.TESTLIB\nRETURN`
        },
        {
          name: 'DAILY001',
          content: `PGM (DAILY001)\nVAR STATUS,INTEGER\nVAR RETCODE,INTEGER\n\n* データ処理中　Data　Processing\nDEFLIBL LIBL-PRODLIB\n\nCALL PGM-ASPAC01.PRODLIB\nCALL PGM-ASPAC02.PRODLIB\nCALL PGM-ASPAC03.PRODLIB\n\n* ファイル作成　File　Creation\nCRTFILE FILE-WORK001,SIZE-1024,ORG-@SF\n\nCALL PGM-CLEANUP.PRODLIB\nRETURN`
        },
        {
          name: 'MAINT002',
          content: `PGM (MAINT002)\nPARA INFILE,OUTFILE\nVAR STATUS,INTEGER\n\n* メンテナンス開始　Maintenance　Start\nDEFLIBL LIBL-SYSLIB\n\nCALL PGM-BACKUP.SYSLIB\nCALL PGM-CLEANUP.SYSLIB\n\n* ソート処理　Sort　Processing\nSORTD INFILE-INPUT01,INRL-250,INBF-6,\n      OUTFILE-OUTPUT01,OUTBF-6,\n      KEY-1|5|DA\n\nCALL PGM-ERRORLOG.SYSLIB\nRETURN`
        },
        {
          name: 'REPORT03',
          content: `PGM (REPORT03)\nVAR RETCODE,INTEGER\n\n* レポート処理　Report　Processing\nCALL PGM-RPTGEN.RPTLIB\nCALL PGM-ASPAC04.PRODLIB\n\n* 終了処理　Cleanup　Processing\nCALL PGM-ERRORLOG.SYSLIB\nRETURN`
        },
        {
          name: 'BACKUP05',
          content: `PGM (BACKUP05)\nVAR RETCODE,INTEGER\n\n* バックアップ実行　Backup　Execution\nDEFLIBL LIBL-BACKLIB\n\nCALL PGM-ASPAC09.BACKLIB\nCALL PGM-ASPAC0X.BACKLIB\nCALL PGM-CT1510.SYSLIB\n\nCALL PGM-CLEANUP.BACKLIB\nRETURN`
        }
      ],
      '/data/assets/DEMO/COBOL': [
        {
          name: 'CBACC000',
          content: `       IDENTIFICATION DIVISION.\n       PROGRAM-ID. CBACC000.\n       PROCEDURE DIVISION.\n           DISPLAY 'プログラム開始　Program Start'.\n           GOBACK.`
        },
        {
          name: 'CBCUS001',
          content: `       IDENTIFICATION DIVISION.\n       PROGRAM-ID. CBCUS001.\n       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n           COPY CPCUS002.\n       PROCEDURE DIVISION.\n           DISPLAY 'カスタマー処理　Customer　Processing'.\n           CALL 'CBVAL013'.\n           GOBACK.`
        }
      ],
      '/data/assets/DEMO/COPYBOOK': [
        {
          name: 'CPCUS002',
          content: `      * COPYBOOK: CPCUS002\n      * Generated for OpenASP AX Demo\n       01  CPCUS002-RECORD.\n           05  CUSTOMER-ID     PIC X(10).\n           05  CUSTOMER-NAME   PIC X(30).\n           05  CUSTOMER-ADDR   PIC X(50).`
        },
        {
          name: 'CPVAL013',
          content: `      * COPYBOOK: CPVAL013\n      * Validation Record Structure\n       01  CPVAL013-RECORD.\n           05  VALID-FLAG      PIC X(1).\n           05  ERROR-CODE      PIC 9(4).\n           05  ERROR-MSG       PIC X(80).`
        }
      ]
    };
    
    // Find matching directory or return empty array
    for (const [path, files] of Object.entries(sampleFiles)) {
      if (directoryPath.includes(path) || path.includes(directoryPath)) {
        return files;
      }
    }
    
    // If no exact match, return a mix of sample files for demo
    if (directoryPath.includes('sample') || directoryPath.includes('test')) {
      return [
        ...sampleFiles['/data/assets/SRC.CLLIB'].slice(0, 2),
        ...sampleFiles['/data/assets/SRC1.COBLIB'].slice(0, 3),
        ...sampleFiles['/data/assets/DEMO/COPYBOOK']
      ];
    }
    
    return [];
  };

  const handlePredefinedPath = async (path: string) => {
    // Load files from the predefined path and add them to existing files
    try {
      setIsLoadingDirectory(true);
      
      const realFiles = await loadRealDirectoryFiles(path);
      
      if (realFiles.length > 0) {
        const newFiles: FileInfo[] = [];
        
        realFiles.forEach((fileData: any) => {
          const newFile: FileInfo = {
            name: fileData.name,
            type: fileData.type as FileInfo['type'],
            size: fileData.size,
            originalEncoding: fileData.encoding as 'EBCDIC' | 'ASCII',
            converted: false,
            content: fileData.content,
          };
          
          newFiles.push(newFile);
        });
        
        // Add all new files to existing files (avoid duplicates)
        setFiles(prev => {
          const existingNames = new Set(prev.map(f => f.name));
          const filteredNewFiles = newFiles.filter(f => !existingNames.has(f.name));
          return [...prev, ...filteredNewFiles];
        });
        
        // Also append to directory path for visual feedback with semicolon separator
        setDirectoryPath(prev => {
          if (prev.trim() === '') {
            return path;
          } else {
            return prev + '; ' + path;
          }
        });
        
        // Remove popup feedback - just silently add files
      }
    } catch (error) {
      console.error('Error loading predefined path:', error);
    } finally {
      setIsLoadingDirectory(false);
    }
  };
  
  const generateCallTree = (rootProgram: string, visited: Set<string> = new Set()): CallTreeNode => {
    const isRepeated = visited.has(rootProgram);
    const isNotFound = !analysisResult?.allPrograms.has(rootProgram) && rootProgram !== 'ERRORLOG' && rootProgram !== 'CLEANUP';
    
    if (isRepeated) {
      return {
        name: rootProgram,
        type: 'COBOL',
        isRepeated: true,
        isNotFound: false,
        children: []
      };
    }
    
    const newVisited = new Set(visited);
    newVisited.add(rootProgram);
    
    // Find calls made by this program (can be CL or COBOL)
    const program = analysisResult?.clPrograms.find(prog => prog.name === rootProgram);
    const children: CallTreeNode[] = [];
    
    if (program) {
      program.calls.forEach(calledProgram => {
        const childNode = generateCallTree(calledProgram, newVisited);
        children.push(childNode);
      });
    }
    
    return {
      name: rootProgram,
      type: program ? program.type : 'COBOL',
      isRepeated: false,
      isNotFound,
      children
    };
  };
  
  const handleCLProgramSelect = async (clProgram: string) => {
    setSelectedCLProgram(clProgram);
    
    console.log(`🔍 Analyzing call tree for selected program: ${clProgram}`);
    
    try {
      // Initialize COBOL Call Tree Analyzer
      const analyzer = new CobolCallTreeAnalyzer();
      
      // Find the selected program file
      const selectedFile = files.find(file => {
        const fileName = file.name.toUpperCase();
        const programName = clProgram.toUpperCase();
        
        // Direct match
        if (fileName === programName) return true;
        
        // Match with common extensions
        if (fileName === `${programName}.cob` || 
            fileName === `${programName}.cobol` ||
            fileName === `${programName}.cl` ||
            fileName === `${programName}.cle`) return true;
        
        // Match without extension
        const fileNameWithoutExt = fileName.replace(/\.(cob|cobol|cl|cle)$/i, '');
        if (fileNameWithoutExt === programName) return true;
        
        return false;
      });
      
      if (!selectedFile || !selectedFile.content) {
        console.warn(`Selected program ${clProgram} not found or has no content`);
        setCallTree({
          name: clProgram,
          type: 'COBOL',
          isRepeated: false,
          isNotFound: true,
          children: [],
          calls: []
        });
        return;
      }
      
      // Add the selected program first
      analyzer.addProgram(selectedFile.name, selectedFile.content, selectedFile.type as 'COBOL' | 'CL');
      console.log(`📥 Added root program: ${selectedFile.name}`);
      
      // Extract CALL statements from the selected program to get related programs
      const selectedAnalyzer = new CobolCallTreeAnalyzer();
      selectedAnalyzer.addProgram(selectedFile.name, selectedFile.content, selectedFile.type as 'COBOL' | 'CL');
      const preliminaryResult = selectedAnalyzer.analyzeCallTree();
      
      // Get all programs called by the selected program (recursively)
      const calledPrograms = new Set<string>();
      const processedPrograms = new Set<string>();
      
      const findCalledProgramsRecursively = (currentProgram: string, depth: number = 0) => {
        if (depth > 5 || processedPrograms.has(currentProgram)) return; // Prevent infinite recursion
        processedPrograms.add(currentProgram);
        
        // Find the program file
        const programFile = files.find(file => {
          const fileName = file.name.toUpperCase();
          const programName = currentProgram.toUpperCase().replace(/\.[A-Z0-9]+$/i, '');
          
          if (fileName === programName) return true;
          if (fileName === `${programName}.cob` || fileName === `${programName}.cobol` ||
              fileName === `${programName}.cl` || fileName === `${programName}.cle`) return true;
          
          const fileNameWithoutExt = fileName.replace(/\.(cob|cobol|cl|cle)$/i, '');
          if (fileNameWithoutExt === programName) return true;
          
          return false;
        });
        
        if (programFile && programFile.content) {
          // Analyze this program's calls
          const tempAnalyzer = new CobolCallTreeAnalyzer();
          tempAnalyzer.addProgram(programFile.name, programFile.content, programFile.type as 'COBOL' | 'CL');
          const tempResult = tempAnalyzer.analyzeCallTree();
          
          tempResult.allCalls.forEach(call => {
            const callee = call.calleeProgram.toUpperCase();
            calledPrograms.add(callee);
            findCalledProgramsRecursively(callee, depth + 1);
          });
        }
      };
      
      // Start with programs called directly by the selected program
      preliminaryResult.allCalls.forEach(call => {
        const callee = call.calleeProgram.toUpperCase();
        calledPrograms.add(callee);
        findCalledProgramsRecursively(callee, 1);
      });
      
      console.log(`🔍 Found ${calledPrograms.size} programs called by ${clProgram} (recursive):`, Array.from(calledPrograms));
      
      // Only load the called programs (not all 777 files)
      let loadedCount = 1; // Already loaded the root program
      calledPrograms.forEach(programName => {
        const programFile = files.find(file => {
          const fileName = file.name.toUpperCase();
          const cleanProgramName = programName.replace(/\.[A-Z0-9]+$/i, ''); // Remove library name
          
          // Direct match
          if (fileName === cleanProgramName) return true;
          
          // Match with common extensions
          if (fileName === `${cleanProgramName}.cob` || 
              fileName === `${cleanProgramName}.cobol` ||
              fileName === `${cleanProgramName}.cl` ||
              fileName === `${cleanProgramName}.cle`) return true;
          
          // Match without extension
          const fileNameWithoutExt = fileName.replace(/\.(cob|cobol|cl|cle)$/i, '');
          if (fileNameWithoutExt === cleanProgramName) return true;
          
          return false;
        });
        
        if (programFile && programFile.content) {
          analyzer.addProgram(programFile.name, programFile.content, programFile.type as 'COBOL' | 'CL');
          console.log(`  ✅ Added called program: ${programFile.name}`);
          loadedCount++;
        } else {
          console.log(`  ❌ Called program not found: ${programName}`);
        }
      });
      
      console.log(`📥 Loaded ${loadedCount} related programs (not all ${files.filter(f => f.type === 'COBOL' || f.type === 'CL').length})`);
      
      // Perform call tree analysis
      const result = analyzer.analyzeCallTree();
      
      console.log(`📊 Analysis complete: ${result.allCalls.length} calls, ${result.missingPrograms.length} missing`);
      
      // Find the root node for our selected program
      const normalizedProgram = clProgram.toUpperCase().replace(/\.(cob|cobol|cl|cle)$/i, '');
      let targetNode = result.rootNodes.find((node: any) => 
        node.name.toUpperCase() === normalizedProgram ||
        node.name.toUpperCase() === clProgram.toUpperCase()
      );
      
      if (targetNode) {
        console.log('🔍 Root node found for', clProgram, ':', targetNode);
        console.log('🔍 Root node children:', targetNode.children);
        const tree = convertToCallTreeNode(targetNode);
        setCallTree(tree);
        console.log(`✅ Call tree set for ${clProgram}`);
      } else {
        // If not a root node, create a simple tree showing what this program calls
        const calls = result.allCalls.filter((call: any) => 
          call.callerProgram.toUpperCase() === normalizedProgram ||
          call.callerProgram.toUpperCase() === clProgram.toUpperCase()
        );
        
        setCallTree({
          name: clProgram,
          type: selectedFile.type === 'CL' ? 'CL' : 'COBOL',
          isRepeated: false,
          isNotFound: false,
          children: calls.map(call => ({
            name: call.calleeProgram,
            type: 'COBOL',
            isRepeated: false,
            isNotFound: result.missingPrograms.includes(call.calleeProgram),
            children: [],
            calls: []
          })),
          calls: []
        });
        console.log(`✅ Created simple call tree for ${clProgram} with ${calls.length} calls`);
      }
      
    } catch (error) {
      console.error(`❌ Error analyzing call tree for ${clProgram}:`, error);
      setCallTree(null);
    }
  };

  // Convert from CobolCallTreeAnalyzer result to CallTreeNode format
  const convertToCallTreeNode = (node: any): CallTreeNode => {
    console.log(`🔍 Converting node: ${node.name}, isFound: ${node.isFound}, type: ${node.type}`);
    return {
      name: node.name,
      type: node.type,
      isRepeated: node.cyclic || false,
      isNotFound: !node.isFound,
      children: node.children.map((child: any) => convertToCallTreeNode(child)),
      calls: node.calls?.map((call: any) => call.calleeProgram) || [],
      cyclic: node.cyclic
    };
  };
  
  const renderCallTreeNode = (node: CallTreeNode, prefix: string = '', isLast: boolean = true, level: number = 0, parentPath: string = ''): React.JSX.Element[] => {
    const elements: React.JSX.Element[] = [];
    const connector = isLast ? '└── ' : '├── ';
    const displayName = `${node.name}${node.isRepeated ? ' (R)' : ''}${node.isNotFound ? ' (NOF)' : ''}`;
    
    // Create a unique key using parent path and node position
    const uniqueKey = `${parentPath}/${node.name}-${level}-${prefix.length}`;
    
    const typeColor = {
      CL: 'text-purple-600 dark:text-purple-400',
      COBOL: 'text-blue-600 dark:text-blue-400',
      COPYBOOK: 'text-green-600 dark:text-green-400'
    }[node.type] || 'text-gray-600 dark:text-gray-400';
    
    elements.push(
      <div key={uniqueKey} className="font-mono text-sm">
        <span className="text-gray-400 dark:text-gray-500">{prefix}{connector}</span>
        <span className={`${typeColor} ${node.isNotFound ? 'bg-red-100 dark:bg-red-900' : ''} ${node.isRepeated ? 'bg-yellow-100 dark:bg-yellow-900' : ''} px-1 rounded`}>
          {displayName}
        </span>
      </div>
    );
    
    if (!node.isRepeated && node.children.length > 0) {
      const newPrefix = prefix + (isLast ? '    ' : '│   ');
      node.children.forEach((child, index) => {
        const isChildLast = index === node.children.length - 1;
        const childPath = `${parentPath}/${node.name}`;
        elements.push(...renderCallTreeNode(child, newPrefix, isChildLast, level + 1, childPath));
      });
    }
    
    return elements;
  };

  const handleConvertSelected = async () => {
    if (selectedFiles.length === 0) {
      alert(t('aiTransform.selectFiles'));
      return;
    }

    console.log('Starting analysis for files:', selectedFiles);
    setIsProcessing(true);
    setShowNeuralNetwork(true);
    console.log('Neural network animation started');
    
    // Skip global Call Tree analysis to prevent UI freezing
    // Call Tree analysis is now performed dynamically when a specific program is selected
    console.log('Skipping global Call Tree analysis for performance - will analyze per program when selected');
    
    // Simulate processing time for animation without heavy computation
    setTimeout(() => {
      // Create empty call tree result to avoid breaking existing code
      const emptyCallTreeResult = {
        rootNodes: [],
        allCalls: [],
        missingPrograms: [],
        cyclicReferences: [],
        printCallTree: () => 'Call Tree will be generated when a specific program is selected'
      };
      
      console.log('Analysis simulation completed - no global Call Tree generated');
      
      // Store empty result for compatibility
      (window as any).callTreeResult = emptyCallTreeResult;
      
      // Trigger completion
      handleAnalysisComplete();
    }, 1500);
  };
  
  const handleAnalysisComplete = async () => {
    console.log('Analysis complete, closing neural network animation');
    setShowNeuralNetwork(false);
    
    // Get the stored call tree result
    const callTreeResult = (window as any).callTreeResult;
    if (!callTreeResult) {
      console.error('No call tree result available');
      setIsProcessing(false);
      return;
    }
    
    // Detailed analysis results based on selected files
    const selectedFileTypes = files.filter(f => selectedFiles.includes(f.name));
    const clFiles = selectedFileTypes.filter(f => f.type === 'CL');
    const cobolFiles = selectedFileTypes.filter(f => f.type === 'COBOL');
    const copybookFiles = selectedFileTypes.filter(f => f.type === 'COPYBOOK');
    
    // Convert selected files
    const updatedFiles = files.map(file => {
      if (selectedFiles.includes(file.name)) {
        const convertedContent = file.originalEncoding === 'EBCDIC' 
          ? ebcdicToAscii(file.content || '')
          : file.content;
        
        return {
          ...file,
          converted: true,
          convertedContent
        };
      }
      return file;
    });
    
    // Create programs with empty calls info (calls will be analyzed dynamically per program)
    const allProgramsWithCalls = [...clFiles, ...cobolFiles].map(file => {      
      return {
        name: file.name,
        type: (file.type === 'CL' ? 'CL' : 'COBOL') as 'CL' | 'COBOL',
        calls: [], // Will be populated dynamically when program is selected
        libraries: [] as string[]
      };
    });
    
    // Create set of all available programs
    const allPrograms = new Set<string>();
    [...clFiles, ...cobolFiles, ...copybookFiles].forEach(file => {
      allPrograms.add(file.name);
    });
    
    const analysisResult: AnalysisResult = {
      summary: {
        totalFiles: selectedFiles.length,
        byType: {
          'CL': clFiles.length,
          'COBOL': cobolFiles.length,
          'COPYBOOK': copybookFiles.length,
          'SMED': selectedFileTypes.filter(f => f.type === 'SMED').length,
          'UNKNOWN': selectedFileTypes.filter(f => f.type === 'UNKNOWN').length,
        },
        dependencies: {
          totalCalls: 0, // Will be calculated dynamically when programs are selected
          missingPrograms: 0, // Will be calculated dynamically when programs are selected  
          circularDependencies: 0 // Will be calculated dynamically when programs are selected
        }
      },
      clPrograms: allProgramsWithCalls,
      allPrograms,
      detailedResults: selectedFileTypes.map(file => ({
        fileName: file.name,
        fileType: file.type,
        size: file.size,
        encoding: file.originalEncoding,
        hasJapaneseChars: file.content?.includes('プログラム') || file.content?.includes('データ') || false
      }))
    };

    setFiles(updatedFiles);
    setAnalysisResult(analysisResult);
    setIsProcessing(false);
    setSelectedFiles([]);
    setSelectedCLProgram(null);
    setCallTree(null);
    console.log('Analysis result set:', analysisResult);
    console.log('Global Call Tree analysis skipped - Call Tree will be generated dynamically when specific programs are selected');
  };

  const exportToExcel = () => {
    const csvContent = [
      'File Name,Type,Size,Original Encoding,Converted,Conversion Status',
      ...files.map(file => 
        `${file.name},${file.type},${file.size},${file.originalEncoding},${file.converted},${file.converted ? 'Success' : 'Pending'}`
      )
    ].join('\n');
    
    const blob = new Blob([csvContent], { type: 'text/csv' });
    const url = window.URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = 'file_analysis.csv';
    a.click();
    window.URL.revokeObjectURL(url);
  };

  const resetAnalysis = () => {
    // 분석 결과 초기화
    setAnalysisResult(null);
    setSelectedFiles([]);
    setSelectedCLProgram(null);
    setCallTree(null);
    setIsProcessing(false);
    setShowNeuralNetwork(false);
    
    // 파일들의 변환 상태 초기화
    const resetFiles = files.map(file => ({
      ...file,
      converted: false,
      convertedContent: undefined
    }));
    setFiles(resetFiles);
    
    console.log('Analysis results reset');
  };

  const fileTypeColors = {
    COBOL: 'bg-blue-100 text-blue-800 dark:bg-blue-900 dark:text-blue-200',
    COPYBOOK: 'bg-green-100 text-green-800 dark:bg-green-900 dark:text-green-200',
    CL: 'bg-purple-100 text-purple-800 dark:bg-purple-900 dark:text-purple-200',
    SMED: 'bg-orange-100 text-orange-800 dark:bg-orange-900 dark:text-orange-200',
    UNKNOWN: 'bg-gray-100 text-gray-800 dark:bg-gray-900 dark:text-gray-200',
  };

  const getFileTypeStats = () => {
    const stats = files.reduce((acc, file) => {
      acc[file.type] = (acc[file.type] || 0) + 1;
      return acc;
    }, {} as Record<string, number>);
    return stats;
  };

  return (
    <div className="h-full p-8">
      <div className="mb-8">
        <h1 className="text-2xl font-bold text-gray-900 dark:text-white mb-2">
          {t('aiTransform.title')}
        </h1>
        <p className="text-gray-600 dark:text-gray-400">
          {t('aiTransform.subtitle')}
        </p>
      </div>

      {/* File Upload Section */}
      <div className="bg-white dark:bg-gray-800 rounded-lg shadow-sm p-6 mb-6 border border-gray-200 dark:border-gray-700">
        <h3 className="text-lg font-semibold text-gray-900 dark:text-white mb-4">
          {t('aiTransform.fileUploadTitle')}
        </h3>
        
        {/* Drag and Drop Area */}
        <div
          onDragOver={handleDragOver}
          onDragLeave={handleDragLeave}
          onDrop={handleDrop}
          className={`relative border-2 border-dashed rounded-lg p-8 mb-4 transition-all ${
            isDragOver
              ? 'border-blue-400 bg-blue-50 dark:bg-blue-900/20'
              : 'border-gray-300 dark:border-gray-600 hover:border-blue-300 dark:hover:border-blue-500'
          }`}
        >
          <div className="text-center">
            <DocumentArrowUpIcon className="w-12 h-12 text-gray-400 mx-auto mb-4" />
            <h4 className="text-lg font-medium text-gray-900 dark:text-white mb-2">
              {isDragOver ? t('aiTransform.dragDropHover') : t('aiTransform.dragDropText')}
            </h4>
            <p className="text-gray-500 dark:text-gray-400 mb-2">
              {t('aiTransform.supportedFiles')}
            </p>
            <p className="text-sm text-blue-600 dark:text-blue-400 mb-4">
              {t('aiTransform.multipleFolderHint')}
            </p>
            
            <div className="space-y-4">
              <div className="flex gap-4 justify-center">
                <input
                  type="file"
                  ref={fileInputRef}
                  onChange={handleFileUpload}
                  multiple
                  accept=".cob,.cobol,.cpy,.copy,.cl,.cle,.smed,.txt"
                  className="hidden"
                />
                
                <button
                  onClick={() => fileInputRef.current?.click()}
                  className="flex items-center px-4 py-2 bg-blue-600 hover:bg-blue-700 text-white rounded-lg transition-colors"
                >
                  <DocumentArrowUpIcon className="w-4 h-4 mr-2" />
                  {t('aiTransform.selectFiles')}
                </button>
                
                <button
                  onClick={() => {
                    const input = document.createElement('input');
                    input.type = 'file';
                    input.webkitdirectory = true;
                    input.onchange = (e) => {
                      const files = (e.target as HTMLInputElement).files;
                      if (files && files.length > 0) {
                        // Get folder name from first file path
                        const folderName = files[0].webkitRelativePath.split('/')[0];
                        console.log(`Adding folder: ${folderName} with ${files.length} files`);
                        
                        processFiles(files);
                        
                        // Show success message in console and potentially update UI state
                        console.log(`フォルダ "${folderName}" から ${files.length} 個のファイルを追加しました。複数のフォルダを選択する場合は、このボタンを再度クリックしてください。`);
                      }
                    };
                    input.click();
                  }}
                  className="flex items-center px-4 py-2 bg-purple-600 hover:bg-purple-700 text-white rounded-lg transition-colors"
                >
                  <FolderOpenIcon className="w-4 h-4 mr-2" />
                  {t('aiTransform.selectFolder')}
                </button>
              </div>
              
              {/* Directory Path Input */}
              <div className="border-t border-gray-200 dark:border-gray-600 pt-4">
                <p className="text-sm text-gray-600 dark:text-gray-400 mb-3">
                  {t('aiTransform.serverPathInput')}
                </p>
                
                <div className="flex gap-2 mb-3">
                  <input
                    type="text"
                    value={directoryPath}
                    onChange={(e) => setDirectoryPath(e.target.value)}
                    placeholder={t('aiTransform.pathPlaceholder')}
                    className="flex-1 px-3 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white placeholder-gray-500 dark:placeholder-gray-400 focus:ring-2 focus:ring-blue-500 focus:border-transparent"
                  />
                  <button
                    onClick={handleDirectoryPathLoad}
                    disabled={isLoadingDirectory || !directoryPath.trim()}
                    className="flex items-center px-4 py-2 bg-green-600 hover:bg-green-700 disabled:bg-gray-400 text-white rounded-lg transition-colors"
                  >
                    {isLoadingDirectory ? (
                      <ArrowPathIcon className="w-4 h-4 mr-2 animate-spin" />
                    ) : (
                      <MagnifyingGlassIcon className="w-4 h-4 mr-2" />
                    )}
                    {isLoadingDirectory ? t('aiTransform.scanning') : t('aiTransform.scan')}
                  </button>
                </div>
                
                {/* Predefined Directory Shortcuts */}
                <div className="flex flex-wrap gap-2">
                  <span className="text-xs text-gray-500 dark:text-gray-400 mr-2">{t('aiTransform.quickSelect')}</span>
                  {[
                    '/data/assets/SRC1.COBLIB',
                    '/data/assets/SRC1.CPYLIB', 
                    '/data/assets/SRC.CLLIB',
                    '/data/assets/DEMO/COBOL',
                    '/data/assets/DEMO/COPYBOOK',
                    '/data/COBOL2Java/samples'
                  ].map((path) => (
                    <button
                      key={path}
                      onClick={() => handlePredefinedPath(path)}
                      disabled={isLoadingDirectory}
                      className="text-xs px-2 py-1 bg-gray-100 dark:bg-gray-600 text-gray-700 dark:text-gray-300 rounded hover:bg-gray-200 dark:hover:bg-gray-500 disabled:bg-gray-300 dark:disabled:bg-gray-700 disabled:cursor-not-allowed transition-colors"
                    >
                      {isLoadingDirectory ? (
                        <span className="flex items-center">
                          <ArrowPathIcon className="w-3 h-3 mr-1 animate-spin" />
                          {t('aiTransform.loading')}
                        </span>
                      ) : (
                        path.split('/').pop()
                      )}
                    </button>
                  ))}
                </div>
              </div>
            </div>
          </div>
          
          {isDragOver && (
            <div className="absolute inset-0 bg-blue-500 bg-opacity-10 rounded-lg flex items-center justify-center">
              <div className="text-blue-600 font-semibold text-lg">
                {t('aiTransform.dropFiles')}
              </div>
            </div>
          )}
        </div>
        
        <div className="flex gap-4 mb-4">
          {files.length > 0 && (
            <div className="text-sm text-gray-600 dark:text-gray-400 mb-4">
              {t('aiTransform.totalFiles', { count: files.length.toString() })}
            </div>
          )}
          
          {files.length > 0 && (
            <>
              <button
                onClick={handleConvertSelected}
                disabled={selectedFiles.length === 0 || isProcessing}
                className="flex items-center px-4 py-2 bg-green-600 hover:bg-green-700 disabled:bg-gray-400 text-white rounded-lg transition-colors"
              >
                {isProcessing ? (
                  <MagnifyingGlassIcon className="w-4 h-4 mr-2 animate-pulse" />
                ) : (
                  <MagnifyingGlassIcon className="w-4 h-4 mr-2" />
                )}
                {isProcessing ? t('aiTransform.analyzing') : t('aiTransform.analyze')}
              </button>
              
              <button
                onClick={exportToExcel}
                className="flex items-center px-4 py-2 bg-orange-600 hover:bg-orange-700 text-white rounded-lg transition-colors"
              >
                <DocumentArrowDownIcon className="w-4 h-4 mr-2" />
                {t('aiTransform.exportExcel')}
              </button>
              
              {analysisResult && (
                <button
                  onClick={resetAnalysis}
                  className="flex items-center px-4 py-2 bg-red-600 hover:bg-red-700 text-white rounded-lg transition-colors"
                >
                  <ArrowPathIcon className="w-4 h-4 mr-2" />
                  분석정보 초기화
                </button>
              )}
            </>
          )}
        </div>

        {/* File Statistics */}
        {files.length > 0 && (
          <div className="grid grid-cols-2 md:grid-cols-5 gap-4 mb-6">
            {Object.entries(getFileTypeStats()).map(([type, count]) => (
              <div key={type} className="text-center">
                <div className={`px-3 py-2 rounded-lg ${fileTypeColors[type as keyof typeof fileTypeColors]}`}>
                  <div className="font-semibold">{count}</div>
                  <div className="text-sm">{type}</div>
                </div>
              </div>
            ))}
          </div>
        )}
      </div>

      {/* File List */}
      {files.length > 0 && (
        <div className="bg-white dark:bg-gray-800 rounded-lg shadow-sm border border-gray-200 dark:border-gray-700">
          <div className="p-4 border-b border-gray-200 dark:border-gray-700">
            <h3 className="text-lg font-semibold text-gray-900 dark:text-white">
              File Analysis Results
            </h3>
          </div>
          
          <div className="overflow-x-auto">
            <table className="w-full">
              <thead className="bg-gray-50 dark:bg-gray-700">
                <tr>
                  <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider">
                    <input
                      type="checkbox"
                      onChange={(e) => {
                        if (e.target.checked) {
                          setSelectedFiles(files.map(f => f.name));
                        } else {
                          setSelectedFiles([]);
                        }
                      }}
                      checked={selectedFiles.length === files.length && files.length > 0}
                    />
                  </th>
                  <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider">
                    File Name
                  </th>
                  <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider">
                    Type
                  </th>
                  <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider">
                    Size
                  </th>
                  <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider">
                    Encoding
                  </th>
                  <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider">
                    Status
                  </th>
                </tr>
              </thead>
              <tbody className="bg-white dark:bg-gray-800 divide-y divide-gray-200 dark:divide-gray-700">
                {files.map((file, index) => (
                  <tr key={index} className="hover:bg-gray-50 dark:hover:bg-gray-700">
                    <td className="px-4 py-4 whitespace-nowrap">
                      <input
                        type="checkbox"
                        checked={selectedFiles.includes(file.name)}
                        onChange={(e) => {
                          if (e.target.checked) {
                            setSelectedFiles(prev => [...prev, file.name]);
                          } else {
                            setSelectedFiles(prev => prev.filter(name => name !== file.name));
                          }
                        }}
                      />
                    </td>
                    <td className="px-4 py-4 whitespace-nowrap text-sm text-gray-900 dark:text-white">
                      {file.name}
                    </td>
                    <td className="px-4 py-4 whitespace-nowrap">
                      <span className={`px-2 py-1 text-xs font-medium rounded-full ${fileTypeColors[file.type]}`}>
                        {file.type}
                      </span>
                    </td>
                    <td className="px-4 py-4 whitespace-nowrap text-sm text-gray-500 dark:text-gray-400">
                      {(file.size / 1024).toFixed(1)} KB
                    </td>
                    <td className="px-4 py-4 whitespace-nowrap">
                      <span className={`px-2 py-1 text-xs font-medium rounded-full ${
                        file.originalEncoding === 'EBCDIC' 
                          ? 'bg-red-100 text-red-800 dark:bg-red-900 dark:text-red-200'
                          : 'bg-green-100 text-green-800 dark:bg-green-900 dark:text-green-200'
                      }`}>
                        {file.originalEncoding}
                      </span>
                    </td>
                    <td className="px-4 py-4 whitespace-nowrap">
                      {file.converted ? (
                        <span className="px-2 py-1 text-xs font-medium rounded-full bg-green-100 text-green-800 dark:bg-green-900 dark:text-green-200">
                          Converted
                        </span>
                      ) : file.originalEncoding === 'EBCDIC' ? (
                        <span className="px-2 py-1 text-xs font-medium rounded-full bg-yellow-100 text-yellow-800 dark:bg-yellow-900 dark:text-yellow-200">
                          Pending
                        </span>
                      ) : (
                        <span className="px-2 py-1 text-xs font-medium rounded-full bg-blue-100 text-blue-800 dark:bg-blue-900 dark:text-blue-200">
                          No conversion needed
                        </span>
                      )}
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </div>
      )}

      {/* Empty State */}
      {files.length === 0 && (
        <div className="bg-gray-50 dark:bg-gray-800 rounded-lg p-12 text-center border-2 border-dashed border-gray-300 dark:border-gray-600">
          <FolderOpenIcon className="w-12 h-12 text-gray-400 mx-auto mb-4" />
          <h3 className="text-lg font-medium text-gray-900 dark:text-white mb-2">
            No files uploaded
          </h3>
          <p className="text-gray-500 dark:text-gray-400 mb-4">
            Upload COBOL, COPYBOOK, CL, or SMED files to begin EBCDIC to ASCII conversion
          </p>
          <button
            onClick={() => fileInputRef.current?.click()}
            className="inline-flex items-center px-4 py-2 bg-blue-600 hover:bg-blue-700 text-white rounded-lg transition-colors"
          >
            <DocumentArrowUpIcon className="w-4 h-4 mr-2" />
            Choose Files
          </button>
        </div>
      )}
      
      {/* Neural Network Animation Popup */}
      <NeuralNetworkAnimation 
        isVisible={showNeuralNetwork} 
        onComplete={handleAnalysisComplete}
      />
      
      {/* File Analysis Results - Moved to top */}
      {analysisResult && (
        <div className="mb-8">
          <h3 className="text-xl font-bold text-gray-900 dark:text-white mb-6 flex items-center">
            <ChartPieIcon className="h-6 w-6 mr-2" />
            프로그램 분석 결과
          </h3>
          
          {/* Summary Statistics */}
          <div className="bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6 mb-6">
            <h4 className="text-lg font-semibold text-gray-900 dark:text-white mb-4">
              📊 분석 요약
            </h4>
            
            <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
              <div className="text-center p-4 bg-blue-50 dark:bg-blue-900/20 rounded-lg">
                <div className="text-2xl font-bold text-blue-600">{analysisResult.summary.totalFiles}</div>
                <div className="text-sm text-gray-600 dark:text-gray-400">전체 파일</div>
              </div>
              <div className="text-center p-4 bg-purple-50 dark:bg-purple-900/20 rounded-lg">
                <div className="text-2xl font-bold text-purple-600">{analysisResult.summary.byType.CL || 0}</div>
                <div className="text-sm text-gray-600 dark:text-gray-400">CL 파일</div>
              </div>
              <div className="text-center p-4 bg-green-50 dark:bg-green-900/20 rounded-lg">
                <div className="text-2xl font-bold text-green-600">{analysisResult.summary.dependencies.totalCalls}</div>
                <div className="text-sm text-gray-600 dark:text-gray-400">프로그램 호출</div>
              </div>
              <div className="text-center p-4 bg-red-50 dark:bg-red-900/20 rounded-lg">
                <div className="text-2xl font-bold text-red-600">{analysisResult.summary.dependencies.missingPrograms}</div>
                <div className="text-sm text-gray-600 dark:text-gray-400">누락 프로그램</div>
              </div>
            </div>
          </div>
          
          {/* CL Programs List and Call Tree */}
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
            {/* Programs List */}
            <div className="bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6">
              <h4 className="text-lg font-semibold text-gray-900 dark:text-white mb-4">
                📜 프로그램 목록 (CL & COBOL)
              </h4>
              
              <div className="space-y-2 max-h-80 overflow-y-auto">
                {analysisResult.clPrograms.length > 0 ? (
                  analysisResult.clPrograms.map((program, index) => (
                    <div 
                      key={index} 
                      onClick={() => handleCLProgramSelect(program.name)}
                      className={`p-3 rounded-lg cursor-pointer transition-all ${
                        selectedCLProgram === program.name 
                          ? 'bg-purple-100 dark:bg-purple-900/50 border-2 border-purple-300 dark:border-purple-600'
                          : 'bg-gray-50 dark:bg-gray-700 hover:bg-purple-50 dark:hover:bg-purple-900/20 border-2 border-transparent'
                      }`}
                    >
                      <div className="flex items-center justify-between">
                        <div>
                          <div className="flex items-center gap-2">
                            <span className="font-semibold text-gray-900 dark:text-white">
                              {program.name}
                            </span>
                            <span className={`text-xs px-1.5 py-0.5 rounded ${
                              program.type === 'CL' 
                                ? 'bg-purple-200 text-purple-800 dark:bg-purple-800 dark:text-purple-200'
                                : 'bg-blue-200 text-blue-800 dark:bg-blue-800 dark:text-blue-200'
                            }`}>
                              {program.type}
                            </span>
                          </div>
                          <div className="text-sm text-gray-600 dark:text-gray-400">
                            {program.calls.length}개 호출{program.type === 'CL' && program.libraries.length > 0 ? `, ${program.libraries.length}개 라이브러리` : ''}
                          </div>
                        </div>
                        <div className="text-purple-500">
                          {selectedCLProgram === program.name ? '▼' : '▶'}
                        </div>
                      </div>
                    </div>
                  ))
                ) : (
                  <div className="text-gray-500 dark:text-gray-400 text-center py-8">
                    CALL 문이 있는 프로그램이 없습니다
                  </div>
                )}
              </div>
            </div>
            
            {/* Call Tree Display */}
            <div className="bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6">
              <h4 className="text-lg font-semibold text-gray-900 dark:text-white mb-4">
                🌳 호출 트리 (Call Tree)
              </h4>
              
              <div className="max-h-80 overflow-y-auto">
                {callTree ? (
                  <div className="bg-black dark:bg-gray-900 text-green-400 p-4 rounded-lg font-mono text-sm">
                    <div className="text-gray-300 mb-2">
                      {selectedCLProgram}/
                    </div>
                    {renderCallTreeNode(callTree)}
                    <div className="mt-4 text-xs text-gray-500">
                      <div>(R) = 반복 호출 (NOF) = 찾을 수 없는 프로그램</div>
                    </div>
                  </div>
                ) : (
                  <div className="text-gray-500 dark:text-gray-400 text-center py-8">
                    프로그램을 선택하면 호출 트리가 표시됩니다
                  </div>
                )}
              </div>
            </div>
          </div>
        </div>
      )}
      
    </div>
  );
};

export default AITransformPage;