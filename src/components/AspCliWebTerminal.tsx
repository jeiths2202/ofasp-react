import React, { useState, useEffect, useRef, useCallback } from 'react';
import './AspCliWebTerminal.css';
import EdtfileBrowser from './EdtfileBrowser';
import SmedMapDisplay from './SmedMapDisplay';
import webSocketService from './websocketService';

interface AspCliWebTerminalProps {
  isDarkMode: boolean;
  workstationName?: string;
  user?: string;
}

interface CommandHistory {
  command: string;
  output: string;
  timestamp: Date;
  success: boolean;
}

interface SystemInfo {
  currentUser: string;
  currentVolume: string;
  currentLibrary: string;
  systemTime: string;
}

const AspCliWebTerminal: React.FC<AspCliWebTerminalProps> = ({ isDarkMode, workstationName, user }) => {
  const [commandHistory, setCommandHistory] = useState<CommandHistory[]>([]);
  const [currentCommand, setCurrentCommand] = useState('');
  const [isExecuting, setIsExecuting] = useState(false);
  const [historyIndex, setHistoryIndex] = useState(-1);
  const [systemInfo, setSystemInfo] = useState<SystemInfo>({
    currentUser: user || 'admin',
    currentVolume: '',
    currentLibrary: '',
    systemTime: new Date().toLocaleString()
  });
  
  const [availableVolumes, setAvailableVolumes] = useState<string[]>([]);
  const [availableLibraries, setAvailableLibraries] = useState<string[]>([]);
  const [showVolumeDropdown, setShowVolumeDropdown] = useState(false);
  
  // Simplified state for Hub architecture
  const [showLibraryDropdown, setShowLibraryDropdown] = useState(false);
  const [showEdtfileBrowser, setShowEdtfileBrowser] = useState(false);
  const [edtfileData, setEdtfileData] = useState<any>(null);
  const [showSmedMap, setShowSmedMap] = useState(false);
  const [smedMapData, setSmedMapData] = useState<any>(null);
  const [hubConnectionStatus, setHubConnectionStatus] = useState<string>('disconnected');
  
  const terminalRef = useRef<HTMLDivElement>(null);
  const inputRef = useRef<HTMLInputElement>(null);
  const [commandSuggestions] = useState([
    'HELP', 'CRTLIB', 'DLTLIB', 'WRKLIB', 'CRTFILE', 'DLTFILE', 
    'DSPFD', 'WRKOBJ', 'WRKVOL', 'WRKSPLF', 'WRKMSG',
    'DSPJOB', 'SAVLIB', 'RSTLIB', 'SNDMSG', 'RCVMSG', 'EDTFILE', 'CTTFILE',
    'CRTPGM', 'CRTMAP', 'CALL'
  ]);

  // 실제 Python API를 통해 볼륨 목록 로드
  const loadAvailableVolumes = async () => {
    try {
      // Python aspcli.py WRKVOL 명령 호출
      const response = await fetch('http://localhost:8000/api/asp-command', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({ 
          command: 'WRKVOL',
          user: systemInfo.currentUser 
        }),
      });

      let volumeOutput = '';
      if (response.ok) {
        const result = await response.json();
        volumeOutput = result.output || '';
      } else {
        // 백엔드가 없는 경우 시뮬레이션으로 폴백
        volumeOutput = await simulateCommand('WRKVOL');
      }
      
      const volumes = parseVolumesFromOutput(volumeOutput);
      setAvailableVolumes(volumes);
      
      // 첫 번째 볼륨을 기본값으로 설정
      if (volumes.length > 0 && !systemInfo.currentVolume) {
        setSystemInfo(prev => ({
          ...prev,
          currentVolume: volumes[0]
        }));
        // 첫 번째 볼륨의 라이브러리도 로드
        loadLibrariesForVolume(volumes[0]);
      }
    } catch (error) {
      console.error('Error loading volumes:', error);
      // 에러 시 시뮬레이션으로 폴백
      try {
        const volumeOutput = await simulateCommand('WRKVOL');
        const volumes = parseVolumesFromOutput(volumeOutput);
        setAvailableVolumes(volumes);
        if (volumes.length > 0 && !systemInfo.currentVolume) {
          setSystemInfo(prev => ({
            ...prev,
            currentVolume: volumes[0]
          }));
          loadLibrariesForVolume(volumes[0]);
        }
      } catch (fallbackError) {
        console.error('Fallback error:', fallbackError);
      }
    }
  };

  // 실제 Python API를 통해 라이브러리 목록 로드
  const loadLibrariesForVolume = async (volume: string) => {
    try {
      // Python aspcli.py WRKLIB 명령 호출
      const response = await fetch('http://localhost:8000/api/asp-command', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({ 
          command: 'WRKLIB',
          user: systemInfo.currentUser 
        }),
      });

      let libraryOutput = '';
      if (response.ok) {
        const result = await response.json();
        libraryOutput = result.output || '';
      } else {
        // 백엔드가 없는 경우 시뮬레이션으로 폴백
        libraryOutput = await simulateCommand('WRKLIB');
      }
      
      const libraries = parseLibrariesFromOutput(libraryOutput, volume);
      setAvailableLibraries(libraries);
      
      // 첫 번째 라이브러리를 기본값으로 설정
      if (libraries.length > 0) {
        setSystemInfo(prev => ({
          ...prev,
          currentLibrary: libraries[0]
        }));
      }
    } catch (error) {
      console.error('Error loading libraries:', error);
      // 에러 시 시뮬레이션으로 폴백
      try {
        const libraryOutput = await simulateCommand('WRKLIB');
        const libraries = parseLibrariesFromOutput(libraryOutput, volume);
        setAvailableLibraries(libraries);
        if (libraries.length > 0) {
          setSystemInfo(prev => ({
            ...prev,
            currentLibrary: libraries[0]
          }));
        }
      } catch (fallbackError) {
        console.error('Fallback error:', fallbackError);
      }
    }
  };

  // WRKVOL 출력에서 볼륨 목록 파싱
  const parseVolumesFromOutput = (output: string): string[] => {
    const lines = output.split('\n');
    const volumes: string[] = [];
    
    lines.forEach(line => {
      const volumeMatch = line.match(/Volume Name\s*:\s*(\w+)/);
      if (volumeMatch) {
        volumes.push(volumeMatch[1]);
      }
    });
    
    return volumes;
  };

  // WRKLIB 출력에서 라이브러리 목록 파싱
  const parseLibrariesFromOutput = (output: string, volume: string): string[] => {
    const lines = output.split('\n');
    const libraries: string[] = [];
    let isVolumeSection = false;
    
    lines.forEach(line => {
      if (line.includes(`Volume: ${volume}`)) {
        isVolumeSection = true;
      } else if (line.includes('Volume:') && !line.includes(volume)) {
        isVolumeSection = false;
      } else if (isVolumeSection && line.includes('- Library:')) {
        const libraryMatch = line.match(/- Library:\s*(\w+)/);
        if (libraryMatch) {
          libraries.push(libraryMatch[1]);
        }
      }
    });
    
    return libraries;
  };

  // Props 변경 시 시스템 정보 업데이트
  useEffect(() => {
    if (user) {
      setSystemInfo(prev => ({
        ...prev,
        currentUser: user
      }));
    }
  }, [user]);

  // 로그인 유저 정보 가져오기 및 초기 데이터 로드
  useEffect(() => {
    const getUserInfo = () => {
      // props로 user가 전달되면 그것을 우선 사용
      if (user) {
        setSystemInfo(prev => ({
          ...prev,
          currentUser: user
        }));
        return;
      }

      // 그렇지 않으면 기존 로직 사용
      const userInfo = localStorage.getItem('openaspUser');
      if (userInfo) {
        try {
          const parsedUser = JSON.parse(userInfo);
          if (parsedUser.app === 'ofasp-ax' && parsedUser.username) {
            setSystemInfo(prev => ({
              ...prev,
              currentUser: parsedUser.username
            }));
          }
        } catch (error) {
          console.error('Error parsing user info:', error);
        }
      }
    };

    getUserInfo();
    loadAvailableVolumes();
  }, [user]);

  // Simplified WebSocket Hub 연결 초기화
  useEffect(() => {
    const initializeHub = async () => {
      try {
        console.log('[WebSocket Hub] Initializing Hub connection...');
        
        // Hub 서버에 연결
        webSocketService.connect('http://localhost:8000');
        
        // Hub 연결 성공 시
        const handleHubConnected = () => {
          console.log('[WebSocket Hub] Connected to Hub, registering...');
          setHubConnectionStatus('connected');
          
          const terminalId = 'webui';
          const username = systemInfo.currentUser || user || 'admin';
          const wsname = workstationName || 'WSNAME00';
          
          webSocketService.registerWithHub(terminalId, username, wsname);
          
          // Hub 연결 성공 메시지
          const newEntry: CommandHistory = {
            command: 'WebSocket Hub Connection',
            output: `Connected to WebSocket Hub v2.0 - Terminal: ${terminalId}, User: ${username}, Workstation: ${wsname}`,
            timestamp: new Date(),
            success: true
          };
          setCommandHistory(prev => [...prev, newEntry]);
        };

        // Hub 연결 해제 시
        const handleHubDisconnected = () => {
          console.log('[WebSocket Hub] Disconnected from Hub');
          setHubConnectionStatus('disconnected');
        };

        // 이벤트 리스너 등록 - 단순화
        webSocketService.on('hub_connected', handleHubConnected);
        webSocketService.on('hub_disconnected', handleHubDisconnected);
        
        return () => {
          webSocketService.off('hub_connected', handleHubConnected);
          webSocketService.off('hub_disconnected', handleHubDisconnected);
        };
      } catch (error) {
        console.error('[WebSocket Hub] Initialization failed:', error);
        setHubConnectionStatus('error');
      }
    };

    initializeHub();
  }, [systemInfo.currentUser, user, workstationName]);

  // Convert simplified employee data to SMED format
  const convertEmployeeDataToSmedFormat = (data: any) => {
    try {
      console.log('[SMED Conversion] Input data:', data);
      
      if (!data || !data.data || !Array.isArray(data.data)) {
        console.error('[SMED Conversion] Invalid employee data:', data);
        return [];
      }
      
      const fields: any[] = [];
      const employees = data.data.slice(0, 5); // Limit to 5 employees
      
      // Add title
      fields.push({
        name: 'TITLE', row: 1, col: 25, length: 20,
        prompt: data.title || 'Employee Information', type: 'output'
      });
      
      // Add separator
      fields.push({
        name: 'SEPARATOR', row: 2, col: 5, length: 40,
        prompt: data.subtitle || '------------------', type: 'output'
      });
      
      // Add headers
      fields.push({
        name: 'HEADER', row: 4, col: 5, length: 50,
        prompt: '  ID      Name             Dept', type: 'output'
      });
      
      // Add employee data (simple table format)
      employees.forEach((emp: any, index: number) => {
        const row = 6 + index;
        const displayText = `${emp.id}  ${emp.name.padEnd(15)}  ${emp.dept}`;
        
        fields.push({
          name: `EMP${index + 1}_DATA`,
          row: row,
          col: 5,
          length: Math.min(displayText.length, 50),
          prompt: displayText,
          type: 'output'
        });
      });
      
      // Add status
      fields.push({
        name: 'STATUS', row: 22, col: 5, length: 30,
        prompt: data.status || 'Press F3 to exit', type: 'output'
      });
      
      console.log(`[SMED Conversion] Generated ${fields.length} fields for ${employees.length} employees`);
      return fields;
      
    } catch (error) {
      console.error('[SMED Conversion] Error converting employee data:', error);
      return [];
    }
  };

  // Simplified WebSocket Hub 이벤트 핸들러 - 단일 이벤트만 처리
  useEffect(() => {
    console.log('[WebSocket Hub] Setting up Hub event handlers...');
    
    // 단일 이벤트 핸들러: smed_data_received (Hub에서 전송)
    const handleSmedDataReceived = (data: any) => {
      console.log('[WebSocket Hub] SMED data received from Hub:', {
        map_file: data.map_file,
        fields_count: Array.isArray(data.fields) ? data.fields.length : Object.keys(data.fields || {}).length,
        hub_metadata: data.hub_metadata
      });
      
      try {
        // Hub에서 받은 데이터 구조 로깅
        console.log('[WebSocket Hub] Received data structure:', {
          type: data.type,
          has_data: !!data.data,
          has_fields: !!data.fields,
          fields_count: data.fields ? (Array.isArray(data.fields) ? data.fields.length : Object.keys(data.fields).length) : 0,
          data_preview: data.data ? data.data.slice(0, 2) : 'No data field',
          full_data: data // 전체 데이터 구조 확인
        });
        
        // 즉시 렌더링 - Hub가 이미 중복 방지 처리함
        let processedFields = data.fields;
        
        // 새로운 employee data 형식 처리 - fields 내부의 데이터 확인
        if (data.fields && data.fields.type === 'smed_map' && data.fields.data) {
          console.log('[WebSocket Hub] Converting employee data to SMED format');
          processedFields = convertEmployeeDataToSmedFormat(data.fields);
          
          if (processedFields.length === 0) {
            console.error('[WebSocket Hub] Failed to convert employee data');
            return;
          }
        } else if (data.type === 'smed_map' && data.data) {
          // 직접 전달된 경우
          console.log('[WebSocket Hub] Converting direct employee data to SMED format');
          processedFields = convertEmployeeDataToSmedFormat(data);
          
          if (processedFields.length === 0) {
            console.error('[WebSocket Hub] Failed to convert employee data');
            return;
          }
        } else if (data.fields && typeof data.fields === 'object' && !Array.isArray(data.fields)) {
          console.log('[WebSocket Hub] Converting legacy fields object to SMED format');
          // Legacy support - 이전 버전과의 호환성
          processedFields = [];
        }
        
        // 즉시 SMED 맵 표시 - 상태 관리 단순화
        const smedData = {
          map_name: data.map_file || 'BROWSE_MENU',
          fields: processedFields,
          program_name: data.program_name || 'MSGSAMPLEBROWSERMENU',
          session_id: data.session_id || `hub_session_${Date.now()}`,
          hub_metadata: data.hub_metadata
        };
        
        console.log('[DEBUG] AspCliWebTerminal: Setting SMED map data:', JSON.stringify(smedData, null, 2));
        console.log('[DEBUG] AspCliWebTerminal: Processed fields count:', processedFields.length);
        console.log('[DEBUG] AspCliWebTerminal: Fields sample:', processedFields.length > 0 ? processedFields.slice(0, 3) : 'No fields');
        
        setSmedMapData(smedData);
        setShowSmedMap(true);
        
        console.log('[DEBUG] AspCliWebTerminal: setSmedMapData and setShowSmedMap called');
        console.log('[DEBUG] AspCliWebTerminal: showSmedMap will be set to true');
        
        // 터미널에 성공 메시지 추가
        const newEntry: CommandHistory = {
          command: 'WebSocket Hub SMED',
          output: `SMED Map received via Hub: ${data.map_file || 'BROWSE_MENU'} (${processedFields.length} fields) - Hub v${data.hub_metadata?.source || '2.0'}`,
          timestamp: new Date(),
          success: true
        };
        setCommandHistory(prev => [...prev, newEntry]);
        
      } catch (error) {
        console.error('[WebSocket Hub] Error processing SMED data:', error);
        
        // 에러 메시지 추가
        const errorEntry: CommandHistory = {
          command: 'WebSocket Hub Error',
          output: `Hub SMED processing error: ${error instanceof Error ? error.message : 'Unknown error'}`,
          timestamp: new Date(),
          success: false
        };
        setCommandHistory(prev => [...prev, errorEntry]);
      }
    };

    // Hub 상태 변경 핸들러
    const handleHubStatus = (data: any) => {
      console.log('[WebSocket Hub] Hub status update:', data);
      // Hub 상태 정보를 터미널에 표시할 수 있음
    };

    // Command confirmation 핸들러
    const handleCommandConfirmation = (data: any) => {
      console.log('[WebSocket Hub] Command confirmation received:', data);
      
      const confirmationEntry: CommandHistory = {
        command: `Command Confirmation: ${data.command}`,
        output: `${data.message} (Session: ${data.session_id})`,
        timestamp: new Date(),
        success: data.success
      };
      setCommandHistory(prev => [...prev, confirmationEntry]);
    };

    // 단순화된 이벤트 리스너 등록
    webSocketService.on('smed_data_received', handleSmedDataReceived);
    webSocketService.on('hub_status', handleHubStatus);
    webSocketService.on('command_confirmation', handleCommandConfirmation);

    return () => {
      // 정리
      console.log('[WebSocket Hub] Cleaning up Hub event listeners');
      webSocketService.off('smed_data_received', handleSmedDataReceived);
      webSocketService.off('hub_status', handleHubStatus);
      webSocketService.off('command_confirmation', handleCommandConfirmation);
    };
  }, []); // Hub 이벤트는 한 번만 등록

  // WebSocket Hub 정리 (컴포넌트 언마운트 시)
  useEffect(() => {
    return () => {
      console.log('[WebSocket Hub] Component unmounting, disconnecting from Hub');
      webSocketService.disconnect();
    };
  }, []);

  // 시스템 시간 업데이트
  useEffect(() => {
    const timer = setInterval(() => {
      setSystemInfo(prev => ({
        ...prev,
        systemTime: new Date().toLocaleString()
      }));
    }, 1000);

    return () => clearInterval(timer);
  }, []);

  // 드롭다운 외부 클릭 시 닫기
  useEffect(() => {
    const handleClickOutside = (event: MouseEvent) => {
      const target = event.target as HTMLElement;
      if (!target.closest('.dropdown-container')) {
        setShowVolumeDropdown(false);
        setShowLibraryDropdown(false);
      }
    };

    document.addEventListener('mousedown', handleClickOutside);
    return () => {
      document.removeEventListener('mousedown', handleClickOutside);
    };
  }, []);

  // 터미널 스크롤 자동 조정
  useEffect(() => {
    if (terminalRef.current) {
      terminalRef.current.scrollTop = terminalRef.current.scrollHeight;
    }
  }, [commandHistory]);

  // OpenASP Manager에 로그 전송
  const sendLogToOpenASPManager = async (entry: CommandHistory) => {
    try {
      const logData = {
        timestamp: entry.timestamp.toISOString(),
        level: entry.success ? 'INFO' : 'ERROR',
        service: 'ASP System Command',
        user: systemInfo.currentUser,
        message: `[${entry.command}] ${entry.output}`,
        details: {
          command: entry.command,
          output: entry.output,
          success: entry.success,
          volume: systemInfo.currentVolume,
          library: systemInfo.currentLibrary
        }
      };

      await fetch('http://localhost:8000/api/logs', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(logData),
      });
    } catch (error) {
      // OpenASP Manager가 실행되지 않은 경우 무시
      console.debug('OpenASP Manager log sending failed:', error);
    }
  };

  // ASP 명령어 실행
  const executeCommand = useCallback(async (command: string) => {
    if (!command.trim()) return;

    setIsExecuting(true);
    const timestamp = new Date();
    let commandOutput = '';
    
    try {
      // Python aspcli.py 호출
      const response = await fetch('http://localhost:8000/api/asp-command', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({ 
          command: command.trim(),
          user: systemInfo.currentUser 
        }),
      });

      let output = '';
      let success = false;

      if (response.ok) {
        const result = await response.json();
        output = result.output || result.error || '명령이 실행되었습니다.';
        success = result.success !== false;
        
        // Check if this is an EDTFILE command and try to parse the data
        if (command.trim().toUpperCase().startsWith('EDTFILE') && success) {
          const parsedData = parseEdtfileOutput(output, command.trim());
          if (parsedData) {
            setEdtfileData(parsedData);
            setShowEdtfileBrowser(true);
            // Don't add this to command history since we're opening browser
            setIsExecuting(false);
            setCurrentCommand('');
            setHistoryIndex(-1);
            return;
          }
        }
        
        // Check if this output contains SMED_MAP_OUTPUT (direct JSON parsing)
        if (output.includes('SMED_MAP_OUTPUT:')) {
          console.log('[WEB_TERMINAL_DEBUG] Found SMED_MAP_OUTPUT in command output');
          try {
            const smedOutputIndex = output.indexOf('SMED_MAP_OUTPUT:');
            const smedOutputPart = output.substring(smedOutputIndex + 'SMED_MAP_OUTPUT:'.length);
            
            // Find the JSON block
            const jsonStart = smedOutputPart.indexOf('{');
            if (jsonStart !== -1) {
              let jsonContent = '';
              let braceCount = 0;
              let i = jsonStart;
              
              while (i < smedOutputPart.length) {
                const char = smedOutputPart[i];
                jsonContent += char;
                
                if (char === '{') braceCount++;
                if (char === '}') braceCount--;
                
                if (braceCount === 0 && jsonContent.length > 1) break;
                i++;
              }
              
              console.log('[WEB_TERMINAL_DEBUG] Extracted SMED JSON:', jsonContent);
              
              const smedData = JSON.parse(jsonContent);
              console.log('[WEB_TERMINAL_DEBUG] Parsed SMED data:', smedData);
              
              // Convert SUB001 field format to SmedMapDisplay format
              const convertedFields = (smedData.fields || []).map((field: any, index: number) => ({
                name: `field_${index}`,
                row: field.row || 1,
                col: field.col || 1,
                length: field.text ? field.text.length : 10,
                value: field.text || '',
                prompt: field.type === 'static' ? field.text : undefined,
                type: field.type || 'static'
              }));
              
              console.log('[WEB_TERMINAL_DEBUG] Converted fields:', convertedFields);
              
              // Set SMED map display with correct data structure
              setSmedMapData({
                map_file: smedData.map_name,
                fields: convertedFields,
                program_name: smedData.map_name,
                title: smedData.title,
                rows: smedData.rows,
                cols: smedData.cols,
                hub_metadata: {
                  source: 'direct_output_parsing',
                  timestamp: new Date().toISOString()
                }
              });
              setShowSmedMap(true);
              
              console.log('[WEB_TERMINAL_DEBUG] SMED map display activated');
            }
          } catch (error) {
            console.error('[WEB_TERMINAL_ERROR] Failed to parse SMED_MAP_OUTPUT:', error);
          }
        }
        // Check if this output contains SMED map display request (legacy)
        else if (output.includes('[INFO] SMED map display requested:')) {
          const mapMatch = output.match(/SMED map display requested: ([^\n]+)/);
          if (mapMatch) {
            const mapFile = mapMatch[1];
            try {
              // Call API to parse SMED map
              const parseResponse = await fetch('http://localhost:8000/api/smed/parse', {
                method: 'POST',
                headers: {
                  'Content-Type': 'application/json',
                },
                body: JSON.stringify({ map_file: mapFile }),
              });
              
              if (parseResponse.ok) {
                const mapData = await parseResponse.json();
                
                // Extract program name from CALL command
                let programName = 'unknown';
                const callMatch = command.trim().toUpperCase().match(/CALL\s+PGM-([^,\s]+)/);
                if (callMatch) {
                  programName = callMatch[1];
                }
                
                // Add program info to map data
                setSmedMapData({
                  ...mapData,
                  program_name: programName,
                  session_id: `session_${Date.now()}`
                });
                setShowSmedMap(true);
                
                // WebSocket을 통해 SMED 데이터 전송 (다른 클라이언트들과 공유)
                if (webSocketService.isConnected() && workstationName) {
                  try {
                    const sent = webSocketService.sendSmedData(workstationName, {
                      map_file: mapFile,
                      fields: mapData.fields,
                      program_name: programName,
                      session_id: `session_${Date.now()}`,
                      action: 'display',
                      timestamp: new Date().toISOString()
                    });
                    if (!sent) {
                      console.warn('Failed to send SMED data via WebSocket - not connected');
                    }
                  } catch (wsError) {
                    console.error('Failed to send SMED data via WebSocket:', wsError);
                  }
                }
                
                // Don't show the output text, just show the map
                setIsExecuting(false);
                setCurrentCommand('');
                setHistoryIndex(-1);
                return;
              }
            } catch (error) {
              console.error('Failed to parse SMED map:', error);
            }
          }
        }

        // Check for MSGSAMPLEBROWSERMENU command - send via Hub (includes both old and new JSON versions)
        if (command.trim().toUpperCase().includes('MSGSAMPLEBROWSERMENU')) {
          if (webSocketService.isConnected()) {
            try {
              console.log('[DEBUG] AspCliWebTerminal: Sending MSGSAMPLEBROWSERMENU command via Hub');
              console.log('[DEBUG] AspCliWebTerminal: Full command:', command);
              console.log('[DEBUG] AspCliWebTerminal: WebSocket connected:', webSocketService.isConnected());
              
              const sent = webSocketService.sendMSGSampleBrowserCommand();
              console.log('[DEBUG] AspCliWebTerminal: Command sent result:', sent);
              
              if (sent) {
                // Add success message to terminal
                const hubEntry: CommandHistory = {
                  command: 'MSGSAMPLEBROWSERMENU (Hub)',
                  output: `[DEBUG] Command sent via WebSocket Hub v2.0. Hub will process and send SMED data directly...`,
                  timestamp: new Date(),
                  success: true
                };
                setCommandHistory(prev => [...prev, hubEntry]);
                console.log('[DEBUG] AspCliWebTerminal: Success message added to terminal history');
                
                // Hub가 직접 처리하므로 여기서 종료
                setIsExecuting(false);
                setCurrentCommand('');
                setHistoryIndex(-1);
                return;
              }
            } catch (hubError) {
              console.error('[WebSocket Hub] Failed to send MSGSAMPLEBROWSERMENU via Hub:', hubError);
              // HTTP API로 폴백
            }
          } else {
            console.warn('[WebSocket Hub] Not connected to Hub, falling back to HTTP API');
          }
        }
      } else {
        // 백엔드가 없는 경우 시뮬레이션
        output = await simulateCommand(command.trim());
        success = true;
      }

      const newEntry: CommandHistory = {
        command: command.trim(),
        output,
        timestamp,
        success
      };

      commandOutput = output;
      setCommandHistory(prev => [...prev, newEntry]);
      
      // OpenASP Manager(localhost:3007)에 로그 전송
      await sendLogToOpenASPManager(newEntry);
      
    } catch (error) {
      // 에러 발생 시 시뮬레이션으로 폴백
      const output = await simulateCommand(command.trim());
      const newEntry: CommandHistory = {
        command: command.trim(),
        output,
        timestamp,
        success: true
      };
      commandOutput = output;
      setCommandHistory(prev => [...prev, newEntry]);
      
      // OpenASP Manager에 로그 전송 (시뮬레이션 결과도)
      await sendLogToOpenASPManager(newEntry);
    }

    setIsExecuting(false);
    setCurrentCommand('');
    setHistoryIndex(-1);
    
    // 커서 위치 설정
    setTimeout(() => {
      focusCursor(commandOutput);
    }, 100);
  }, [systemInfo.currentUser]);

  // Parse EDTFILE output to extract structured data
  const parseEdtfileOutput = (output: string, command: string) => {
    try {
      // Extract filename from command
      const fileMatch = command.match(/FILE\(([^/]+)\/([^)]+)\)/i);
      if (!fileMatch) return null;
      
      const [, library, filename] = fileMatch;
      
      // Look for the structured data section
      const lines = output.split('\n');
      const records: Array<{number: number, data: string, rawBytes: number[]}> = [];
      
      let rectype = 'FB';
      let reclen = 80;
      let encoding = 'shift_jis';
      
      // Extract metadata
      const typeMatch = output.match(/Type:\s*(\w+)/);
      const lenMatch = output.match(/RecLen:\s*(\d+)/);
      const encMatch = output.match(/Encoding:\s*(\w+)/);
      
      if (typeMatch) rectype = typeMatch[1];
      if (lenMatch) reclen = parseInt(lenMatch[1]);
      if (encMatch) encoding = encMatch[1];
      
      // Parse record data - look for "Record X:" pattern
      for (let i = 0; i < lines.length; i++) {
        const line = lines[i]; // Don't trim - preserve all data including nulls and binary
        
        // Look for record line: "Record     1: data..." - capture everything as-is
        const recordMatch = line.match(/^Record\s+(\d+):\s*(.*)$/);
        
        if (recordMatch) {
          const recordNum = parseInt(recordMatch[1]);
          const recordData = recordMatch[2]; // Use exactly as server sent - no parsing or trimming
          
          console.log(`[DEBUG] React: Found record ${recordNum}`);
          console.log(`[DEBUG] React: Record data: ${recordData}`);
          console.log(`[DEBUG] React: Record data length: ${recordData.length}`);
          
          // Check for Unicode characters in recordData
          for (let k = 0; k < Math.min(recordData.length, 20); k++) {
            const char = recordData[k];
            const code = char.charCodeAt(0);
            if (code > 127) {
              console.log(`[DEBUG] React: Unicode char at pos ${k}: U+${code.toString(16).toUpperCase().padStart(4, '0')} = '${char}'`);
            }
          }
          
          // Look for HEX lines that follow this record
          const hexBytes: number[] = [];
          let foundHexSection = false;
          
          for (let j = i + 1; j < lines.length && j < i + 15; j++) {
            const hexLine = lines[j].trim();
            
            // Check if this line contains HEX data
            if (hexLine.includes('HEX:')) {
              foundHexSection = true;
              // Extract hex values after "HEX:"
              const hexPart = hexLine.substring(hexLine.indexOf('HEX:') + 4).trim();
              const hexMatches = hexPart.match(/[0-9A-F]{2}/g);
              if (hexMatches) {
                hexBytes.push(...hexMatches.map(h => parseInt(h, 16)));
              }
            } else if (foundHexSection && hexLine.match(/^\s*[0-9A-F]{2}(\s+[0-9A-F]{2})*\s*$/)) {
              // Line with only hex values (continuation of HEX data)
              const hexMatches = hexLine.match(/[0-9A-F]{2}/g);
              if (hexMatches) {
                hexBytes.push(...hexMatches.map(h => parseInt(h, 16)));
              }
            } else if (hexLine.includes('CHR:')) {
              // Skip CHR lines, but continue in HEX section
              continue;
            } else if (hexLine.includes('Record') || hexLine.includes('Total Records') || hexLine.includes('---')) {
              // Hit next record or end, stop looking for hex
              break;
            } else if (foundHexSection && hexLine.includes('...')) {
              // Found "... (80 total bytes)" line, we have incomplete hex data
              break;
            }
          }
          
          // Use hex bytes from server as-is, don't try to regenerate or pad
          
          records.push({
            number: recordNum,
            data: recordData,
            rawBytes: hexBytes
          });
        }
      }
      
      if (records.length === 0) return null;
      
      return {
        filename: `${library}/${filename}`,
        records,
        rectype,
        reclen,
        encoding
      };
    } catch (error) {
      console.error('Failed to parse EDTFILE output:', error);
      return null;
    }
  };

  // 명령어 시뮬레이션 (백엔드 없을 때)
  const simulateCommand = async (command: string): Promise<string> => {
    const upperCommand = command.toUpperCase();
    
    // 명령어 파싱
    const parts = upperCommand.split(' ');
    const mainCommand = parts[0];
    
    await new Promise(resolve => setTimeout(resolve, 500)); // 시뮬레이션 지연

    switch (mainCommand) {
      case 'WRKVOL':
        return `[INFO] ボリューム状況:
  📦 ボリューム名      : DISK99
     ├ ライブラリ数   : 3
     ├ 総ファイル数   : 12
     └ ディスク使用量 : 1,024 Byte
  📦 ボリューム名      : MSGQ
     ├ ライブラリ数   : 1
     ├ 総ファイル数   : 0
     └ ディスク使用量 : 0 Byte`;

      case 'CRTLIB':
        const libMatch = command.match(/LIB-(\w+)/i);
        const volMatch = command.match(/VOL-(\w+)/i);
        const libName = libMatch ? libMatch[1] : 'NEWLIB';
        const volName = volMatch ? volMatch[1] : 'DISK99';
        return `[INFO] ライブラリ '${libName}' がボリューム '${volName}' に作成されました。`;

      case 'CRTFILE':
        const fileMatch = command.match(/FILE\((\w+)\/(\w+)\)/i);
        if (fileMatch) {
          const [, lib, file] = fileMatch;
          return `[INFO] ファイル '${file}' がライブラリ '${lib}' に作成されました。`;
        }
        return `[INFO] ファイルが作成されました。`;

      case 'DSPFD':
        const dspfdMatch = command.match(/FILE\((\w+)\/(\w+)\)/i);
        if (dspfdMatch) {
          const [, lib, file] = dspfdMatch;
          return `[INFO] ファイル定義情報:
  📁 ファイルパス     : /volume/DISK99/${lib}/${file}
  📄 ファイル名       : ${file}
  📦 ファイルサイズ   : 0 Byte
  🕒 作成日時         : ${new Date().toLocaleString()}
  📉 ファイルが空です : はい`;
        }
        return `[INFO] ファイル定義情報を表示します。`;

      case 'HELP':
        const parts = command.split(' ');
        if (parts.length > 1) {
          const helpCommand = parts[1].toUpperCase();
          return getCommandHelp(helpCommand);
        }
        return `使用可能なASPコマンド:

ライブラリ管理:
  CRTLIB LIB-<name>,VOL-<volume>  - ライブラリ作成
  DLTLIB LIB-<name>,VOL-<volume>  - ライブラリ削除
  WRKLIB                          - ライブラリ一覧

ファイル管理:
  CRTFILE FILE(<lib>/<file>),VOL-<volume>  - ファイル作成
  DLTFILE FILE(<lib>/<file>),VOL-<volume>  - ファイル削除
  DSPFD FILE(<lib>/<file>),VOL-<volume>    - ファイル定義表示

システム管理:
  WRKVOL     - ボリューム状況
  WRKOBJ LIB-<name>,VOL-<volume>  - オブジェクト作業
  WRKSPLF    - スプールファイル作業
  DSPJOB     - ジョブ表示

メッセージ管理:
  SNDMSG TO-<user>,MSG-<message>  - メッセージ送信
  RCVMSG USER-<user>              - メッセージ受信
  WRKMSG                          - メッセージキュー表示

プログラム実行:
  CALL PGM-<lib>/<prog>,VOL-<volume>  - プログラム実行

バックアップ:
  SAVLIB LIB-<name>,VOL-<volume>  - ライブラリ保存
  RSTLIB FILE-<backup_file>       - ライブラリ復元

詳細なヘルプ: HELP <コマンド名>
例: HELP CRTLIB, HELP WRKVOL`;

      case 'CLS':
      case 'CLEAR':
        setCommandHistory([]);
        return '';

      default:
        if (upperCommand.includes('LIB-') || upperCommand.includes('FILE(') || upperCommand.includes('VOL-')) {
          return `[INFO] コマンド '${mainCommand}' が実行されました。`;
        }
        return `[ERROR] 不明なコマンドです: ${mainCommand}
HELP を入力して使用可能なコマンドを確認してください。`;
    }
  };

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    if (currentCommand.trim() && !isExecuting) {
      executeCommand(currentCommand);
    }
  };

  // 各コマンドの詳細ヘルプ
  const getCommandHelp = (command: string): string => {
    switch (command) {
      case 'CRTLIB':
        return `CRTLIB - ライブラリ作成

概要:
  新しいライブラリを指定されたボリュームに作成します。

構文:
  CRTLIB LIB-<library_name>,VOL-<volume_name>

パラメータ:
  LIB-<library_name>  : 作成するライブラリ名
  VOL-<volume_name>    : 作成先ボリューム名

例:
  CRTLIB LIB-TESTLIB,VOL-DISK99
  CRTLIB LIB-SALES,VOL-DISK01

注意:
  - ライブラリ名は英数字のみ使用可能
  - 同名のライブラリが存在する場合は上書きされます`;

      case 'DLTLIB':
        return `DLTLIB - ライブラリ削除

概要:
  指定されたライブラリとその中身を完全に削除します。

構文:
  DLTLIB LIB-<library_name>,VOL-<volume_name>

パラメータ:
  LIB-<library_name>  : 削除するライブラリ名
  VOL-<volume_name>    : 対象ボリューム名

例:
  DLTLIB LIB-TESTLIB,VOL-DISK99
  DLTLIB LIB-OLDLIB,VOL-DISK01

警告:
  - この操作は元に戻せません
  - ライブラリ内の全ファイルも削除されます`;

      case 'WRKVOL':
        return `WRKVOL - ボリューム状況表示

概要:
  システム内の全ボリュームの使用状況を表示します。

構文:
  WRKVOL

表示情報:
  - ボリューム名
  - ライブラリ数
  - 総ファイル数
  - ディスク使用量

例:
  WRKVOL

出力例:
  📦 ボリューム名      : DISK99
     ├ ライブラリ数   : 3
     ├ 総ファイル数   : 12
     └ ディスク使用量 : 1,024 Byte`;

      case 'CRTFILE':
        return `CRTFILE - ファイル作成

概要:
  指定されたライブラリ内にファイルを作成します。

構文:
  CRTFILE FILE(<library>/<filename>),VOL-<volume>

パラメータ:
  FILE(<library>/<filename>) : ライブラリ/ファイル名
  VOL-<volume>               : 対象ボリューム名

例:
  CRTFILE FILE(TESTLIB/CUSTMAST),VOL-DISK99
  CRTFILE FILE(SALES/REPORT),VOL-DISK01

注意:
  - 指定されたライブラリが存在する必要があります
  - ファイルは空の状態で作成されます`;

      case 'DSPFD':
        return `DSPFD - ファイル定義表示

概要:
  指定されたファイルの詳細情報を表示します。

構文:
  DSPFD FILE(<library>/<filename>),VOL-<volume>

パラメータ:
  FILE(<library>/<filename>) : ライブラリ/ファイル名
  VOL-<volume>               : 対象ボリューム名

表示情報:
  - ファイルパス
  - ファイルサイズ
  - 作成日時
  - 最終更新日時
  - 空ファイル判定

例:
  DSPFD FILE(TESTLIB/CUSTMAST),VOL-DISK99`;

      case 'WRKOBJ':
        return `WRKOBJ - オブジェクト作業

概要:
  指定されたライブラリ内のオブジェクト一覧を表示します。

構文:
  WRKOBJ LIB-<library_name>,VOL-<volume_name>

パラメータ:
  LIB-<library_name>  : 対象ライブラリ名
  VOL-<volume_name>    : 対象ボリューム名

表示情報:
  - オブジェクト名
  - ファイルサイズ
  - 最終更新日時

例:
  WRKOBJ LIB-TESTLIB,VOL-DISK99`;

      case 'SNDMSG':
        return `SNDMSG - メッセージ送信

概要:
  指定されたユーザーにメッセージを送信します。

構文:
  SNDMSG TO-<username>,MSG-<message>

パラメータ:
  TO-<username>  : 送信先ユーザー名
  MSG-<message>  : 送信メッセージ内容

例:
  SNDMSG TO-ADMIN,MSG-システム開始しました
  SNDMSG TO-USER01,MSG-処理完了

注意:
  - メッセージは日時付きで保存されます`;

      case 'RCVMSG':
        return `RCVMSG - メッセージ受信

概要:
  指定されたユーザーの受信メッセージを表示します。

構文:
  RCVMSG USER-<username>

パラメータ:
  USER-<username>  : 対象ユーザー名

例:
  RCVMSG USER-ADMIN
  RCVMSG USER-USER01

注意:
  - 受信したメッセージがない場合は通知されます`;

      case 'DSPJOB':
        return `DSPJOB - ジョブ表示

概要:
  システム内のジョブ実行履歴を表示します。

構文:
  DSPJOB

表示情報:
  - ジョブID
  - プログラム名
  - 開始時刻
  - 終了時刻
  - 実行ステータス

例:
  DSPJOB

注意:
  - 最新10件のジョブ履歴が表示されます`;

      case 'CALL':
        return `CALL - プログラム実行

概要:
  指定されたプログラムを実行します。

構文:
  CALL PGM-<library>/<program>,VOL-<volume>

パラメータ:
  PGM-<library>/<program>  : ライブラリ/プログラム名
  VOL-<volume>             : 対象ボリューム名

対応形式:
  - Python (.py)
  - Shell Script (.sh)

例:
  CALL PGM-TESTLIB/HELLO,VOL-DISK99
  CALL PGM-BATCH/PROCESS,VOL-DISK01`;

      case 'SAVLIB':
        return `SAVLIB - ライブラリ保存

概要:
  指定されたライブラリをバックアップファイルに保存します。

構文:
  SAVLIB LIB-<library_name>,VOL-<volume_name>

パラメータ:
  LIB-<library_name>  : 保存するライブラリ名
  VOL-<volume_name>    : 対象ボリューム名

例:
  SAVLIB LIB-TESTLIB,VOL-DISK99

注意:
  - バックアップファイルは自動的に日時付きで命名されます
  - tar.gz形式で圧縮保存されます`;

      case 'RSTLIB':
        return `RSTLIB - ライブラリ復元

概要:
  バックアップファイルからライブラリを復元します。

構文:
  RSTLIB FILE-<backup_filename>

パラメータ:
  FILE-<backup_filename>  : 復元するバックアップファイル名

例:
  RSTLIB FILE-TESTLIB_DISK99_20250719120000.tar.gz

注意:
  - バックアップファイルが存在する必要があります
  - 既存の同名ライブラリは上書きされます`;

      case 'WRKLIB':
        return `WRKLIB - ライブラリ一覧

概要:
  システム内の全ライブラリを一覧表示します。

構文:
  WRKLIB

表示情報:
  - ボリューム名
  - ライブラリ名

例:
  WRKLIB`;

      case 'WRKSPLF':
        return `WRKSPLF - スプールファイル作業

概要:
  システム内のスプールファイルを一覧表示します。

構文:
  WRKSPLF

表示情報:
  - スプールファイル名
  - ファイルサイズ
  - 最終更新日時

例:
  WRKSPLF`;

      case 'WRKMSG':
        return `WRKMSG - メッセージキュー表示

概要:
  システムメッセージキューの内容を表示します。

構文:
  WRKMSG

例:
  WRKMSG

注意:
  - システム全体のメッセージログが表示されます`;

      default:
        return `[ERROR] 不明なコマンドです: ${command}
使用可能なコマンド一覧を見るには HELP と入力してください。`;
    }
  };

  // 커서 포커스 관리
  const focusCursor = (output: string) => {
    // SMED 맵 출력에서 입력 필드가 있는지 확인
    if (output.includes('입력필드') || output.includes('INPUT') || output.includes('_____')) {
      // SMED 맵 입력 필드가 있으면 해당 필드에 포커스
      const mapInputField = document.querySelector('.map-input-field') as HTMLInputElement;
      if (mapInputField) {
        mapInputField.focus();
        return;
      }
    }
    
    // 그 외의 경우는 명령어 입력창에 포커스
    if (inputRef.current) {
      inputRef.current.focus();
    }
  };

  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === 'Tab') {
      e.preventDefault();
      // 명령어 자동완성
      const suggestions = commandSuggestions.filter(cmd => 
        cmd.toLowerCase().startsWith(currentCommand.toLowerCase())
      );
      if (suggestions.length === 1) {
        setCurrentCommand(suggestions[0] + ' ');
      }
    } else if (e.key === 'ArrowUp') {
      e.preventDefault();
      // 이전 명령어 불러오기 (최대 10개)
      const maxHistory = Math.min(commandHistory.length, 10);
      const recentHistory = commandHistory.slice(-maxHistory);
      
      if (recentHistory.length > 0) {
        const newIndex = historyIndex === -1 ? recentHistory.length - 1 : Math.max(0, historyIndex - 1);
        setHistoryIndex(newIndex);
        setCurrentCommand(recentHistory[newIndex].command);
      }
    } else if (e.key === 'ArrowDown') {
      e.preventDefault();
      // 다음 명령어 불러오기
      const maxHistory = Math.min(commandHistory.length, 10);
      const recentHistory = commandHistory.slice(-maxHistory);
      
      if (historyIndex >= 0) {
        if (historyIndex < recentHistory.length - 1) {
          const newIndex = historyIndex + 1;
          setHistoryIndex(newIndex);
          setCurrentCommand(recentHistory[newIndex].command);
        } else {
          setHistoryIndex(-1);
          setCurrentCommand('');
        }
      }
    }
  };

  const clearHistory = () => {
    setCommandHistory([]);
    setHistoryIndex(-1);
  };

  return (
    <div className={`asp-cli-terminal ${isDarkMode ? 'dark' : 'light'}`}>
      {/* 헤더 */}
      <div className="terminal-header">
        <div className="header-title">
          <span className="title-icon">🖥️</span>
          <span className="title-text">ASP System Command Terminal</span>
          <span className={`hub-status hub-${hubConnectionStatus}`}>
            Hub: {hubConnectionStatus === 'connected' ? '🟢' : hubConnectionStatus === 'error' ? '🔴' : '🟡'} v2.0
          </span>
        </div>
        <div className="header-info">
          <span className="info-item">ユーザー: {systemInfo.currentUser}</span>
          <span className="info-separator">|</span>
          <div className="info-item dropdown-container">
            <span 
              className="dropdown-trigger"
              onClick={() => setShowVolumeDropdown(!showVolumeDropdown)}
              title="ボリューム選択"
            >
              ボリューム: {systemInfo.currentVolume || 'N/A'} ▼
            </span>
            {showVolumeDropdown && (
              <div className="dropdown-menu">
                {availableVolumes.map(volume => (
                  <div 
                    key={volume}
                    className={`dropdown-item ${volume === systemInfo.currentVolume ? 'selected' : ''}`}
                    onClick={() => {
                      setSystemInfo(prev => ({ ...prev, currentVolume: volume }));
                      setShowVolumeDropdown(false);
                      loadLibrariesForVolume(volume);
                    }}
                  >
                    {volume}
                  </div>
                ))}
              </div>
            )}
          </div>
          <span className="info-separator">|</span>
          <div className="info-item dropdown-container">
            <span 
              className="dropdown-trigger"
              onClick={() => setShowLibraryDropdown(!showLibraryDropdown)}
              title="ライブラリ選択"
            >
              ライブラリ: {systemInfo.currentLibrary || 'N/A'} ▼
            </span>
            {showLibraryDropdown && (
              <div className="dropdown-menu">
                {availableLibraries.map(library => (
                  <div 
                    key={library}
                    className={`dropdown-item ${library === systemInfo.currentLibrary ? 'selected' : ''}`}
                    onClick={() => {
                      setSystemInfo(prev => ({ ...prev, currentLibrary: library }));
                      setShowLibraryDropdown(false);
                    }}
                  >
                    {library}
                  </div>
                ))}
              </div>
            )}
          </div>
          <span className="info-separator">|</span>
          <span className="info-item">{systemInfo.systemTime}</span>
        </div>
        <button 
          className="clear-button"
          onClick={clearHistory}
          title="画面クリア"
        >
          🗑️
        </button>
      </div>

      {/* 터미널 본문 */}
      <div className="terminal-body" ref={terminalRef}>
        {/* 시작 메시지 */}
        {commandHistory.length === 0 && (
          <div className="welcome-message">
            <div className="welcome-logo">
              ╔══════════════════════════════════════════╗
              ║           ASP System Command             ║
              ║              Terminal v2.0               ║
              ╚══════════════════════════════════════════╝
            </div>
            <div className="welcome-text">
              ASP システムコマンドターミナルへようこそ。<br/>
              ヘルプを表示するには <strong>HELP</strong> を入力してください。
            </div>
          </div>
        )}

        {/* 명령어 히스토리 */}
        {commandHistory.map((entry, index) => (
          <div key={index} className="command-entry">
            <div className="command-line">
              <span className="prompt">ASP&gt;</span>
              <span className="command-text">{entry.command}</span>
              <span className="command-time">
                [{entry.timestamp.toLocaleTimeString()}]
              </span>
            </div>
            <div className={`command-output ${entry.success ? 'success' : 'error'}`}>
              <pre>{entry.output}</pre>
            </div>
          </div>
        ))}

        {/* 실행 중 표시 */}
        {isExecuting && (
          <div className="executing-indicator">
            <span className="prompt">ASP&gt;</span>
            <span className="command-text">{currentCommand}</span>
            <span className="loading-dots">実行中...</span>
          </div>
        )}
      </div>

      {/* 명령어 입력 */}
      <form className="terminal-input" onSubmit={handleSubmit}>
        <span className="input-prompt">ASP&gt;</span>
        <input
          ref={inputRef}
          type="text"
          value={currentCommand}
          onChange={(e) => setCurrentCommand(e.target.value)}
          onKeyDown={handleKeyDown}
          placeholder="ASP コマンドを入力してください... (例: WRKVOL, HELP)"
          disabled={isExecuting}
          className="command-input"
          autoFocus
        />
        <button 
          type="submit" 
          disabled={isExecuting || !currentCommand.trim()}
          className="execute-button"
        >
          {isExecuting ? '実行中...' : '実行'}
        </button>
      </form>

      {/* 도움말 패널 */}
      <div className="help-panel">
        <strong>ショートカット:</strong> Tab(自動完成), ↑↓(コマンド履歴), Ctrl+L(画面クリア) | <strong>履歴:</strong> 最大 10 コマンド保存
      </div>

      {/* EDTFILE Browser */}
      {showEdtfileBrowser && edtfileData && (
        <EdtfileBrowser
          isDarkMode={isDarkMode}
          fileData={edtfileData}
          onClose={() => {
            setShowEdtfileBrowser(false);
            setEdtfileData(null);
          }}
        />
      )}
      
      {/* SMED Map Display */}
      {showSmedMap && smedMapData && (
        <div className="smed-map-overlay">
          <div className="smed-map-container">
            <div className="smed-map-header">
              <h3>SMED Map: {smedMapData.map_name}</h3>
              <button 
                className="smed-close-button"
                onClick={() => {
                  setShowSmedMap(false);
                  setSmedMapData(null);
                }}
              >
                ✕
              </button>
            </div>
            <SmedMapDisplay
              fields={smedMapData.fields}
              mapName={smedMapData.map_name}
              onClose={() => {
                setShowSmedMap(false);
                setSmedMapData(null);
                // Focus back to terminal input
                if (inputRef.current) {
                  inputRef.current.focus();
                }
              }}
              onKeyEvent={async (key: string, fieldValues: Record<string, string>) => {
                // Send key event via Hub
                try {
                  if (webSocketService.isConnected()) {
                    console.log('[WebSocket Hub] Sending key event via Hub:', key);
                    const sent = webSocketService.sendKeyEventToHub(key, fieldValues);
                    
                    if (sent) {
                      // Log the Hub key event
                      const keyEventEntry: CommandHistory = {
                        command: `SMED Key Event (Hub): ${key}`,
                        output: `Key ${key} sent via WebSocket Hub v2.0`,
                        timestamp: new Date(),
                        success: true
                      };
                      setCommandHistory(prev => [...prev, keyEventEntry]);
                      
                      // Hub가 응답을 처리하므로 로컬에서는 F3 처리만
                      if (key === 'F3') {
                        return { action: 'close' };
                      }
                      
                      return { action: 'processing', hub: true };
                    }
                  }
                  
                  // Hub 연결 실패 시 HTTP API 폴백
                  console.log('[WebSocket Hub] Hub not available, trying HTTP API fallback');
                  const response = await fetch('http://localhost:8000/api/smed/key-event', {
                    method: 'POST',
                    headers: {
                      'Content-Type': 'application/json',
                    },
                    body: JSON.stringify({
                      program_name: smedMapData.program_name || 'unknown',
                      session_id: smedMapData.session_id || 'default',
                      key: key,
                      field_values: fieldValues
                    }),
                  });

                  if (response.ok) {
                    const result = await response.json();
                    
                    // Log the HTTP key event
                    const keyEventEntry: CommandHistory = {
                      command: `SMED Key Event (HTTP): ${key}`,
                      output: `Key ${key} sent via HTTP API (Hub fallback)`,
                      timestamp: new Date(),
                      success: true
                    };
                    setCommandHistory(prev => [...prev, keyEventEntry]);
                    
                    return result;
                  }
                } catch (error) {
                  console.error('[WebSocket Hub] Error sending key event:', error);
                  
                  // 테스트용 F3 처리
                  if (key === 'F3') {
                    return { action: 'close' };
                  }
                }
                
                return null;
              }}
              onSubmit={async (fieldValues) => {
                // DEBUG: Log all field values with detailed debugging
                console.log('[WEB_TERMINAL_DEBUG] === SMED Form Submission ===');
                console.log('[WEB_TERMINAL_DEBUG] Timestamp:', new Date().toISOString());
                console.log('[WEB_TERMINAL_DEBUG] SMED Map Data:', smedMapData);
                console.log('[WEB_TERMINAL_DEBUG] Field Values:', JSON.stringify(fieldValues, null, 2));
                console.log('[WEB_TERMINAL_DEBUG] Map Name:', smedMapData?.map_name);
                console.log('[WEB_TERMINAL_DEBUG] INPUT Field Present:', 'INPUT' in fieldValues);
                console.log('[WEB_TERMINAL_DEBUG] INPUT Field Value:', fieldValues.INPUT);
                console.log('[WEB_TERMINAL_DEBUG] INPUT Field Type:', typeof fieldValues.INPUT);
                
                // Handle MAIN001 menu selection with enhanced debugging
                // Check for both INPUT and SEL fields (MAIN001 uses SEL field)
                const inputValue = fieldValues.INPUT || fieldValues.SEL;
                if (smedMapData?.map_name === 'MAIN001' && inputValue) {
                  const selection = inputValue.toString().trim();
                  console.log('[WEB_TERMINAL_DEBUG] MAIN001 Menu Processing:');
                  console.log('[WEB_TERMINAL_DEBUG] - Raw INPUT:', fieldValues.INPUT);
                  console.log('[WEB_TERMINAL_DEBUG] - Raw SEL:', fieldValues.SEL);
                  console.log('[WEB_TERMINAL_DEBUG] - Used inputValue:', inputValue);
                  console.log('[WEB_TERMINAL_DEBUG] - Trimmed Selection:', selection);
                  console.log('[WEB_TERMINAL_DEBUG] - Selection Length:', selection.length);
                  console.log('[WEB_TERMINAL_DEBUG] - Selection Char Codes:', Array.from(selection).map(c => c.charCodeAt(0)));
                  
                  // Close SMED map first
                  console.log('[WEB_TERMINAL_DEBUG] Closing SMED map...');
                  setShowSmedMap(false);
                  setSmedMapData(null);
                  
                  // Execute corresponding CALL command based on selection
                  let callCommand = '';
                  console.log('[WEB_TERMINAL_DEBUG] Processing selection switch...');
                  switch (selection) {
                    case '1':
                      callCommand = 'CALL PGM-SUB001.JAVA,VOL-DISK01';
                      console.log('[WEB_TERMINAL_DEBUG] Selection 1: Direct SUB001 call');
                      break;
                    case '2':
                      callCommand = 'CALL PGM-MAIN001.JAVA,PARA-(2),VOL-DISK01';
                      console.log('[WEB_TERMINAL_DEBUG] Selection 2: CREATE1 call');
                      break;
                    case '3':
                      callCommand = 'CALL PGM-MAIN001.JAVA,PARA-(3),VOL-DISK01';
                      console.log('[WEB_TERMINAL_DEBUG] Selection 3: UPDATE1 call');
                      break;
                    case '4':
                      callCommand = 'CALL PGM-MAIN001.JAVA,PARA-(4),VOL-DISK01';
                      console.log('[WEB_TERMINAL_DEBUG] Selection 4: DELETE1 call');
                      break;
                    default:
                      console.log('[WEB_TERMINAL_DEBUG] Invalid selection detected');
                      console.log('[WEB_TERMINAL_DEBUG] Expected: "1", "2", "3", or "4"');
                      console.log('[WEB_TERMINAL_DEBUG] Received:', JSON.stringify(selection));
                      const errorEntry: CommandHistory = {
                        command: `Selection: ${selection}`,
                        output: `Error: Invalid selection. Please choose 1-4.`,
                        timestamp: new Date(),
                        success: false
                      };
                      setCommandHistory(prev => [...prev, errorEntry]);
                      return;
                  }
                  
                  // Execute the CALL command with detailed logging
                  console.log('[WEB_TERMINAL_DEBUG] === Executing CALL Command ===');
                  console.log('[WEB_TERMINAL_DEBUG] Command:', callCommand);
                  console.log('[WEB_TERMINAL_DEBUG] Setting current command...');
                  setCurrentCommand(callCommand);
                  
                  console.log('[WEB_TERMINAL_DEBUG] Calling executeCommand...');
                  try {
                    const result = await executeCommand(callCommand);
                    console.log('[WEB_TERMINAL_DEBUG] executeCommand result:', result);
                  } catch (error) {
                    console.error('[WEB_TERMINAL_ERROR] executeCommand failed:', error);
                  }
                  console.log('[WEB_TERMINAL_DEBUG] === CALL Command Execution Complete ===');
                  return;
                } else {
                  console.log('[WEB_TERMINAL_DEBUG] MAIN001 menu condition not met:');
                  console.log('[WEB_TERMINAL_DEBUG] - Map name match:', smedMapData?.map_name === 'MAIN001');
                  console.log('[WEB_TERMINAL_DEBUG] - INPUT field present:', !!fieldValues.INPUT);
                  console.log('[WEB_TERMINAL_DEBUG] - SEL field present:', !!fieldValues.SEL);
                  console.log('[WEB_TERMINAL_DEBUG] - Combined inputValue present:', !!(fieldValues.INPUT || fieldValues.SEL));
                }
                
                // For other SMED maps, show default success message
                setShowSmedMap(false);
                setSmedMapData(null);
                
                const newEntry: CommandHistory = {
                  command: 'SMED Submit',
                  output: `Fields submitted: ${JSON.stringify(fieldValues, null, 2)}`,
                  timestamp: new Date(),
                  success: true
                };
                setCommandHistory(prev => [...prev, newEntry]);
              }}
            />
          </div>
        </div>
      )}
    </div>
  );
};

export default AspCliWebTerminal;