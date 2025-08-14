import React, { useState, useEffect } from 'react';
import {
  CpuChipIcon,
  ServerIcon,
  CircleStackIcon,
  ClockIcon,
  ArrowPathIcon,
  ExclamationTriangleIcon,
  ChartBarIcon,
  UserGroupIcon,
  DocumentTextIcon,
  Cog6ToothIcon,
  PlayIcon,
  StopIcon,
  PauseIcon,
  QueueListIcon,
  CommandLineIcon,
  FolderIcon,
  ShieldCheckIcon,
  CloudIcon,
  TrashIcon,
  ChevronUpIcon,
  ChevronDownIcon
} from '@heroicons/react/24/outline';
import { APP_CONFIG } from '../config/app';

interface SystemData {
  system_info: {
    hostname: string;
    uptime: string;
    cpu_percent: number;
    cpu_count: number;
    memory_total: string;
    memory_used: string;
    memory_percent: number;
    disk_total: string;
    disk_used: string;
    disk_percent: number;
    process_count: number;
    load_avg: [number, number, number];
  };
  processes: Array<{
    pid: number;
    name: string;
    user: string;
    cpu_percent: number;
    memory_percent: number;
    status: string;
  }>;
  alerts: Array<{
    type: string;
    message: string;
    timestamp: string;
  }>;
  logs: string[];
  last_update: string;
}

interface CatalogResource {
  TYPE: string;
  PGMTYPE?: string;
  MAPTYPE?: string;
  RECTYPE?: string;
  RECLEN?: number;
  ENCODING?: string;
  DESCRIPTION: string;
  VERSION?: string;
  CREATED: string;
  UPDATED: string;
  [key: string]: any;
}

interface Job {
  id: string;
  name: string;
  status: string;
  user: string;
  start_time: string;
  cpu_time: string;
  priority: number;
  program?: string;
  library?: string;
  volume?: string;
}

interface DatasetInfo {
  name: string;
  type: string;
  library: string;
  volume: string;
  rectype?: string;
  reclen?: number;
  encoding?: string;
  description: string;
}

interface DatasetLock {
  dataset: string;
  level: string;
  pid: number;
  user: string;
  process?: string;
  lock_time?: string;
}

const SystemManagerPage: React.FC = () => {
  const [systemData, setSystemData] = useState<SystemData | null>(null);
  const [loading, setLoading] = useState(true);
  const [activeSection, setActiveSection] = useState('overview');
  // const [selectedJob, setSelectedJob] = useState<string | null>(null); // Removed unused variable
  const [catalogData, setCatalogData] = useState<any>(null);
  const [jobs, setJobs] = useState<Job[]>([]);
  const [selectedResource, setSelectedResource] = useState<{resource: CatalogResource, name: string, library: string, volume: string} | null>(null);
  const [showCLViewer, setShowCLViewer] = useState(false);
  const [clContent, setCLContent] = useState<string>('');
  const [selectedCL, setSelectedCL] = useState<{name: string, library: string, volume: string} | null>(null);
  const [isLoadingCL, setIsLoadingCL] = useState(false);
  const [selectedJobLog, setSelectedJobLog] = useState<{job: Job, log: string} | null>(null);
  const [isSubmittingJob, setIsSubmittingJob] = useState(false);
  const [selectedDataset, setSelectedDataset] = useState<(DatasetInfo & {data?: string}) | null>(null);
  const [isLoadingDataset, setIsLoadingDataset] = useState(false);
  const [editingDataset, setEditingDataset] = useState<(DatasetInfo & {data?: string}) | null>(null);
  const [editedData, setEditedData] = useState<string>('');
  const [datasetLocks, setDatasetLocks] = useState<DatasetLock[]>([]);
  const [selectedLocks, setSelectedLocks] = useState<Set<string>>(new Set());
  const [isLoadingLocks, setIsLoadingLocks] = useState(false);
  const [cursorPosition, setCursorPosition] = useState<{row: number, col: number}>({row: 0, col: 0});
  const [hoveredChar, setHoveredChar] = useState<{char: string, hex: string, pos: number} | null>(null);
  
  // Dataset viewer pagination
  const [currentPage, setCurrentPage] = useState(1);
  const [recordsPerPage] = useState(50); // 한 페이지당 레코드 수
  const [totalRecords, setTotalRecords] = useState(0);

  // Job sorting state
  const [sortField, setSortField] = useState<string>('start_time');
  const [sortDirection, setSortDirection] = useState<'asc' | 'desc'>('desc');

  // Catalog sorting state
  const [catalogSortField, setCatalogSortField] = useState<string>('name');
  const [catalogSortDirection, setCatalogSortDirection] = useState<'asc' | 'desc'>('asc');

  const sections = [
    { id: 'overview', label: 'System Overview', icon: <ServerIcon className="w-5 h-5" /> },
    { id: 'catalog', label: 'Catalog', icon: <DocumentTextIcon className="w-5 h-5" /> },
    { id: 'jobs', label: 'Job Management', icon: <PlayIcon className="w-5 h-5" /> },
    { id: 'queues', label: 'Queue Management', icon: <QueueListIcon className="w-5 h-5" /> },
    { id: 'datasets', label: 'Dataset Management', icon: <FolderIcon className="w-5 h-5" /> },
    { id: 'resources', label: 'Dataset Monitoring', icon: <CircleStackIcon className="w-5 h-5" /> },
    { id: 'users', label: 'User Management', icon: <UserGroupIcon className="w-5 h-5" /> },
    { id: 'network', label: 'Network Control', icon: <CloudIcon className="w-5 h-5" /> },
    { id: 'security', label: 'Security & Access', icon: <ShieldCheckIcon className="w-5 h-5" /> },
    { id: 'config', label: 'System Configuration', icon: <Cog6ToothIcon className="w-5 h-5" /> },
    { id: 'terminal', label: 'Command Terminal', icon: <CommandLineIcon className="w-5 h-5" /> }
  ];




  useEffect(() => {
    fetchSystemData();
    fetchCatalogData();
    fetchJobs();
    fetchDatasetLocks();
    const interval = setInterval(() => {
      fetchSystemData();
      fetchJobs();
      fetchDatasetLocks();
    }, 10000); // Refresh dataset locks every 10 seconds
    return () => clearInterval(interval);
  }, []);

  const fetchCatalogData = async () => {
    try {
      const response = await fetch('http://localhost:8000/api/catalog');
      if (response.ok) {
        const data = await response.json();
        setCatalogData(data);
      }
    } catch (error) {
      console.error('Failed to fetch catalog data:', error);
    }
  };

  const fetchJobs = async () => {
    try {
      const response = await fetch(`${APP_CONFIG.api.baseUrl}/api/jobs`);
      if (response.ok) {
        const data = await response.json();
        setJobs(data.jobs || []);
      }
    } catch (error) {
      console.error('Failed to fetch jobs:', error);
      // Fallback to empty array instead of mock data
      setJobs([]);
    }
  };

  const fetchDatasetLocks = async () => {
    setIsLoadingLocks(true);
    try {
      // Call dslock_suite API to get lock information from main API server (port 8000)
      const response = await fetch('http://localhost:8000/api/dslock/query', {
        method: 'GET',
        headers: {
          'Content-Type': 'application/json',
        }
      });
      
      if (response.ok) {
        const data = await response.json();
        // Parse JSON response from dslock_query_locks API
        const rawLocks = JSON.parse(data.locks || '[]');
        
        // Use lock_time field directly from API (already in HH:MM:SS format)
        const locks = rawLocks.map((lock: any) => ({
          dataset: lock.dataset,
          level: lock.level,
          pid: lock.pid,
          user: lock.user,
          process: lock.process,
          lock_time: lock.lock_time || '不明'
        }));
        
        setDatasetLocks(locks);
      } else {
        console.error('Failed to fetch dataset locks:', response.statusText);
        // Fallback to mock data for testing
        setDatasetLocks([
          {
            dataset: 'DISK01/TESTLIB/EMPLOYEE.FB',
            level: 'OLD',
            pid: 1234,
            user: 'aspuser',
            process: 'test_program',
            lock_time: '2025-08-13T10:30:00Z'
          },
          {
            dataset: 'DISK01/TESTLIB/CUSTOMER.FB', 
            level: 'SHR',
            pid: 5678,
            user: 'batchuser',
            process: 'batch_process',
            lock_time: '2025-08-13T11:15:00Z'
          }
        ]);
      }
    } catch (error) {
      console.error('Error fetching dataset locks:', error);
      setDatasetLocks([]);
    } finally {
      setIsLoadingLocks(false);
    }
  };

  const cleanupSelectedLocks = async () => {
    if (selectedLocks.size === 0) {
      alert('クリーンアップするロックを選択してください');
      return;
    }

    const confirmation = window.confirm(
      `選択した ${selectedLocks.size} 個のロックをクリーンアップしますか？`
    );
    
    if (!confirmation) return;

    try {
      for (const lockKey of Array.from(selectedLocks)) {
        const lock = datasetLocks.find(l => `${l.dataset}_${l.pid}` === lockKey);
        if (lock) {
          const response = await fetch('http://localhost:8000/api/dslock/cleanup', {
            method: 'DELETE',
            headers: {
              'Content-Type': 'application/json',
            },
            body: JSON.stringify({
              pid: lock.pid,
              dataset: lock.dataset
            })
          });
          
          if (!response.ok) {
            console.error(`Failed to cleanup lock for ${lock.dataset}:`, response.statusText);
          }
        }
      }
      
      // Clear selections and refresh lock data
      setSelectedLocks(new Set());
      await fetchDatasetLocks();
      alert('選択したロックをクリーンアップしました');
      
    } catch (error) {
      console.error('Error during lock cleanup:', error);
      alert('ロッククリーンアップ中にエラーが発生しました');
    }
  };

  const fetchSystemData = async () => {
    try {
      // Try to fetch from ASP Manager backend, fallback to mock data
      const response = await fetch(`${APP_CONFIG.api.baseUrl}/api/system`);
      if (response.ok) {
        const data = await response.json();
        setSystemData(data);
      } else {
        throw new Error('API not available');
      }
    } catch (error) {
      // Fallback to mock data
      setSystemData({
        system_info: {
          hostname: 'ASP-SYS-001',
          uptime: '5d 12h 30m',
          cpu_percent: 25.4,
          cpu_count: 8,
          memory_total: '16.0 GB',
          memory_used: '8.2 GB',
          memory_percent: 51.2,
          disk_total: '1.0 TB',
          disk_used: '156.7 GB',
          disk_percent: 15.3,
          process_count: 142,
          load_avg: [0.84, 1.12, 1.35]
        },
        processes: [
          { pid: 1234, name: 'asp-server', user: 'root', cpu_percent: 12.5, memory_percent: 8.3, status: 'running' },
          { pid: 5678, name: 'batch-proc', user: 'batch', cpu_percent: 8.2, memory_percent: 4.1, status: 'running' },
          { pid: 9012, name: 'data-loader', user: 'data', cpu_percent: 5.7, memory_percent: 12.4, status: 'sleeping' }
        ],
        alerts: [
          { type: 'info', message: 'System monitoring active', timestamp: new Date().toISOString() },
          { type: 'warning', message: 'Job queue approaching capacity', timestamp: new Date().toISOString() }
        ],
        logs: [
          `[${new Date().toLocaleTimeString()}] INFO: System manager dashboard loaded`,
          `[${new Date().toLocaleTimeString()}] INFO: Monitoring 142 processes`,
          `[${new Date().toLocaleTimeString()}] INFO: All queues operational`
        ],
        last_update: new Date().toISOString()
      });
    }
    setLoading(false);
  };

  const submitJob = async (program: string, library: string, volume: string) => {
    setIsSubmittingJob(true);
    try {
      const jobName = `JOB_${program}_${Date.now()}`;
      const response = await fetch(`${APP_CONFIG.api.baseUrl}/api/jobs/submit`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          job: jobName,
          program: program,
          library: library,
          volume: volume,
          parameters: '',
          priority: 5
        })
      });
      
      if (response.ok) {
        const result = await response.json();
        console.log('Job submitted:', result);
        // Refresh jobs list
        await fetchJobs();
        // Close modal
        setSelectedResource(null);
        // Switch to jobs tab
        setActiveSection('jobs');
      } else {
        console.error('Failed to submit job');
      }
    } catch (error) {
      console.error('Error submitting job:', error);
    } finally {
      setIsSubmittingJob(false);
    }
  };

  const handleJobAction = async (jobId: string, action: string) => {
    try {
      const response = await fetch(`${APP_CONFIG.api.baseUrl}/api/jobs/${jobId}/${action}`, {
        method: 'POST'
      });
      
      if (response.ok) {
        console.log(`Job ${jobId}: ${action} successful`);
        await fetchJobs(); // Refresh jobs list
      } else {
        console.error(`Failed to ${action} job ${jobId}`);
      }
    } catch (error) {
      console.error(`Error performing ${action} on job ${jobId}:`, error);
    }
  };

  const handleDeleteJob = async (jobId: string) => {
    if (!window.confirm('このジョブを削除しますか？データベースからも完全に削除されます。')) {
      return;
    }
    
    try {
      const response = await fetch(`${APP_CONFIG.api.baseUrl}/api/jobs/${jobId}`, {
        method: 'DELETE'
      });
      
      if (response.ok) {
        console.log(`Job ${jobId}: deleted successfully`);
        await fetchJobs(); // Refresh jobs list
        alert('ジョブが正常に削除されました。');
      } else {
        const errorData = await response.json();
        console.error(`Failed to delete job ${jobId}:`, errorData);
        alert(`ジョブの削除に失敗しました: ${errorData.error || errorData.message}`);
      }
    } catch (error) {
      console.error(`Error deleting job ${jobId}:`, error);
      alert('ジョブの削除中にエラーが発生しました。');
    }
  };

  const handleSort = (field: string) => {
    if (sortField === field) {
      // Same field clicked, toggle direction
      setSortDirection(sortDirection === 'asc' ? 'desc' : 'asc');
    } else {
      // New field clicked, default to ascending
      setSortField(field);
      setSortDirection('asc');
    }
  };

  const getSortedJobs = () => {
    const sorted = [...jobs].sort((a, b) => {
      let aValue: any = a[sortField as keyof Job] || '';
      let bValue: any = b[sortField as keyof Job] || '';

      // Handle special cases for sorting
      switch (sortField) {
        case 'priority':
          aValue = Number(aValue) || 0;
          bValue = Number(bValue) || 0;
          break;
        case 'start_time':
          // Convert time strings to comparable format
          aValue = aValue ? new Date(aValue).getTime() : 0;
          bValue = bValue ? new Date(bValue).getTime() : 0;
          break;
        case 'id':
        case 'name':
        case 'status':
        case 'user':
        default:
          aValue = String(aValue).toLowerCase();
          bValue = String(bValue).toLowerCase();
      }

      if (sortDirection === 'asc') {
        return aValue > bValue ? 1 : aValue < bValue ? -1 : 0;
      } else {
        return aValue < bValue ? 1 : aValue > bValue ? -1 : 0;
      }
    });

    return sorted;
  };

  const getSortIcon = (field: string) => {
    if (sortField !== field) {
      return null;
    }
    return sortDirection === 'asc' ? 
      <ChevronUpIcon className="w-4 h-4 inline ml-1" /> : 
      <ChevronDownIcon className="w-4 h-4 inline ml-1" />;
  };

  // Catalog sorting functions
  const handleCatalogSort = (field: string) => {
    if (catalogSortField === field) {
      // Same field clicked, toggle direction
      setCatalogSortDirection(catalogSortDirection === 'asc' ? 'desc' : 'asc');
    } else {
      // New field clicked, default to ascending
      setCatalogSortField(field);
      setCatalogSortDirection('asc');
    }
  };

  const getCatalogSortIcon = (field: string) => {
    if (catalogSortField !== field) {
      return null;
    }
    return catalogSortDirection === 'asc' ? 
      <ChevronUpIcon className="w-4 h-4 inline ml-1" /> : 
      <ChevronDownIcon className="w-4 h-4 inline ml-1" />;
  };

  const fetchJobLog = async (job: Job) => {
    try {
      const response = await fetch(`${APP_CONFIG.api.baseUrl}/api/jobs/${job.id}/log`);
      if (response.ok) {
        const logData = await response.text();
        setSelectedJobLog({ job, log: logData });
      } else {
        setSelectedJobLog({ job, log: 'Log not available or job log file not found.' });
      }
    } catch (error) {
      console.error('Error fetching job log:', error);
      setSelectedJobLog({ job, log: 'Error fetching job log.' });
    }
  };

  const handleCLDoubleClick = async (resource: {resource: CatalogResource, name: string, library: string, volume: string}) => {
    setIsLoadingCL(true);
    setSelectedCL({
      name: resource.name,
      library: resource.library,
      volume: resource.volume
    });
    
    try {
      // CL 파일명 - 확장자 없이 사용 (서버에서 자동으로 처리)
      const filename = resource.resource.SOURCEFILE || resource.name;
      const response = await fetch(`${APP_CONFIG.api.baseUrl}/api/cl/${resource.volume}/${resource.library}/${filename}`);
      
      if (response.ok) {
        const data = await response.json();
        if (data.success) {
          setCLContent(data.content);
          setShowCLViewer(true);
        } else {
          console.error('Failed to load CL file:', data.error);
          setCLContent('CLファイルの読み込みに失敗しました。');
          setShowCLViewer(true);
        }
      } else {
        console.error('Failed to fetch CL file');
        setCLContent('CLファイルが見つかりません。');
        setShowCLViewer(true);
      }
    } catch (error) {
      console.error('Error fetching CL file:', error);
      setCLContent('CLファイルの読み込み中にエラーが発生しました。');
      setShowCLViewer(true);
    } finally {
      setIsLoadingCL(false);
    }
  };

  const executeCL = async () => {
    if (!selectedCL) return;
    
    try {
      const jobName = `CL_${selectedCL.name}_${Date.now()}`;
      const response = await fetch(`${APP_CONFIG.api.baseUrl}/api/cl/execute`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          program: selectedCL.name,
          library: selectedCL.library,
          volume: selectedCL.volume,
          jobname: jobName,
          parameters: ''
        })
      });
      
      if (response.ok) {
        const result = await response.json();
        if (result.success) {
          alert(`CLプログラムが正常に投入されました。\\nジョブID: ${result.jobId || 'N/A'}`);
          // Close CL viewer
          setShowCLViewer(false);
          // Switch to jobs tab to see the submitted job
          setActiveSection('jobs');
        } else {
          alert(`CLプログラムの実行に失敗しました: ${result.error}`);
        }
      } else {
        alert('CLプログラムの実行に失敗しました。');
      }
    } catch (error) {
      console.error('Error executing CL:', error);
      alert('CLプログラムの実行中にエラーが発生しました。');
    }
  };

  const viewDataset = async (dataset: DatasetInfo) => {
    setIsLoadingDataset(true);
    setCurrentPage(1); // 새 데이터셋을 열 때 첫 페이지로 리셋
    setTotalRecords(0); // 레코드 수 리셋
    
    try {
      const response = await fetch(`${APP_CONFIG.api.baseUrl}/api/datasets/${dataset.volume}/${dataset.library}/${dataset.name}/data`);
      if (response.ok) {
        const data = await response.text();
        setSelectedDataset({ ...dataset, data });
      } else {
        setSelectedDataset({ ...dataset, data: 'データが見つかりません。' });
      }
    } catch (error) {
      console.error('Error fetching dataset data:', error);
      setSelectedDataset({ ...dataset, data: 'データの取得中にエラーが発生しました。' });
    } finally {
      setIsLoadingDataset(false);
    }
  };

  const editDataset = async (dataset: DatasetInfo) => {
    setIsLoadingDataset(true);
    try {
      const response = await fetch(`${APP_CONFIG.api.baseUrl}/api/datasets/${dataset.volume}/${dataset.library}/${dataset.name}/data`);
      if (response.ok) {
        const data = await response.text();
        setEditingDataset({ ...dataset, data });
        setEditedData(data);
      } else {
        alert('データセットの読み込みに失敗しました。');
      }
    } catch (error) {
      console.error('Error loading dataset for edit:', error);
      alert('データセットの読み込み中にエラーが発生しました。');
    } finally {
      setIsLoadingDataset(false);
    }
  };

  const saveDatasetChanges = async () => {
    if (!editingDataset) return;
    
    try {
      const response = await fetch(`${APP_CONFIG.api.baseUrl}/api/datasets/${editingDataset.volume}/${editingDataset.library}/${editingDataset.name}`, {
        method: 'PUT',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          data: editedData
        })
      });
      
      if (response.ok) {
        alert('データセットが正常に保存されました。');
        setEditingDataset(null);
        setEditedData('');
      } else {
        const errorData = await response.json();
        alert(`データセットの保存に失敗しました: ${errorData.message || errorData.error}`);
      }
    } catch (error) {
      console.error('Error saving dataset:', error);
      alert('データセットの保存中にエラーが発生しました。');
    }
  };

  const cancelDatasetEdit = () => {
    setEditingDataset(null);
    setEditedData('');
  };

  // 문자를 헥사값으로 변환하는 함수
  const charToHex = (char: string): string => {
    if (!char) return '';
    const code = char.charCodeAt(0);
    return code.toString(16).toUpperCase().padStart(4, '0');
  };

  // 데이터에서 특정 위치의 문자 정보를 가져오는 함수
  const getCharAtPosition = (data: string, position: number): {char: string, hex: string, pos: number} | null => {
    if (!data || position < 0 || position >= data.length) return null;
    const char = data.charAt(position);
    return {
      char: char === '\n' ? '\\n' : char === '\t' ? '\\t' : char || ' ',
      hex: charToHex(char),
      pos: position
    };
  };

  const deleteDataset = async (dataset: DatasetInfo) => {
    if (window.confirm(`データセット "${dataset.name}" を削除しますか？\n\nライブラリ: ${dataset.library}\nボリューム: ${dataset.volume}\n\nこの操作は取り消せません。`)) {
      try {
        const response = await fetch(`${APP_CONFIG.api.baseUrl}/api/datasets/${dataset.volume}/${dataset.library}/${dataset.name}`, {
          method: 'DELETE'
        });
        if (response.ok) {
          alert('データセットが削除されました。');
          // Refresh catalog data
          await fetchCatalogData();
        } else {
          alert('データセットの削除に失敗しました。');
        }
      } catch (error) {
        console.error('Error deleting dataset:', error);
        alert('データセットの削除中にエラーが発生しました。');
      }
    }
  };

  // SHIFT_JIS에서 역슬래시를 엔화 기호로 변환하는 함수
  const convertBackslashToYen = (text: string, encoding: string) => {
    if (encoding?.toLowerCase().includes('shift') || encoding?.toLowerCase().includes('sjis')) {
      return text.replace(/\\/g, '¥');
    }
    return text;
  };

  // 바이트 단위로 문자열을 처리하는 함수
  const getByteLength = (text: string, encoding: string = 'utf8') => {
    if (encoding?.toLowerCase().includes('shift') || encoding?.toLowerCase().includes('sjis')) {
      // SHIFT_JIS: 반각=1바이트, 전각=2바이트
      let byteLength = 0;
      for (let i = 0; i < text.length; i++) {
        const char = text[i];
        const code = char.charCodeAt(0);
        
        // ASCII 범위 (반각) = 1바이트
        if (code <= 0x7F) {
          byteLength += 1;
        }
        // 일본어 전각 문자 = 2바이트
        else if (code >= 0x3040 && code <= 0x309F) { // 히라가나
          byteLength += 2;
        } else if (code >= 0x30A0 && code <= 0x30FF) { // 가타카나
          byteLength += 2;
        } else if (code >= 0x4E00 && code <= 0x9FAF) { // 한자
          byteLength += 2;
        } else if (code >= 0xFF00 && code <= 0xFFEF) { // 전각 영숫자/기호
          byteLength += 2;
        } else {
          // 기타 문자는 2바이트로 추정
          byteLength += 2;
        }
      }
      return byteLength;
    }
    
    // UTF-8: 문자 단위로 계산
    return text.length;
  };

  // 바이트 단위로 문자열을 자르는 함수
  const sliceByBytes = (text: string, start: number, length: number, encoding: string = 'utf8') => {
    if (encoding?.toLowerCase().includes('shift') || encoding?.toLowerCase().includes('sjis')) {
      let result = '';
      let byteCount = 0;
      let charIndex = 0;
      
      // start 바이트까지 건너뛰기
      while (charIndex < text.length && byteCount < start) {
        const char = text[charIndex];
        const charByteLength = getCharByteLength(char, encoding);
        
        if (byteCount + charByteLength <= start) {
          byteCount += charByteLength;
          charIndex++;
        } else {
          break;
        }
      }
      
      // length 바이트만큼 추출
      byteCount = 0;
      while (charIndex < text.length && byteCount < length) {
        const char = text[charIndex];
        const charByteLength = getCharByteLength(char, encoding);
        
        if (byteCount + charByteLength <= length) {
          result += char;
          byteCount += charByteLength;
          charIndex++;
        } else {
          break;
        }
      }
      
      return result;
    }
    
    // UTF-8: 일반적인 substring 사용
    return text.substring(start, start + length);
  };

  // 개별 문자의 바이트 길이를 계산하는 함수
  const getCharByteLength = (char: string, encoding: string = 'utf8') => {
    if (encoding?.toLowerCase().includes('shift') || encoding?.toLowerCase().includes('sjis')) {
      const code = char.charCodeAt(0);
      
      if (code <= 0x7F) return 1; // ASCII
      if (code >= 0x3040 && code <= 0x309F) return 2; // 히라가나
      if (code >= 0x30A0 && code <= 0x30FF) return 2; // 가타카나
      if (code >= 0x4E00 && code <= 0x9FAF) return 2; // 한자
      if (code >= 0xFF00 && code <= 0xFFEF) return 2; // 전각 영숫자/기호
      
      return 2; // 기타 전각 문자
    }
    
    return 1; // UTF-8 기본
  };

  const formatDatasetContent = (data: string, rectype: string, reclen: number, page: number = 1, pageSize: number = 50, encoding: string = 'utf8') => {
    if (!data) return { content: 'データがありません。', totalRecords: 0 };
    
    console.log('formatDatasetContent called with:', { 
      dataLength: data.length, 
      rectype, 
      reclen, 
      page, 
      pageSize,
      encoding
    });
    
    const records = [];
    let recordNumber = 1;
    
    // Ensure reclen is a valid number
    const validReclen = Math.max(1, reclen || 80);
    
    // Parse all records based on record type
    switch (rectype?.toUpperCase()) {
      case 'FB': {
        // Fixed Block - Server already sends records separated by newlines
        const lines = data.split('\n');
        lines.forEach((line) => {
          if (line.length > 0) {
            // SHIFT_JIS에서 역슬래시를 엔화 기호로 변환
            const convertedRecord = convertBackslashToYen(line, encoding);
            records.push({ number: recordNumber, content: convertedRecord });
            recordNumber++;
          }
        });
        break;
      }
      
      case 'VB': {
        // Variable Block - 可変長レコード (선택적으로 바이트 단위 처리)
        let charOffset = 0;
        while (charOffset < data.length - 4) {
          try {
            const lengthBytes = data.substr(charOffset, 4);
            let recordLength = 0;
            
            // Try to parse length from first 4 bytes
            recordLength = (lengthBytes.charCodeAt(0) << 24) |
                          (lengthBytes.charCodeAt(1) << 16) |
                          (lengthBytes.charCodeAt(2) << 8) |
                          lengthBytes.charCodeAt(3);
            
            if (recordLength <= 4 || recordLength > data.length - charOffset) {
              recordLength = parseInt(lengthBytes, 10) || validReclen;
            }
            
            recordLength = Math.min(recordLength, data.length - charOffset);
            
            const record = data.substr(charOffset + 4, recordLength - 4);
            if (record.length === 0) break;
            
            const convertedRecord = convertBackslashToYen(record, encoding);
            records.push({ number: recordNumber, content: convertedRecord });
            charOffset += recordLength;
            recordNumber++;
          } catch (e) {
            console.warn('VB record parsing error at offset', charOffset, e);
            break;
          }
        }
        break;
      }
      
      case 'V': {
        // Variable length without length prefix - split by delimiters or newlines
        const lines = data.split('\n');
        lines.forEach((line, index) => {
          if (line.trim().length > 0) {
            const convertedLine = convertBackslashToYen(line, encoding);
            records.push({ number: index + 1, content: convertedLine });
          }
        });
        break;
      }
      
      case 'U': {
        // Undefined/Unstructured - treat as single block or split by natural boundaries
        const lines = data.split('\n');
        if (lines.length > 1) {
          lines.forEach((line, index) => {
            const convertedLine = convertBackslashToYen(line, encoding);
            records.push({ number: index + 1, content: convertedLine });
          });
        } else {
          // Single block, split by reclen (바이트 단위)
          let localByteOffset = 0;
          while (localByteOffset < getByteLength(data, encoding)) {
            const record = sliceByBytes(data, localByteOffset, validReclen, encoding);
            if (record.length === 0) break;
            
            const convertedRecord = convertBackslashToYen(record, encoding);
            records.push({ number: recordNumber, content: convertedRecord });
            localByteOffset += validReclen;
            recordNumber++;
          }
        }
        break;
      }
      
      default: {
        // Unknown record type - treat as fixed length (바이트 단위)
        console.warn('Unknown record type:', rectype, 'treating as fixed length');
        let defaultByteOffset = 0;
        while (defaultByteOffset < getByteLength(data, encoding)) {
          const record = sliceByBytes(data, defaultByteOffset, validReclen, encoding);
          if (record.length === 0) break;
          
          const convertedRecord = convertBackslashToYen(record, encoding);
          records.push({ number: recordNumber, content: convertedRecord });
          defaultByteOffset += validReclen;
          recordNumber++;
        }
        break;
      }
    }
    
    console.log('Parsed records:', records.length, 'total records');
    
    // Calculate pagination
    const totalRecords = records.length;
    const startIndex = (page - 1) * pageSize;
    const endIndex = startIndex + pageSize;
    const pageRecords = records.slice(startIndex, endIndex);
    
    // Format page records with line breaks
    const formattedRecords = pageRecords.map(record => 
      `${record.number.toString().padStart(5, '0')}: ${record.content}`
    );
    
    return {
      content: formattedRecords.join('\n'),
      totalRecords: totalRecords,
      currentPage: page,
      totalPages: Math.ceil(totalRecords / pageSize)
    };
  };

  const renderDatasetMonitoring = () => {
    const handleLockSelection = (lockKey: string, checked: boolean) => {
      const newSelection = new Set(selectedLocks);
      if (checked) {
        newSelection.add(lockKey);
      } else {
        newSelection.delete(lockKey);
      }
      setSelectedLocks(newSelection);
    };

    const handleSelectAll = () => {
      if (selectedLocks.size === datasetLocks.length) {
        setSelectedLocks(new Set());
      } else {
        const allKeys = datasetLocks.map(lock => `${lock.dataset}_${lock.pid}`);
        setSelectedLocks(new Set(allKeys));
      }
    };

    const formatLockTime = (timestamp?: string) => {
      if (!timestamp) return '不明';
      
      // Check if timestamp is already in HH:MM:SS format
      if (/^\d{2}:\d{2}:\d{2}$/.test(timestamp)) {
        return timestamp; // Return HH:MM:SS format as-is
      }
      
      // Handle other timestamp formats (ISO 8601, etc.)
      try {
        return new Date(timestamp).toLocaleString('ja-JP');
      } catch {
        return timestamp;
      }
    };
    
    return (
      <div className="space-y-6">
        <div className="flex justify-between items-center">
          <h3 className="text-xl font-semibold text-white">データセットモニタリング</h3>
          <div className="flex space-x-2">
            {selectedLocks.size > 0 && (
              <button 
                onClick={cleanupSelectedLocks}
                className="px-4 py-2 bg-red-600 text-white rounded-lg hover:bg-red-700 transition-colors flex items-center"
              >
                <TrashIcon className="w-4 h-4 mr-2" />
                選択したロックをクリーンアップ ({selectedLocks.size})
              </button>
            )}
            <button 
              onClick={fetchDatasetLocks}
              className="px-4 py-2 bg-gray-600 text-white rounded-lg hover:bg-gray-700 transition-colors flex items-center"
              disabled={isLoadingLocks}
            >
              {isLoadingLocks ? (
                <ArrowPathIcon className="w-4 h-4 mr-2 animate-spin" />
              ) : (
                <ArrowPathIcon className="w-4 h-4 mr-2" />
              )}
              更新
            </button>
          </div>
        </div>

        {/* Dataset Locks Overview */}
        <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
          <div className="bg-gray-800 rounded-lg p-6 border border-gray-700">
            <div className="flex items-center justify-between mb-4">
              <h4 className="text-lg font-semibold text-white">総ロック数</h4>
              <CircleStackIcon className="w-6 h-6 text-blue-400" />
            </div>
            <div className="text-3xl font-bold text-white">
              {datasetLocks.length}
            </div>
            <div className="text-sm text-gray-400 mt-2">
              選択中: {selectedLocks.size}
            </div>
          </div>

          <div className="bg-gray-800 rounded-lg p-6 border border-gray-700">
            <div className="flex items-center justify-between mb-4">
              <h4 className="text-lg font-semibold text-white">排他ロック (OLD/MOD)</h4>
              <ExclamationTriangleIcon className="w-6 h-6 text-orange-400" />
            </div>
            <div className="text-3xl font-bold text-white">
              {datasetLocks.filter(lock => lock.level === 'OLD' || lock.level === 'MOD').length}
            </div>
            <div className="text-sm text-gray-400 mt-2">
              共有ロック (SHR): {datasetLocks.filter(lock => lock.level === 'SHR').length}
            </div>
          </div>

          <div className="bg-gray-800 rounded-lg p-6 border border-gray-700">
            <div className="flex items-center justify-between mb-4">
              <h4 className="text-lg font-semibold text-white">ユニークユーザー</h4>
              <UserGroupIcon className="w-6 h-6 text-green-400" />
            </div>
            <div className="text-3xl font-bold text-white">
              {new Set(datasetLocks.map(lock => lock.user)).size}
            </div>
            <div className="text-sm text-gray-400 mt-2">
              アクティブプロセス: {new Set(datasetLocks.map(lock => lock.pid)).size}
            </div>
          </div>
        </div>

        {/* Dataset Locks Table */}
        <div className="bg-gray-800 rounded-lg border border-gray-700 overflow-hidden">
          <div className="p-6 border-b border-gray-700">
            <h4 className="text-lg font-semibold text-white">データセットロック一覧</h4>
          </div>
          
          {isLoadingLocks ? (
            <div className="p-8 text-center">
              <ArrowPathIcon className="w-8 h-8 text-blue-400 animate-spin mx-auto mb-4" />
              <p className="text-gray-400">データセットロック情報を取得中...</p>
            </div>
          ) : datasetLocks.length > 0 ? (
            <div className="overflow-x-auto">
              <table className="w-full">
                <thead className="bg-gray-900">
                  <tr>
                    <th className="px-4 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider">
                      <input
                        type="checkbox"
                        className="rounded border-gray-600 bg-gray-700 text-blue-600"
                        checked={selectedLocks.size === datasetLocks.length && datasetLocks.length > 0}
                        onChange={handleSelectAll}
                      />
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider">データセット名</th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider">ロックモード</th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider">ユーザー名</th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider">PID</th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider">ロック時間</th>
                  </tr>
                </thead>
                <tbody className="divide-y divide-gray-700">
                  {datasetLocks.map((lock, index) => {
                    const lockKey = `${lock.dataset}_${lock.pid}`;
                    return (
                      <tr key={lockKey} className="hover:bg-gray-750">
                        <td className="px-4 py-4 whitespace-nowrap">
                          <input
                            type="checkbox"
                            className="rounded border-gray-600 bg-gray-700 text-blue-600"
                            checked={selectedLocks.has(lockKey)}
                            onChange={(e) => handleLockSelection(lockKey, e.target.checked)}
                          />
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm text-white font-mono">
                          {lock.dataset}
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm">
                          <span className={`inline-flex px-2 py-1 text-xs font-semibold rounded-full ${
                            lock.level === 'OLD' || lock.level === 'MOD' 
                              ? 'bg-red-900/30 text-red-400 border border-red-700' 
                              : 'bg-green-900/30 text-green-400 border border-green-700'
                          }`}>
                            {lock.level}
                          </span>
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-300">
                          {lock.user}
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-300 font-mono">
                          {lock.pid}
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-300">
                          {formatLockTime(lock.lock_time)}
                        </td>
                      </tr>
                    );
                  })}
                </tbody>
              </table>
            </div>
          ) : (
            <div className="p-8 text-center">
              <CircleStackIcon className="w-8 h-8 text-gray-500 mx-auto mb-4" />
              <p className="text-gray-400">現在アクティブなデータセットロックはありません</p>
            </div>
          )}
        </div>
      </div>
    );
  };

  const extractProgramFromJobName = (jobName: string): string => {
    // Extract program name from job name (e.g., JOB_CRTFILE01_123456 -> CRTFILE01)
    const match = jobName.match(/JOB_([A-Z0-9]+)_\d+/);
    return match ? match[1] : jobName.replace('JOB_', '').split('_')[0];
  };

  const calculateRunningTime = (startTime: string): string => {
    if (!startTime) return '00:00:00';
    
    const now = new Date();
    const start = new Date();
    const [hours, minutes, seconds] = startTime.split(':').map(Number);
    start.setHours(hours, minutes, seconds);
    
    const diff = Math.floor((now.getTime() - start.getTime()) / 1000);
    const h = Math.floor(diff / 3600);
    const m = Math.floor((diff % 3600) / 60);
    const s = diff % 60;
    
    return `${h.toString().padStart(2, '0')}:${m.toString().padStart(2, '0')}:${s.toString().padStart(2, '0')}`;
  };

  const getStatusColor = (status: string) => {
    switch (status) {
      case 'running': return 'text-green-400 bg-green-900/20';
      case 'completed': return 'text-blue-400 bg-blue-900/20';
      case 'failed': return 'text-red-400 bg-red-900/20';
      case 'pending': return 'text-yellow-400 bg-yellow-900/20';
      case 'held': return 'text-gray-400 bg-gray-900/20';
      default: return 'text-gray-400 bg-gray-900/20';
    }
  };

  const renderOverview = () => (
    <div className="space-y-6">
      {/* System Status Cards */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
        <div className="bg-gray-800 rounded-lg p-6 border border-gray-700">
          <div className="flex items-center justify-between">
            <div>
              <p className="text-gray-400 text-sm">CPU Usage</p>
              <p className="text-2xl font-semibold text-white">{systemData?.system_info.cpu_percent.toFixed(1)}%</p>
            </div>
            <CpuChipIcon className="w-8 h-8 text-blue-400" />
          </div>
          <div className="mt-4 bg-gray-700 rounded-full h-2">
            <div 
              className="bg-blue-400 h-2 rounded-full transition-all duration-300" 
              style={{ width: `${systemData?.system_info.cpu_percent}%` }}
            />
          </div>
        </div>

        <div className="bg-gray-800 rounded-lg p-6 border border-gray-700">
          <div className="flex items-center justify-between">
            <div>
              <p className="text-gray-400 text-sm">Memory</p>
              <p className="text-2xl font-semibold text-white">{systemData?.system_info.memory_percent.toFixed(1)}%</p>
            </div>
            <CircleStackIcon className="w-8 h-8 text-green-400" />
          </div>
          <div className="mt-4 bg-gray-700 rounded-full h-2">
            <div 
              className="bg-green-400 h-2 rounded-full transition-all duration-300" 
              style={{ width: `${systemData?.system_info.memory_percent}%` }}
            />
          </div>
        </div>

        <div className="bg-gray-800 rounded-lg p-6 border border-gray-700">
          <div className="flex items-center justify-between">
            <div>
              <p className="text-gray-400 text-sm">Disk Usage</p>
              <p className="text-2xl font-semibold text-white">{systemData?.system_info.disk_percent.toFixed(1)}%</p>
            </div>
            <ServerIcon className="w-8 h-8 text-purple-400" />
          </div>
          <div className="mt-4 bg-gray-700 rounded-full h-2">
            <div 
              className="bg-purple-400 h-2 rounded-full transition-all duration-300" 
              style={{ width: `${systemData?.system_info.disk_percent}%` }}
            />
          </div>
        </div>

        <div className="bg-gray-800 rounded-lg p-6 border border-gray-700">
          <div className="flex items-center justify-between">
            <div>
              <p className="text-gray-400 text-sm">Processes</p>
              <p className="text-2xl font-semibold text-white">{systemData?.system_info.process_count}</p>
            </div>
            <ClockIcon className="w-8 h-8 text-orange-400" />
          </div>
          <p className="text-sm text-gray-400 mt-2">Uptime: {systemData?.system_info.uptime}</p>
        </div>
      </div>

      {/* Alerts */}
      {systemData?.alerts && systemData.alerts.length > 0 && (
        <div className="bg-gray-800 rounded-lg p-6 border border-gray-700">
          <h3 className="text-lg font-semibold text-white mb-4 flex items-center">
            <ExclamationTriangleIcon className="w-5 h-5 mr-2 text-yellow-400" />
            System Alerts
          </h3>
          <div className="space-y-2">
            {systemData.alerts.map((alert, index) => (
              <div key={index} className={`p-3 rounded border-l-4 ${
                alert.type === 'warning' ? 'bg-yellow-900/20 border-yellow-400' : 'bg-blue-900/20 border-blue-400'
              }`}>
                <p className="text-white">{alert.message}</p>
                <p className="text-xs text-gray-400">{new Date(alert.timestamp).toLocaleTimeString()}</p>
              </div>
            ))}
          </div>
        </div>
      )}

      {/* System Information */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        <div className="bg-gray-800 rounded-lg p-6 border border-gray-700">
          <h3 className="text-lg font-semibold text-white mb-4">System Information</h3>
          <div className="space-y-3">
            <div className="flex justify-between">
              <span className="text-gray-400">Hostname:</span>
              <span className="text-white">{systemData?.system_info.hostname}</span>
            </div>
            <div className="flex justify-between">
              <span className="text-gray-400">CPU Cores:</span>
              <span className="text-white">{systemData?.system_info.cpu_count}</span>
            </div>
            <div className="flex justify-between">
              <span className="text-gray-400">Total Memory:</span>
              <span className="text-white">{systemData?.system_info.memory_total}</span>
            </div>
            <div className="flex justify-between">
              <span className="text-gray-400">Total Disk:</span>
              <span className="text-white">{systemData?.system_info.disk_total}</span>
            </div>
            <div className="flex justify-between">
              <span className="text-gray-400">Load Average:</span>
              <span className="text-white">{systemData?.system_info.load_avg.map(l => l.toFixed(2)).join(', ')}</span>
            </div>
          </div>
        </div>

        <div className="bg-gray-800 rounded-lg p-6 border border-gray-700">
          <h3 className="text-lg font-semibold text-white mb-4">Top Processes</h3>
          <div className="space-y-2">
            {systemData?.processes.slice(0, 5).map((proc) => (
              <div key={proc.pid} className="flex justify-between items-center py-2 border-b border-gray-700">
                <div>
                  <p className="text-white font-medium">{proc.name}</p>
                  <p className="text-xs text-gray-400">PID: {proc.pid} | User: {proc.user}</p>
                </div>
                <div className="text-right">
                  <p className="text-white">{proc.cpu_percent.toFixed(1)}% CPU</p>
                  <p className="text-xs text-gray-400">{proc.memory_percent.toFixed(1)}% MEM</p>
                </div>
              </div>
            ))}
          </div>
        </div>
      </div>
    </div>
  );

  const renderJobs = () => (
    <div className="space-y-6">
      <div className="flex justify-between items-center">
        <h3 className="text-xl font-semibold text-white">ジョブ管理</h3>
        <div className="flex space-x-2 items-center">
          <span className="text-sm text-gray-400">
            総件数: {jobs.length}件
          </span>
          <button 
            onClick={fetchJobs}
            className="px-4 py-2 bg-gray-600 text-white rounded-lg hover:bg-gray-700 transition-colors"
          >
            更新
          </button>
        </div>
      </div>

      <div className="bg-gray-800 rounded-lg border border-gray-700 overflow-hidden">
        <table className="w-full">
          <thead className="bg-gray-900">
            <tr>
              <th 
                className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider cursor-pointer hover:bg-gray-800 transition-colors"
                onClick={() => handleSort('id')}
              >
                <div className="flex items-center">
                  ジョブID
                  {getSortIcon('id')}
                </div>
              </th>
              <th 
                className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider cursor-pointer hover:bg-gray-800 transition-colors"
                onClick={() => handleSort('name')}
              >
                <div className="flex items-center">
                  名前
                  {getSortIcon('name')}
                </div>
              </th>
              <th 
                className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider cursor-pointer hover:bg-gray-800 transition-colors"
                onClick={() => handleSort('status')}
              >
                <div className="flex items-center">
                  ステータス
                  {getSortIcon('status')}
                </div>
              </th>
              <th 
                className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider cursor-pointer hover:bg-gray-800 transition-colors"
                onClick={() => handleSort('user')}
              >
                <div className="flex items-center">
                  ユーザー
                  {getSortIcon('user')}
                </div>
              </th>
              <th 
                className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider cursor-pointer hover:bg-gray-800 transition-colors"
                onClick={() => handleSort('start_time')}
              >
                <div className="flex items-center">
                  開始時刻
                  {getSortIcon('start_time')}
                </div>
              </th>
              <th className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider">CPU時間</th>
              <th 
                className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider cursor-pointer hover:bg-gray-800 transition-colors"
                onClick={() => handleSort('priority')}
              >
                <div className="flex items-center">
                  優先度
                  {getSortIcon('priority')}
                </div>
              </th>
              <th className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider">アクション</th>
            </tr>
          </thead>
          <tbody className="divide-y divide-gray-700">
            {getSortedJobs().map((job) => (
              <tr 
                key={job.id} 
                className="hover:bg-gray-750 cursor-pointer"
                onDoubleClick={() => fetchJobLog(job)}
              >
                <td className="px-6 py-4 whitespace-nowrap text-sm text-white">{job.id}</td>
                <td className="px-6 py-4 whitespace-nowrap text-sm text-white">{job.name}</td>
                <td className="px-6 py-4 whitespace-nowrap">
                  <span className={`px-2 py-1 text-xs rounded-full ${getStatusColor(job.status)}`}>
                    {job.status.toUpperCase()}
                  </span>
                </td>
                <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-300">{job.user || 'system'}</td>
                <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-300">{job.start_time || '-'}</td>
                <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-300">{job.cpu_time || '00:00:00'}</td>
                <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-300">{job.priority || 5}</td>
                <td className="px-6 py-4 whitespace-nowrap text-sm">
                  <div className="flex space-x-2">
                    {job.status === 'running' && (
                      <>
                        <button 
                          onClick={(e) => {
                            e.stopPropagation();
                            handleJobAction(job.id, 'hold');
                          }}
                          className="text-yellow-400 hover:text-yellow-300"
                          title="Hold"
                        >
                          <PauseIcon className="w-4 h-4" />
                        </button>
                        <button 
                          onClick={(e) => {
                            e.stopPropagation();
                            handleJobAction(job.id, 'cancel');
                          }}
                          className="text-red-400 hover:text-red-300"
                          title="Cancel"
                        >
                          <StopIcon className="w-4 h-4" />
                        </button>
                      </>
                    )}
                    {job.status === 'held' && (
                      <button 
                        onClick={(e) => {
                          e.stopPropagation();
                          handleJobAction(job.id, 'resume');
                        }}
                        className="text-green-400 hover:text-green-300"
                        title="Resume"
                      >
                        <PlayIcon className="w-4 h-4" />
                      </button>
                    )}
                    {(job.status === 'pending' || job.status === 'error' || job.status === 'completed') && (
                      <button 
                        onClick={(e) => {
                          e.stopPropagation();
                          handleJobAction(job.id, 'start');
                        }}
                        className="text-blue-400 hover:text-blue-300"
                        title={job.status === 'pending' ? 'Start' : 'Restart'}
                      >
                        <PlayIcon className="w-4 h-4" />
                      </button>
                    )}
                    <button 
                      onClick={(e) => {
                        e.stopPropagation();
                        fetchJobLog(job);
                      }}
                      className="text-blue-400 hover:text-blue-300"
                      title="View Log"
                    >
                      <DocumentTextIcon className="w-4 h-4" />
                    </button>
                    <button 
                      onClick={(e) => {
                        e.stopPropagation();
                        handleDeleteJob(job.id);
                      }}
                      className="text-red-400 hover:text-red-300"
                      title="Delete"
                    >
                      <TrashIcon className="w-4 h-4" />
                    </button>
                  </div>
                </td>
              </tr>
            ))}
          </tbody>
        </table>
        {jobs.length === 0 && (
          <div className="p-8 text-center text-gray-400">
            現在実行中のジョブはありません
          </div>
        )}
      </div>
    </div>
  );

  const renderCatalog = () => {
    if (!catalogData) {
      return (
        <div className="flex items-center justify-center h-64">
          <div className="text-center">
            <ArrowPathIcon className="w-8 h-8 text-blue-400 animate-spin mx-auto mb-4" />
            <p className="text-gray-400">Loading catalog data...</p>
          </div>
        </div>
      );
    }

    const resources: Array<{name: string, type: string, library: string, volume: string, owner: string, resource: CatalogResource}> = [];
    
    Object.entries(catalogData).forEach(([volumeName, volume]) => {
      if (typeof volume === 'object' && volume !== null) {
        Object.entries(volume).forEach(([libraryName, library]) => {
          if (typeof library === 'object' && library !== null) {
            Object.entries(library).forEach(([resourceName, resourceData]) => {
              if (typeof resourceData === 'object' && resourceData !== null && 'TYPE' in resourceData) {
                const resource = resourceData as CatalogResource;
                resources.push({
                  name: resourceName,
                  type: resource.TYPE,
                  library: libraryName,
                  volume: volumeName,
                  owner: 'admin',
                  resource: resource
                });
              }
            });
          }
        });
      }
    });

    // Sort resources based on current sort settings
    const sortedResources = [...resources].sort((a, b) => {
      let aValue: any;
      let bValue: any;

      switch (catalogSortField) {
        case 'name':
          aValue = a.name.toLowerCase();
          bValue = b.name.toLowerCase();
          break;
        case 'type':
          aValue = (a.resource.PGMTYPE || a.type).toLowerCase();
          bValue = (b.resource.PGMTYPE || b.type).toLowerCase();
          break;
        case 'library':
          aValue = a.library.toLowerCase();
          bValue = b.library.toLowerCase();
          break;
        case 'volume':
          aValue = a.volume.toLowerCase();
          bValue = b.volume.toLowerCase();
          break;
        case 'owner':
          aValue = a.owner.toLowerCase();
          bValue = b.owner.toLowerCase();
          break;
        default:
          aValue = a.name.toLowerCase();
          bValue = b.name.toLowerCase();
      }

      if (catalogSortDirection === 'asc') {
        return aValue > bValue ? 1 : aValue < bValue ? -1 : 0;
      } else {
        return aValue < bValue ? 1 : aValue > bValue ? -1 : 0;
      }
    });

    return (
      <div className="space-y-6">
        <div className="flex justify-between items-center">
          <h3 className="text-xl font-semibold text-white">カタログ管理</h3>
          <button 
            onClick={fetchCatalogData}
            className="px-4 py-2 bg-gray-600 text-white rounded-lg hover:bg-gray-700 transition-colors"
          >
            更新
          </button>
        </div>

        <div className="bg-gray-800 rounded-lg border border-gray-700 overflow-hidden">
          <table className="w-full">
            <thead className="bg-gray-900">
              <tr>
                <th 
                  className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider cursor-pointer hover:bg-gray-800 transition-colors"
                  onClick={() => handleCatalogSort('name')}
                >
                  <div className="flex items-center">
                    リソース名
                    {getCatalogSortIcon('name')}
                  </div>
                </th>
                <th 
                  className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider cursor-pointer hover:bg-gray-800 transition-colors"
                  onClick={() => handleCatalogSort('type')}
                >
                  <div className="flex items-center">
                    タイプ名
                    {getCatalogSortIcon('type')}
                  </div>
                </th>
                <th 
                  className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider cursor-pointer hover:bg-gray-800 transition-colors"
                  onClick={() => handleCatalogSort('library')}
                >
                  <div className="flex items-center">
                    ライブラリ名
                    {getCatalogSortIcon('library')}
                  </div>
                </th>
                <th 
                  className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider cursor-pointer hover:bg-gray-800 transition-colors"
                  onClick={() => handleCatalogSort('volume')}
                >
                  <div className="flex items-center">
                    ボリューム名
                    {getCatalogSortIcon('volume')}
                  </div>
                </th>
                <th 
                  className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider cursor-pointer hover:bg-gray-800 transition-colors"
                  onClick={() => handleCatalogSort('owner')}
                >
                  <div className="flex items-center">
                    所有者
                    {getCatalogSortIcon('owner')}
                  </div>
                </th>
              </tr>
            </thead>
            <tbody className="divide-y divide-gray-700">
              {sortedResources.map((resource, index) => (
                <tr 
                  key={index} 
                  className="hover:bg-gray-750 cursor-pointer"
                  onDoubleClick={() => {
                    if (resource.resource.PGMTYPE === 'CL') {
                      handleCLDoubleClick(resource);
                    } else {
                      setSelectedResource(resource);
                    }
                  }}
                >
                  <td className="px-6 py-4 whitespace-nowrap text-sm text-white font-mono">{resource.name}</td>
                  <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-300">{resource.resource.PGMTYPE || resource.type}</td>
                  <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-300">{resource.library}</td>
                  <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-300">{resource.volume}</td>
                  <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-300">{resource.owner}</td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </div>
    );
  };

  const renderQueues = () => (
    <div className="space-y-6">
      <div className="flex justify-between items-center">
        <h3 className="text-xl font-semibold text-white">Queue Management</h3>
        <button className="px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors">
          Add Queue
        </button>
      </div>

      <div className="bg-gray-800 rounded-lg p-6 border border-gray-700">
        <p className="text-gray-400 text-center">Queue management data will be available when integrated with job system.</p>
      </div>
    </div>
  );

  const renderDatasets = () => {
    if (!catalogData) {
      return (
        <div className="flex items-center justify-center h-64">
          <div className="text-center">
            <ArrowPathIcon className="w-8 h-8 text-blue-400 animate-spin mx-auto mb-4" />
            <p className="text-gray-400">Loading dataset information...</p>
          </div>
        </div>
      );
    }

    const datasets: DatasetInfo[] = [];
    
    Object.entries(catalogData).forEach(([volumeName, volume]) => {
      if (typeof volume === 'object' && volume !== null) {
        Object.entries(volume).forEach(([libraryName, library]) => {
          if (typeof library === 'object' && library !== null) {
            Object.entries(library).forEach(([resourceName, resourceData]) => {
              if (typeof resourceData === 'object' && resourceData !== null && 'TYPE' in resourceData) {
                const resource = resourceData as CatalogResource;
                if (resource.TYPE === 'DATASET') {
                  datasets.push({
                    name: resourceName,
                    type: resource.TYPE,
                    library: libraryName,
                    volume: volumeName,
                    rectype: resource.RECTYPE,
                    reclen: resource.RECLEN,
                    encoding: resource.ENCODING,
                    description: resource.DESCRIPTION || 'No description'
                  } as DatasetInfo);
                }
              }
            });
          }
        });
      }
    });

    return (
      <div className="space-y-6">
        <div className="flex justify-between items-center">
          <h3 className="text-xl font-semibold text-white">Dataset Management</h3>
          <div className="flex space-x-2">
            <button className="px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors">
              Create Dataset
            </button>
            <button 
              onClick={() => setActiveSection('catalog')}
              className="px-4 py-2 bg-gray-600 text-white rounded-lg hover:bg-gray-700 transition-colors"
            >
              Catalog Browse
            </button>
          </div>
        </div>

        <div className="bg-gray-800 rounded-lg border border-gray-700 overflow-hidden">
          <table className="w-full">
            <thead className="bg-gray-900">
              <tr>
                <th className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider">Dataset Name</th>
                <th className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider">Record Type</th>
                <th className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider">Library</th>
                <th className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider">Volume</th>
                <th className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider">Description</th>
                <th className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider">Actions</th>
              </tr>
            </thead>
            <tbody className="divide-y divide-gray-700">
              {datasets.map((dataset, index) => (
                <tr key={index} className="hover:bg-gray-750">
                  <td className="px-6 py-4 whitespace-nowrap text-sm text-white font-mono">{dataset.name}</td>
                  <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-300">{dataset.rectype || 'N/A'}</td>
                  <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-300">{dataset.library}</td>
                  <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-300">{dataset.volume}</td>
                  <td className="px-6 py-4 text-sm text-gray-300">{dataset.description}</td>
                  <td className="px-6 py-4 whitespace-nowrap text-sm">
                    <div className="flex space-x-2">
                      <button 
                        onClick={() => viewDataset(dataset)}
                        className="text-blue-400 hover:text-blue-300 disabled:text-gray-500"
                        disabled={isLoadingDataset}
                      >
                        表示
                      </button>
                      <button 
                        onClick={() => editDataset(dataset)}
                        className="text-green-400 hover:text-green-300"
                      >
                        編集
                      </button>
                      <button 
                        onClick={() => deleteDataset(dataset)}
                        className="text-red-400 hover:text-red-300"
                      >
                        削除
                      </button>
                    </div>
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
          {datasets.length === 0 && (
            <div className="p-8 text-center text-gray-400">
              No datasets found in catalog
            </div>
          )}
        </div>
      </div>
    );
  };

  const renderContent = () => {
    switch (activeSection) {
      case 'overview': return renderOverview();
      case 'catalog': return renderCatalog();
      case 'jobs': return renderJobs();
      case 'queues': return renderQueues();
      case 'datasets': return renderDatasets();
      case 'resources': return renderDatasetMonitoring();
      default: 
        return (
          <div className="flex items-center justify-center h-64">
            <div className="text-center">
              <Cog6ToothIcon className="w-16 h-16 text-gray-400 mx-auto mb-4" />
              <h3 className="text-lg font-medium text-gray-300">Feature Coming Soon</h3>
              <p className="text-gray-400">This section is under development</p>
            </div>
          </div>
        );
    }
  };

  if (loading) {
    return (
      <div className="flex items-center justify-center h-64">
        <div className="text-center">
          <ArrowPathIcon className="w-8 h-8 text-blue-400 animate-spin mx-auto mb-4" />
          <p className="text-gray-400">Loading system data...</p>
        </div>
      </div>
    );
  }

  return (
    <div className="flex h-full bg-gray-900">
      {/* Sidebar Navigation */}
      <div className="w-64 bg-gray-800 border-r border-gray-700 p-4">
        <h2 className="text-xl font-bold text-white mb-6">System Manager</h2>
        <nav className="space-y-2">
          {sections.map((section) => (
            <button
              key={section.id}
              onClick={() => setActiveSection(section.id)}
              className={`w-full flex items-center px-3 py-2 text-left rounded-lg transition-colors ${
                activeSection === section.id
                  ? 'bg-blue-600 text-white'
                  : 'text-gray-300 hover:bg-gray-700 hover:text-white'
              }`}
            >
              {section.icon}
              <span className="ml-3">{section.label}</span>
            </button>
          ))}
        </nav>
      </div>

      {/* Main Content */}
      <div className="flex-1 p-6 overflow-auto">
        {renderContent()}
        
        {/* Resource Detail Modal */}
        {selectedResource && (
          <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50">
            <div className="bg-gray-800 rounded-lg p-6 w-96 max-w-full border border-gray-700">
              <div className="flex justify-between items-center mb-4">
                <h3 className="text-lg font-semibold text-white">リソース詳細</h3>
                <button 
                  onClick={() => setSelectedResource(null)}
                  className="text-gray-400 hover:text-white"
                >
                  ×
                </button>
              </div>
              
              <div className="space-y-3">
                <div className="grid grid-cols-2 gap-3">
                  <div>
                    <p className="text-gray-400 text-sm">名前:</p>
                    <p className="text-white font-mono">{selectedResource.name}</p>
                  </div>
                  <div>
                    <p className="text-gray-400 text-sm">タイプ:</p>
                    <p className="text-white">{selectedResource.resource.TYPE}</p>
                  </div>
                  <div>
                    <p className="text-gray-400 text-sm">ライブラリ:</p>
                    <p className="text-white">{selectedResource.library}</p>
                  </div>
                  <div>
                    <p className="text-gray-400 text-sm">ボリューム:</p>
                    <p className="text-white">{selectedResource.volume}</p>
                  </div>
                </div>
                
                <div>
                  <p className="text-gray-400 text-sm">説明:</p>
                  <p className="text-white">{selectedResource.resource.DESCRIPTION}</p>
                </div>
                
                {selectedResource.resource.VERSION && (
                  <div>
                    <p className="text-gray-400 text-sm">バージョン:</p>
                    <p className="text-white">{selectedResource.resource.VERSION}</p>
                  </div>
                )}
                
                <div className="grid grid-cols-2 gap-3">
                  <div>
                    <p className="text-gray-400 text-sm">作成日:</p>
                    <p className="text-white text-xs">{new Date(selectedResource.resource.CREATED).toLocaleString('ja-JP')}</p>
                  </div>
                  <div>
                    <p className="text-gray-400 text-sm">更新日:</p>
                    <p className="text-white text-xs">{new Date(selectedResource.resource.UPDATED).toLocaleString('ja-JP')}</p>
                  </div>
                </div>
                
                {selectedResource.resource.TYPE === 'PGM' && (
                  <div className="mt-4 pt-4 border-t border-gray-700">
                    <button 
                      className="w-full px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors disabled:bg-gray-600 disabled:cursor-not-allowed"
                      disabled={isSubmittingJob}
                      onClick={() => {
                        submitJob(selectedResource.name, selectedResource.library, selectedResource.volume);
                      }}
                    >
                      {isSubmittingJob ? '実行中...' : 'JOB実行 (SBMJOB)'}
                    </button>
                  </div>
                )}
              </div>
            </div>
          </div>
        )}
        
        {/* Dataset Viewer Modal */}
        {selectedDataset && (
          <div className="fixed inset-0 bg-black bg-opacity-50 z-50">
            {/* Modal positioned relative to main content area with proper margins */}
            <div className="fixed top-4 bottom-4 left-72 right-4 flex items-center justify-center p-4">
              <div className="bg-gray-800 rounded-lg w-full h-full max-h-[85vh] border border-gray-700 flex flex-col shadow-2xl">
              {/* Header */}
              <div className="flex justify-between items-center p-6 border-b border-gray-700">
                <div>
                  <h3 className="text-xl font-semibold text-white">
                    データセット表示: {selectedDataset.name}
                  </h3>
                  <p className="text-sm text-gray-400 mt-1">
                    {selectedDataset.volume}/{selectedDataset.library} | 
                    {selectedDataset.rectype} | 
                    RECLEN: {selectedDataset.reclen} | 
                    {selectedDataset.encoding?.toUpperCase()}
                  </p>
                </div>
                <button 
                  onClick={() => {
                    setSelectedDataset(null);
                    setHoveredChar(null);
                    setCursorPosition({row: 0, col: 0});
                  }}
                  className="text-gray-400 hover:text-white text-2xl hover:bg-gray-700 rounded p-2 transition-colors"
                >
                  ✕
                </button>
              </div>
              
              {/* Content Area */}
              <div className="flex-1 flex flex-col overflow-hidden">
                {/* Data Display Area */}
                <div className="flex-1 bg-gray-900 m-6 mb-2 rounded border border-gray-600 overflow-hidden flex flex-col">
                  {isLoadingDataset ? (
                    <div className="flex items-center justify-center h-full">
                      <div className="text-center">
                        <ArrowPathIcon className="w-8 h-8 text-blue-400 animate-spin mx-auto mb-4" />
                        <p className="text-gray-400">データを読み込んでいます...</p>
                      </div>
                    </div>
                  ) : (
                    <div 
                      className="flex-1 p-4 overflow-auto"
                      onMouseLeave={() => setHoveredChar(null)}
                    >
                      <div className="text-sm text-gray-300 font-mono whitespace-pre-wrap leading-relaxed select-text">
                        {selectedDataset.data ? 
                          (() => {
                            // 실제 dataset의 rectype, reclen, encoding 사용 (하드코딩 제거)
                            const rectype = selectedDataset.rectype || 'FB';
                            const reclen = selectedDataset.reclen || 80;
                            const encoding = selectedDataset.encoding || 'utf8';
                            
                            console.log('Dataset format info:', {
                              name: selectedDataset.name,
                              rectype: rectype,
                              reclen: reclen,
                              encoding: encoding
                            });
                            
                            // FB/VB 레코드 형식에 맞게 데이터 포맷 (바이트 단위 처리 및 페이징 적용)
                            const formatResult = formatDatasetContent(
                              selectedDataset.data, 
                              rectype,
                              reclen,
                              currentPage,
                              recordsPerPage,
                              encoding
                            );
                            
                            // Update total records for pagination
                            if (formatResult.totalRecords !== totalRecords) {
                              setTotalRecords(formatResult.totalRecords);
                            }
                            
                            const formattedData = formatResult.content;
                            
                            return formattedData.split('').map((char, index) => (
                              <span
                                key={index}
                                className={`hover:bg-blue-600 hover:text-white cursor-pointer transition-colors ${
                                  hoveredChar?.pos === index ? 'bg-blue-600 text-white' : ''
                                }`}
                                onMouseEnter={() => {
                                  const charInfo = getCharAtPosition(formattedData, index);
                                  if (charInfo) {
                                    setHoveredChar(charInfo);
                                  }
                                }}
                                onClick={() => {
                                  // 클릭된 위치의 행/열 계산
                                  const textUpToIndex = formattedData.substring(0, index);
                                  const lines = textUpToIndex.split('\n');
                                  const row = lines.length - 1;
                                  const col = lines[lines.length - 1].length;
                                  setCursorPosition({ row, col });
                                }}
                              >
                                {char === '\n' ? '\n' : char}
                              </span>
                            ));
                          })() : 
                          'データがありません。'
                        }
                      </div>
                    </div>
                  )}
                </div>

                {/* Hex Display Area */}
                <div className="mx-6 mb-4 bg-gray-800 border border-gray-600 rounded p-3">
                  <div className="flex justify-between items-center mb-2">
                    <span className="text-sm font-medium text-gray-400">文字情報</span>
                    <span className="text-xs text-gray-500">
                      行: {cursorPosition.row + 1}, 列: {cursorPosition.col + 1}
                    </span>
                  </div>
                  <div className="bg-gray-900 rounded p-3 font-mono text-sm">
                    {hoveredChar ? (
                      <div className="flex space-x-6">
                        <div>
                          <span className="text-gray-400">文字: </span>
                          <span className="text-white bg-gray-700 px-2 py-1 rounded">
                            {hoveredChar.char === ' ' ? '(Space)' : hoveredChar.char}
                          </span>
                        </div>
                        <div>
                          <span className="text-gray-400">HEX: </span>
                          <span className="text-green-400 bg-gray-700 px-2 py-1 rounded">
                            0x{hoveredChar.hex}
                          </span>
                        </div>
                        <div>
                          <span className="text-gray-400">位置: </span>
                          <span className="text-blue-400">{hoveredChar.pos}</span>
                        </div>
                        <div>
                          <span className="text-gray-400">DEC: </span>
                          <span className="text-yellow-400">{parseInt(hoveredChar.hex, 16)}</span>
                        </div>
                      </div>
                    ) : (
                      <div className="text-gray-500">
                        文字の上にマウスカーソルを移動してください
                      </div>
                    )}
                  </div>
                </div>

                {/* Footer */}
                <div className="flex justify-between items-center px-6 pb-4">
                  <div className="text-sm text-gray-400">
                    説明: {selectedDataset.description}
                  </div>
                  <div className="flex space-x-3">
                    <button 
                      onClick={() => editDataset(selectedDataset)}
                      className="px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors"
                    >
                      編集
                    </button>
                    <button 
                      onClick={() => {
                        setSelectedDataset(null);
                        setHoveredChar(null);
                        setCursorPosition({row: 0, col: 0});
                      }}
                      className="px-4 py-2 bg-gray-600 text-white rounded-lg hover:bg-gray-700 transition-colors"
                    >
                      閉じる
                    </button>
                  </div>
                </div>
              </div>
              </div>
            </div>
          </div>
        )}
        
        {/* Job Log Modal */}
        {selectedJobLog && (
          <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50">
            <div className="bg-gray-800 rounded-lg p-6 w-3/4 max-w-4xl h-3/4 border border-gray-700 flex flex-col">
              <div className="flex justify-between items-center mb-4">
                <h3 className="text-lg font-semibold text-white">
                  JOBログ: {selectedJobLog.job.name} ({selectedJobLog.job.id})
                </h3>
                <button 
                  onClick={() => setSelectedJobLog(null)}
                  className="text-gray-400 hover:text-white"
                >
                  ×
                </button>
              </div>
              
              <div className="flex-1 bg-gray-900 rounded p-4 overflow-auto">
                <pre className="text-sm text-gray-300 font-mono whitespace-pre-wrap">
                  {selectedJobLog.log}
                </pre>
              </div>
              
              <div className="mt-4 flex justify-end space-x-2">
                <button 
                  onClick={() => fetchJobLog(selectedJobLog.job)}
                  className="px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors"
                >
                  更新
                </button>
                <button 
                  onClick={() => setSelectedJobLog(null)}
                  className="px-4 py-2 bg-gray-600 text-white rounded-lg hover:bg-gray-700 transition-colors"
                >
                  閉じる
                </button>
              </div>
            </div>
          </div>
        )}

        {/* CL Viewer Modal */}
        {showCLViewer && selectedCL && (
          <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50">
            <div className="bg-gray-800 rounded-lg p-6 w-3/4 max-w-4xl h-3/4 border border-gray-700 flex flex-col">
              <div className="flex justify-between items-center mb-4">
                <div>
                  <h3 className="text-lg font-semibold text-white">
                    CLプログラム: {selectedCL.name}
                  </h3>
                  <p className="text-sm text-gray-400">
                    {selectedCL.volume}/{selectedCL.library}
                  </p>
                </div>
                <button 
                  onClick={() => setShowCLViewer(false)}
                  className="text-gray-400 hover:text-white text-2xl"
                >
                  ×
                </button>
              </div>
              
              <div className="flex-1 bg-gray-900 rounded p-4 overflow-auto">
                {isLoadingCL ? (
                  <div className="flex items-center justify-center h-full">
                    <ArrowPathIcon className="w-8 h-8 text-blue-400 animate-spin" />
                    <span className="ml-2 text-gray-400">CLファイル読み込み中...</span>
                  </div>
                ) : (
                  <pre className="text-sm text-gray-300 font-mono whitespace-pre-wrap">
                    {clContent}
                  </pre>
                )}
              </div>
              
              <div className="mt-4 flex justify-end space-x-2">
                <button 
                  onClick={executeCL}
                  disabled={isLoadingCL}
                  className="px-6 py-2 bg-green-600 text-white rounded-lg hover:bg-green-700 transition-colors font-medium disabled:bg-gray-600 disabled:cursor-not-allowed"
                >
                  <PlayIcon className="w-4 h-4 inline mr-2" />
                  実行
                </button>
                <button 
                  onClick={() => setShowCLViewer(false)}
                  className="px-4 py-2 bg-gray-600 text-white rounded-lg hover:bg-gray-700 transition-colors"
                >
                  閉じる
                </button>
              </div>
            </div>
          </div>
        )}

        {/* Dataset Edit Modal */}
        {editingDataset && (
          <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50">
            <div className="bg-gray-800 rounded-lg p-6 w-3/4 max-w-6xl h-5/6 border border-gray-700 flex flex-col">
              <div className="flex justify-between items-center mb-4">
                <div>
                  <h3 className="text-lg font-semibold text-white">
                    データセット編集: {editingDataset.name}
                  </h3>
                  <p className="text-sm text-gray-400">
                    {editingDataset.volume}/{editingDataset.library} - {editingDataset.description}
                  </p>
                  <p className="text-xs text-yellow-400 mt-1">
                    エンコーディング: {editingDataset.encoding || 'UTF-8'} 
                    {(editingDataset.encoding?.toLowerCase().includes('sjis') || 
                      editingDataset.encoding?.toLowerCase().includes('shift_jis')) ? 
                      ' (SJIS ↔ Unicode 自動変換)' : ''}
                  </p>
                </div>
                <button 
                  onClick={cancelDatasetEdit}
                  className="text-gray-400 hover:text-white text-2xl"
                >
                  ×
                </button>
              </div>
              
              <div className="flex-1 flex flex-col">
                <div className="bg-gray-900 rounded p-4 flex-1 overflow-hidden">
                  <textarea
                    value={editedData}
                    onChange={(e) => setEditedData(e.target.value)}
                    className="w-full h-full bg-transparent text-gray-300 font-mono text-sm resize-none outline-none"
                    style={{ minHeight: '400px' }}
                    placeholder="データセットの内容を編集してください..."
                  />
                </div>
                
                <div className="mt-4 flex justify-between items-center">
                  <div className="text-sm text-gray-400">
                    {editingDataset.rectype === 'FB' && `固定長レコード (${editingDataset.reclen}バイト)`}
                    {editingDataset.rectype === 'VB' && `可変長レコード (最大${editingDataset.reclen}バイト)`}
                  </div>
                  <div className="flex space-x-2">
                    <button 
                      onClick={saveDatasetChanges}
                      className="px-6 py-2 bg-green-600 text-white rounded-lg hover:bg-green-700 transition-colors font-medium"
                    >
                      保存
                    </button>
                    <button 
                      onClick={cancelDatasetEdit}
                      className="px-6 py-2 bg-gray-600 text-white rounded-lg hover:bg-gray-700 transition-colors"
                    >
                      キャンセル
                    </button>
                  </div>
                </div>
              </div>
            </div>
          </div>
        )}
      </div>
    </div>
  );
};

export default SystemManagerPage;