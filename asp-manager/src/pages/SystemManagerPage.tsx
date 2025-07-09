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
  CloudIcon
} from '@heroicons/react/24/outline';

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

const SystemManagerPage: React.FC = () => {
  const [systemData, setSystemData] = useState<SystemData | null>(null);
  const [loading, setLoading] = useState(true);
  const [activeSection, setActiveSection] = useState('overview');
  const [selectedJob, setSelectedJob] = useState<string | null>(null);

  const sections = [
    { id: 'overview', label: 'System Overview', icon: <ServerIcon className="w-5 h-5" /> },
    { id: 'jobs', label: 'Job Management', icon: <PlayIcon className="w-5 h-5" /> },
    { id: 'queues', label: 'Queue Management', icon: <QueueListIcon className="w-5 h-5" /> },
    { id: 'datasets', label: 'Dataset Management', icon: <FolderIcon className="w-5 h-5" /> },
    { id: 'resources', label: 'Resource Monitoring', icon: <ChartBarIcon className="w-5 h-5" /> },
    { id: 'users', label: 'User Management', icon: <UserGroupIcon className="w-5 h-5" /> },
    { id: 'network', label: 'Network Control', icon: <CloudIcon className="w-5 h-5" /> },
    { id: 'security', label: 'Security & Access', icon: <ShieldCheckIcon className="w-5 h-5" /> },
    { id: 'config', label: 'System Configuration', icon: <Cog6ToothIcon className="w-5 h-5" /> },
    { id: 'terminal', label: 'Command Terminal', icon: <CommandLineIcon className="w-5 h-5" /> }
  ];

  const mockJobs = [
    { id: 'JOB001', name: 'DAILY_BACKUP', status: 'running', user: 'SYSBATCH', start_time: '18:30:00', cpu_time: '00:05:23', priority: 10 },
    { id: 'JOB002', name: 'REPORT_GEN', status: 'completed', user: 'APPUSER', start_time: '18:25:00', cpu_time: '00:02:15', priority: 5 },
    { id: 'JOB003', name: 'DATA_LOAD', status: 'failed', user: 'DATAUSER', start_time: '18:20:00', cpu_time: '00:01:45', priority: 8 },
    { id: 'JOB004', name: 'FILE_CLEANUP', status: 'pending', user: 'MAINT', start_time: '19:00:00', cpu_time: '00:00:00', priority: 3 },
    { id: 'JOB005', name: 'MONTHLY_ARCH', status: 'held', user: 'ARCHIVE', start_time: '20:00:00', cpu_time: '00:00:00', priority: 1 }
  ];

  const mockQueues = [
    { name: 'BATCH_HIGH', status: 'active', jobs: 5, max_jobs: 10, description: 'High priority batch jobs' },
    { name: 'BATCH_NORMAL', status: 'active', jobs: 8, max_jobs: 15, description: 'Normal priority batch jobs' },
    { name: 'INTERACTIVE', status: 'active', jobs: 12, max_jobs: 20, description: 'Interactive user sessions' },
    { name: 'MAINTENANCE', status: 'inactive', jobs: 0, max_jobs: 5, description: 'System maintenance queue' }
  ];

  const mockDatasets = [
    { name: 'SYS.PROCLIB', type: 'PDS', volumes: ['SYS001'], size: '15.2 MB', records: 2847 },
    { name: 'USER.DATA.FILE', type: 'PS', volumes: ['DATA01'], size: '125.8 MB', records: 45621 },
    { name: 'BACKUP.GDG.BASE', type: 'GDG', volumes: ['BACKUP1'], size: '2.3 GB', records: 0 },
    { name: 'TEMP.WORK.SPACE', type: 'VSAM', volumes: ['WORK01'], size: '512 MB', records: 0 }
  ];

  useEffect(() => {
    fetchSystemData();
    const interval = setInterval(fetchSystemData, 5000);
    return () => clearInterval(interval);
  }, []);

  const fetchSystemData = async () => {
    try {
      // Try to fetch from port 3004 (standalone ASP Manager), fallback to mock data
      const response = await fetch('http://localhost:3004/api/system');
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

  const handleJobAction = (jobId: string, action: string) => {
    console.log(`Job ${jobId}: ${action}`);
    // Implement job control actions
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
        <h3 className="text-xl font-semibold text-white">Job Management</h3>
        <div className="flex space-x-2">
          <button className="px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors">
            Submit Job
          </button>
          <button className="px-4 py-2 bg-gray-600 text-white rounded-lg hover:bg-gray-700 transition-colors">
            Refresh
          </button>
        </div>
      </div>

      <div className="bg-gray-800 rounded-lg border border-gray-700 overflow-hidden">
        <table className="w-full">
          <thead className="bg-gray-900">
            <tr>
              <th className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider">Job ID</th>
              <th className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider">Name</th>
              <th className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider">Status</th>
              <th className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider">User</th>
              <th className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider">Start Time</th>
              <th className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider">CPU Time</th>
              <th className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider">Priority</th>
              <th className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider">Actions</th>
            </tr>
          </thead>
          <tbody className="divide-y divide-gray-700">
            {mockJobs.map((job) => (
              <tr key={job.id} className="hover:bg-gray-750">
                <td className="px-6 py-4 whitespace-nowrap text-sm text-white">{job.id}</td>
                <td className="px-6 py-4 whitespace-nowrap text-sm text-white">{job.name}</td>
                <td className="px-6 py-4 whitespace-nowrap">
                  <span className={`px-2 py-1 text-xs rounded-full ${getStatusColor(job.status)}`}>
                    {job.status.toUpperCase()}
                  </span>
                </td>
                <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-300">{job.user}</td>
                <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-300">{job.start_time}</td>
                <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-300">{job.cpu_time}</td>
                <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-300">{job.priority}</td>
                <td className="px-6 py-4 whitespace-nowrap text-sm">
                  <div className="flex space-x-2">
                    {job.status === 'running' && (
                      <>
                        <button 
                          onClick={() => handleJobAction(job.id, 'hold')}
                          className="text-yellow-400 hover:text-yellow-300"
                          title="Hold"
                        >
                          <PauseIcon className="w-4 h-4" />
                        </button>
                        <button 
                          onClick={() => handleJobAction(job.id, 'cancel')}
                          className="text-red-400 hover:text-red-300"
                          title="Cancel"
                        >
                          <StopIcon className="w-4 h-4" />
                        </button>
                      </>
                    )}
                    {job.status === 'held' && (
                      <button 
                        onClick={() => handleJobAction(job.id, 'resume')}
                        className="text-green-400 hover:text-green-300"
                        title="Resume"
                      >
                        <PlayIcon className="w-4 h-4" />
                      </button>
                    )}
                    {job.status === 'pending' && (
                      <button 
                        onClick={() => handleJobAction(job.id, 'start')}
                        className="text-blue-400 hover:text-blue-300"
                        title="Start"
                      >
                        <PlayIcon className="w-4 h-4" />
                      </button>
                    )}
                  </div>
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </div>
  );

  const renderQueues = () => (
    <div className="space-y-6">
      <div className="flex justify-between items-center">
        <h3 className="text-xl font-semibold text-white">Queue Management</h3>
        <button className="px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors">
          Add Queue
        </button>
      </div>

      <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
        {mockQueues.map((queue) => (
          <div key={queue.name} className="bg-gray-800 rounded-lg p-6 border border-gray-700">
            <div className="flex justify-between items-start mb-4">
              <div>
                <h4 className="text-lg font-semibold text-white">{queue.name}</h4>
                <p className="text-sm text-gray-400">{queue.description}</p>
              </div>
              <span className={`px-2 py-1 text-xs rounded-full ${
                queue.status === 'active' ? 'bg-green-900/20 text-green-400' : 'bg-red-900/20 text-red-400'
              }`}>
                {queue.status.toUpperCase()}
              </span>
            </div>
            
            <div className="space-y-3">
              <div className="flex justify-between">
                <span className="text-gray-400">Active Jobs:</span>
                <span className="text-white">{queue.jobs} / {queue.max_jobs}</span>
              </div>
              
              <div className="bg-gray-700 rounded-full h-2">
                <div 
                  className="bg-blue-400 h-2 rounded-full transition-all duration-300" 
                  style={{ width: `${(queue.jobs / queue.max_jobs) * 100}%` }}
                />
              </div>
              
              <div className="flex space-x-2 pt-2">
                <button className="px-3 py-1 bg-blue-600 text-white text-sm rounded hover:bg-blue-700">
                  Manage
                </button>
                <button className="px-3 py-1 bg-gray-600 text-white text-sm rounded hover:bg-gray-700">
                  Configure
                </button>
              </div>
            </div>
          </div>
        ))}
      </div>
    </div>
  );

  const renderDatasets = () => (
    <div className="space-y-6">
      <div className="flex justify-between items-center">
        <h3 className="text-xl font-semibold text-white">Dataset Management</h3>
        <div className="flex space-x-2">
          <button className="px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors">
            Create Dataset
          </button>
          <button className="px-4 py-2 bg-gray-600 text-white rounded-lg hover:bg-gray-700 transition-colors">
            Catalog Browse
          </button>
        </div>
      </div>

      <div className="bg-gray-800 rounded-lg border border-gray-700 overflow-hidden">
        <table className="w-full">
          <thead className="bg-gray-900">
            <tr>
              <th className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider">Dataset Name</th>
              <th className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider">Type</th>
              <th className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider">Volumes</th>
              <th className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider">Size</th>
              <th className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider">Records</th>
              <th className="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider">Actions</th>
            </tr>
          </thead>
          <tbody className="divide-y divide-gray-700">
            {mockDatasets.map((dataset, index) => (
              <tr key={index} className="hover:bg-gray-750">
                <td className="px-6 py-4 whitespace-nowrap text-sm text-white font-mono">{dataset.name}</td>
                <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-300">{dataset.type}</td>
                <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-300">{dataset.volumes.join(', ')}</td>
                <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-300">{dataset.size}</td>
                <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-300">{dataset.records.toLocaleString()}</td>
                <td className="px-6 py-4 whitespace-nowrap text-sm">
                  <div className="flex space-x-2">
                    <button className="text-blue-400 hover:text-blue-300">View</button>
                    <button className="text-green-400 hover:text-green-300">Edit</button>
                    <button className="text-red-400 hover:text-red-300">Delete</button>
                  </div>
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </div>
  );

  const renderContent = () => {
    switch (activeSection) {
      case 'overview': return renderOverview();
      case 'jobs': return renderJobs();
      case 'queues': return renderQueues();
      case 'datasets': return renderDatasets();
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
      </div>
    </div>
  );
};

export default SystemManagerPage;