import React, { useState, useEffect, useRef } from 'react';
import { 
  CpuChipIcon,
  ServerIcon,
  CircleStackIcon,
  ClockIcon,
  ArrowPathIcon
} from '@heroicons/react/24/outline';

interface SystemInfo {
  cpu: {
    usage_percent: number;
    core_count: number;
    frequency: {
      current: number;
      max: number;
    };
  };
  memory: {
    total: number;
    used: number;
    available: number;
    percent: number;
    total_gb: number;
    used_gb: number;
    available_gb: number;
  };
  disk: {
    total: number;
    used: number;
    free: number;
    percent: number;
    total_gb: number;
    used_gb: number;
    free_gb: number;
  };
  uptime: {
    hours: number;
    minutes: number;
    formatted: string;
  };
}

const DashboardPage: React.FC = () => {
  const [systemInfo, setSystemInfo] = useState<SystemInfo | null>(null);
  const [loading, setLoading] = useState(true);
  const [refreshInterval, setRefreshInterval] = useState<number>(5); // デフォルト5秒
  const [isRefreshing, setIsRefreshing] = useState(false);
  const [lastUpdated, setLastUpdated] = useState<Date | null>(null);
  const intervalRef = useRef<NodeJS.Timeout | null>(null);

  const refreshOptions = [
    { value: 1, label: '1秒' },
    { value: 5, label: '5秒' },
    { value: 10, label: '10秒' },
    { value: 60, label: '60秒' },
  ];

  useEffect(() => {
    fetchSystemInfo();
    startAutoRefresh();
    return () => stopAutoRefresh();
  }, []);

  useEffect(() => {
    // リフレッシュ間隔が変更されたときに再起動
    stopAutoRefresh();
    startAutoRefresh();
  }, [refreshInterval]);

  const startAutoRefresh = () => {
    if (intervalRef.current) clearInterval(intervalRef.current);
    intervalRef.current = setInterval(() => {
      fetchSystemInfo();
    }, refreshInterval * 1000);
  };

  const stopAutoRefresh = () => {
    if (intervalRef.current) {
      clearInterval(intervalRef.current);
      intervalRef.current = null;
    }
  };

  const fetchSystemInfo = async () => {
    try {
      setIsRefreshing(true);
      const response = await fetch('http://localhost:8000/api/system/info');
      if (response.ok) {
        const data = await response.json();
        setSystemInfo(data);
        setLastUpdated(new Date());
      }
    } catch (error) {
      console.error('システム情報取得エラー:', error);
    } finally {
      setLoading(false);
      setIsRefreshing(false);
    }
  };

  const handleRefreshIntervalChange = (event: React.ChangeEvent<HTMLSelectElement>) => {
    const newInterval = parseInt(event.target.value);
    setRefreshInterval(newInterval);
  };

  const handleManualRefresh = () => {
    fetchSystemInfo();
  };

  const stats = [
    { 
      label: 'CPU使用率', 
      value: `${systemInfo?.cpu.usage_percent || 0}%`,
      icon: CpuChipIcon, 
      color: 'blue',
      detail: `${systemInfo?.cpu.core_count || 0}コア`
    },
    { 
      label: 'メモリ使用率', 
      value: `${systemInfo?.memory.percent || 0}%`,
      icon: ServerIcon, 
      color: 'green',
      detail: `${systemInfo?.memory.used_gb || 0}GB / ${systemInfo?.memory.total_gb || 0}GB`
    },
    { 
      label: 'ディスク使用率', 
      value: `${systemInfo?.disk.percent || 0}%`,
      icon: CircleStackIcon, 
      color: 'purple',
      detail: `${systemInfo?.disk.used_gb || 0}GB / ${systemInfo?.disk.total_gb || 0}GB`
    },
    { 
      label: 'システム稼働時間', 
      value: `${systemInfo?.uptime.hours || 0}h`,
      icon: ClockIcon, 
      color: 'orange',
      detail: systemInfo?.uptime.formatted || '---'
    },
  ];

  // プログレスバーコンポーネント
  const ProgressBar: React.FC<{ value: number; color: string; label: string }> = ({ value, color, label }) => (
    <div className="mb-4">
      <div className="flex justify-between text-sm mb-1">
        <span className="text-gray-700 dark:text-gray-300">{label}</span>
        <span className="text-gray-500 dark:text-gray-400">{value}%</span>
      </div>
      <div className="w-full bg-gray-200 dark:bg-gray-700 rounded-full h-2.5">
        <div 
          className={`bg-gradient-to-r from-${color}-400 to-${color}-600 h-2.5 rounded-full transition-all duration-1000 ease-out`}
          style={{ width: `${Math.min(value, 100)}%` }}
        ></div>
      </div>
    </div>
  );

  // パイチャートコンポーネント (SVG使用)
  const PieChart: React.FC<{ used: number; total: number }> = ({ used, total }) => {
    const percentage = (used / total) * 100;
    const radius = 80;
    const circumference = 2 * Math.PI * radius;
    const strokeDasharray = circumference;
    const strokeDashoffset = circumference - (percentage / 100) * circumference;

    return (
      <div className="relative w-48 h-48 mx-auto">
        <svg className="w-48 h-48 transform -rotate-90" viewBox="0 0 200 200">
          {/* 背景円 */}
          <circle
            cx="100"
            cy="100"
            r={radius}
            stroke="rgb(229 231 235)"
            strokeWidth="20"
            fill="transparent"
            className="dark:stroke-gray-700"
          />
          {/* 使用量円 */}
          <circle
            cx="100"
            cy="100"
            r={radius}
            stroke="url(#gradient)"
            strokeWidth="20"
            fill="transparent"
            strokeDasharray={strokeDasharray}
            strokeDashoffset={strokeDashoffset}
            strokeLinecap="round"
            className="transition-all duration-1000 ease-out"
          />
          {/* グラデーション定義 */}
          <defs>
            <linearGradient id="gradient" x1="0%" y1="0%" x2="100%" y2="0%">
              <stop offset="0%" stopColor="rgb(139 92 246)" />
              <stop offset="100%" stopColor="rgb(168 85 247)" />
            </linearGradient>
          </defs>
        </svg>
        {/* 中央のテキスト */}
        <div className="absolute inset-0 flex items-center justify-center">
          <div className="text-center">
            <div className="text-2xl font-bold text-gray-900 dark:text-white">
              {percentage.toFixed(1)}%
            </div>
            <div className="text-sm text-gray-500 dark:text-gray-400">
              使用中
            </div>
          </div>
        </div>
      </div>
    );
  };

  if (loading) {
    return (
      <div className="h-full flex items-center justify-center">
        <div className="text-center">
          <div className="animate-spin h-12 w-12 border-4 border-blue-500 border-t-transparent rounded-full mx-auto mb-4"></div>
          <p className="text-gray-600 dark:text-gray-400">システム情報を読み込み中...</p>
        </div>
      </div>
    );
  }

  return (
    <div className="p-8">
      <div className="mb-8">
        <h1 className="text-2xl font-bold text-gray-900 dark:text-white mb-2">
          ダッシュボード
        </h1>
        <p className="text-gray-600 dark:text-gray-400">
          リアルタイムシステム監視とパフォーマンス指標
        </p>
      </div>

      {/* 統計カード */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
        {stats.map((stat, index) => (
          <div key={index} className="bg-white dark:bg-gray-800 rounded-lg shadow-sm p-6 border border-gray-200 dark:border-gray-700">
            <div className="flex items-center justify-between mb-4">
              <div className={`p-3 bg-gradient-to-br from-${stat.color}-400 to-${stat.color}-600 rounded-lg shadow-lg`}>
                <stat.icon className="w-6 h-6 text-white" />
              </div>
              <div className="text-right">
                <div className="text-2xl font-bold text-gray-900 dark:text-white">
                  {stat.value}
                </div>
                <div className="text-xs text-gray-500 dark:text-gray-400">
                  リアルタイム
                </div>
              </div>
            </div>
            <div className="text-sm font-medium text-gray-900 dark:text-white mb-1">
              {stat.label}
            </div>
            <div className="text-xs text-gray-500 dark:text-gray-400">
              {stat.detail}
            </div>
          </div>
        ))}
      </div>

      {/* グラフエリア */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        {/* システム使用状況 */}
        <div className="bg-white dark:bg-gray-800 rounded-lg shadow-sm p-6 border border-gray-200 dark:border-gray-700">
          <div className="flex items-center justify-between mb-6">
            <h3 className="text-lg font-semibold text-gray-900 dark:text-white">
              システム使用状況
            </h3>
            <div className="flex items-center space-x-3">
              {/* リフレッシュ間隔選択 */}
              <div className="flex items-center space-x-2">
                <label htmlFor="refresh-interval" className="text-sm text-gray-600 dark:text-gray-400">
                  更新間隔:
                </label>
                <select
                  id="refresh-interval"
                  value={refreshInterval}
                  onChange={handleRefreshIntervalChange}
                  className="px-2 py-1 text-sm border border-gray-300 dark:border-gray-600 rounded-md bg-white dark:bg-gray-700 text-gray-900 dark:text-white focus:ring-2 focus:ring-blue-500 focus:border-blue-500"
                >
                  {refreshOptions.map((option) => (
                    <option key={option.value} value={option.value}>
                      {option.label}
                    </option>
                  ))}
                </select>
              </div>
              
              {/* 手動リフレッシュボタン */}
              <button
                onClick={handleManualRefresh}
                disabled={isRefreshing}
                className="flex items-center px-3 py-1.5 text-sm bg-blue-600 hover:bg-blue-700 disabled:bg-blue-400 text-white rounded-md transition-colors focus:ring-2 focus:ring-blue-500 focus:ring-offset-2"
              >
                <ArrowPathIcon 
                  className={`w-4 h-4 mr-1 ${isRefreshing ? 'animate-spin' : ''}`} 
                />
                更新
              </button>
            </div>
          </div>
          
          {/* ステータス情報 */}
          {lastUpdated && (
            <div className="mb-4 text-xs text-gray-500 dark:text-gray-400 flex items-center justify-between">
              <span>
                最終更新: {lastUpdated.toLocaleTimeString('ja-JP')}
              </span>
              <span className={`flex items-center ${isRefreshing ? 'text-blue-600 dark:text-blue-400' : 'text-green-600 dark:text-green-400'}`}>
                <div className={`w-2 h-2 rounded-full mr-1 ${isRefreshing ? 'bg-blue-600 animate-pulse' : 'bg-green-600'}`}></div>
                {isRefreshing ? '更新中...' : '接続済み'}
              </span>
            </div>
          )}
          <div className="space-y-6">
            <ProgressBar 
              value={systemInfo?.cpu.usage_percent || 0} 
              color="blue" 
              label="CPU使用率"
            />
            <ProgressBar 
              value={systemInfo?.memory.percent || 0} 
              color="green" 
              label="メモリ使用率"
            />
            <ProgressBar 
              value={systemInfo?.disk.percent || 0} 
              color="purple" 
              label="ディスク使用率"
            />
          </div>
          
          {/* 詳細情報 */}
          <div className="mt-6 pt-6 border-t border-gray-200 dark:border-gray-700">
            <div className="grid grid-cols-1 gap-4">
              <div className="flex justify-between text-sm">
                <span className="text-gray-600 dark:text-gray-400">CPU周波数:</span>
                <span className="text-gray-900 dark:text-white">
                  {systemInfo?.cpu.frequency.current || 0} MHz
                </span>
              </div>
              <div className="flex justify-between text-sm">
                <span className="text-gray-600 dark:text-gray-400">利用可能メモリ:</span>
                <span className="text-gray-900 dark:text-white">
                  {systemInfo?.memory.available_gb || 0} GB
                </span>
              </div>
              <div className="flex justify-between text-sm">
                <span className="text-gray-600 dark:text-gray-400">空きディスク:</span>
                <span className="text-gray-900 dark:text-white">
                  {systemInfo?.disk.free_gb || 0} GB
                </span>
              </div>
            </div>
          </div>
        </div>

        {/* ディスク使用状況パイチャート */}
        <div className="bg-white dark:bg-gray-800 rounded-lg shadow-sm p-6 border border-gray-200 dark:border-gray-700">
          <h3 className="text-lg font-semibold text-gray-900 dark:text-white mb-6">
            ディスク使用状況
          </h3>
          <div className="flex flex-col items-center">
            <PieChart 
              used={systemInfo?.disk.used || 0} 
              total={systemInfo?.disk.total || 1} 
            />
            <div className="mt-6 w-full">
              <div className="flex justify-between items-center mb-2">
                <div className="flex items-center">
                  <div className="w-3 h-3 bg-gradient-to-r from-purple-400 to-purple-600 rounded-full mr-2"></div>
                  <span className="text-sm text-gray-600 dark:text-gray-400">使用中</span>
                </div>
                <span className="text-sm font-medium text-gray-900 dark:text-white">
                  {systemInfo?.disk.used_gb || 0} GB
                </span>
              </div>
              <div className="flex justify-between items-center">
                <div className="flex items-center">
                  <div className="w-3 h-3 bg-gray-300 dark:bg-gray-600 rounded-full mr-2"></div>
                  <span className="text-sm text-gray-600 dark:text-gray-400">空き容量</span>
                </div>
                <span className="text-sm font-medium text-gray-900 dark:text-white">
                  {systemInfo?.disk.free_gb || 0} GB
                </span>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default DashboardPage;