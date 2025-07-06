import React from 'react';
import { 
  ChartBarIcon,
  UserGroupIcon,
  ServerIcon,
  DocumentTextIcon 
} from '@heroicons/react/24/outline';

const DashboardPage: React.FC = () => {
  const stats = [
    { label: 'アクティブユーザー', value: '126', icon: UserGroupIcon, color: 'blue' },
    { label: 'SMEDマップ', value: '15', icon: DocumentTextIcon, color: 'green' },
    { label: 'システム稼働率', value: '99.9%', icon: ServerIcon, color: 'purple' },
    { label: '処理件数', value: '1,234', icon: ChartBarIcon, color: 'orange' },
  ];

  return (
    <div className="p-8">
      <div className="mb-8">
        <h1 className="text-2xl font-bold text-gray-900 dark:text-white mb-2">
          ダッシュボード
        </h1>
        <p className="text-gray-600 dark:text-gray-400">
          システムの概要とパフォーマンス指標
        </p>
      </div>

      {/* 統計カード */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
        {stats.map((stat, index) => (
          <div key={index} className="bg-white dark:bg-gray-800 rounded-lg shadow-sm p-6">
            <div className="flex items-center justify-between mb-4">
              <div className={`p-2 bg-${stat.color}-100 dark:bg-${stat.color}-900/20 rounded-lg`}>
                <stat.icon className={`w-6 h-6 text-${stat.color}-600 dark:text-${stat.color}-400`} />
              </div>
              <span className="text-sm text-gray-500 dark:text-gray-400">
                今日
              </span>
            </div>
            <div className="text-2xl font-bold text-gray-900 dark:text-white mb-1">
              {stat.value}
            </div>
            <div className="text-sm text-gray-600 dark:text-gray-400">
              {stat.label}
            </div>
          </div>
        ))}
      </div>

      {/* グラフエリア */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        <div className="bg-white dark:bg-gray-800 rounded-lg shadow-sm p-6">
          <h3 className="text-lg font-semibold text-gray-900 dark:text-white mb-4">
            システム使用状況
          </h3>
          <div className="h-64 flex items-center justify-center text-gray-400">
            <ChartBarIcon className="w-16 h-16" />
          </div>
        </div>

        <div className="bg-white dark:bg-gray-800 rounded-lg shadow-sm p-6">
          <h3 className="text-lg font-semibold text-gray-900 dark:text-white mb-4">
            最近のアクティビティ
          </h3>
          <div className="space-y-3">
            {[1, 2, 3, 4].map((i) => (
              <div key={i} className="flex items-center space-x-3 text-sm">
                <div className="w-2 h-2 bg-green-500 rounded-full"></div>
                <span className="text-gray-600 dark:text-gray-400">
                  ユーザー{i}がログインしました
                </span>
                <span className="text-gray-400 dark:text-gray-500 text-xs ml-auto">
                  {i}分前
                </span>
              </div>
            ))}
          </div>
        </div>
      </div>
    </div>
  );
};

export default DashboardPage;