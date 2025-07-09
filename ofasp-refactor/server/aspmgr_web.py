#!/usr/bin/env python3
"""
ASP Manager Web Dashboard
Flask-based web interface for system management
"""

import os
import json
import time
import threading
from datetime import datetime
from flask import Flask, jsonify, render_template_string, request
from flask_cors import CORS
import psutil

# Configuration from environment variables
PORT = int(os.getenv('ASPMGR_WEB_PORT', '5000'))
HOST = os.getenv('ASPMGR_WEB_HOST', '0.0.0.0')
DEBUG = os.getenv('ASPMGR_DEBUG', 'false').lower() == 'true'
REFRESH_INTERVAL = float(os.getenv('ASPMGR_REFRESH_INTERVAL', '2.0'))

# Thresholds
CPU_THRESHOLD = float(os.getenv('ASPMGR_CPU_THRESHOLD', '80.0'))
MEMORY_THRESHOLD = float(os.getenv('ASPMGR_MEMORY_THRESHOLD', '80.0'))
DISK_THRESHOLD = float(os.getenv('ASPMGR_DISK_THRESHOLD', '90.0'))

app = Flask(__name__)
CORS(app, origins=['http://localhost:3005', 'http://localhost:5000'])

# Global data cache
system_data = {
    'system_info': {},
    'processes': [],
    'alerts': [],
    'logs': [],
    'last_update': None
}
data_lock = threading.Lock()


def format_bytes(bytes_value):
    """Format bytes into human readable format"""
    for unit in ['B', 'KB', 'MB', 'GB', 'TB']:
        if bytes_value < 1024.0:
            return f"{bytes_value:.1f} {unit}"
        bytes_value /= 1024.0
    return f"{bytes_value:.1f} PB"


def format_uptime(boot_time):
    """Format uptime from boot time"""
    uptime_seconds = time.time() - boot_time
    days = int(uptime_seconds // 86400)
    hours = int((uptime_seconds % 86400) // 3600)
    minutes = int((uptime_seconds % 3600) // 60)
    
    if days > 0:
        return f"{days}d {hours}h {minutes}m"
    elif hours > 0:
        return f"{hours}h {minutes}m"
    else:
        return f"{minutes}m"


def update_system_data():
    """Update system monitoring data"""
    global system_data
    
    try:
        # System info
        cpu_percent = psutil.cpu_percent(interval=1)
        memory = psutil.virtual_memory()
        disk = psutil.disk_usage('/')
        boot_time = psutil.boot_time()
        
        system_info = {
            'hostname': os.uname().nodename,
            'uptime': format_uptime(boot_time),
            'cpu_percent': cpu_percent,
            'cpu_count': psutil.cpu_count(),
            'memory_total': format_bytes(memory.total),
            'memory_used': format_bytes(memory.used),
            'memory_percent': memory.percent,
            'disk_total': format_bytes(disk.total),
            'disk_used': format_bytes(disk.used),
            'disk_percent': disk.percent,
            'process_count': len(psutil.pids()),
            'load_avg': os.getloadavg() if hasattr(os, 'getloadavg') else (0, 0, 0)
        }
        
        # Process list (top 10 by CPU)
        processes = []
        for proc in psutil.process_iter(['pid', 'name', 'username', 'cpu_percent', 'memory_percent', 'status']):
            try:
                pinfo = proc.info
                processes.append({
                    'pid': pinfo['pid'],
                    'name': pinfo['name'],
                    'user': pinfo['username'],
                    'cpu_percent': pinfo['cpu_percent'],
                    'memory_percent': pinfo['memory_percent'],
                    'status': pinfo['status']
                })
            except (psutil.NoSuchProcess, psutil.AccessDenied):
                pass
        
        # Sort by CPU usage and take top 10
        processes.sort(key=lambda x: x['cpu_percent'], reverse=True)
        processes = processes[:10]
        
        # Check alerts
        alerts = []
        if cpu_percent > CPU_THRESHOLD:
            alerts.append({
                'type': 'warning',
                'message': f'High CPU usage: {cpu_percent:.1f}%',
                'timestamp': datetime.now().isoformat()
            })
        
        if memory.percent > MEMORY_THRESHOLD:
            alerts.append({
                'type': 'warning',
                'message': f'High memory usage: {memory.percent:.1f}%',
                'timestamp': datetime.now().isoformat()
            })
        
        if disk.percent > DISK_THRESHOLD:
            alerts.append({
                'type': 'warning',
                'message': f'High disk usage: {disk.percent:.1f}%',
                'timestamp': datetime.now().isoformat()
            })
        
        # Demo logs
        logs = [
            f"[{datetime.now().strftime('%H:%M:%S')}] INFO: System monitoring active",
            f"[{datetime.now().strftime('%H:%M:%S')}] INFO: CPU usage: {cpu_percent:.1f}%",
            f"[{datetime.now().strftime('%H:%M:%S')}] INFO: Memory usage: {memory.percent:.1f}%",
            f"[{datetime.now().strftime('%H:%M:%S')}] INFO: Disk usage: {disk.percent:.1f}%",
            f"[{datetime.now().strftime('%H:%M:%S')}] INFO: Total processes: {len(psutil.pids())}",
        ]
        
        # Update global data
        with data_lock:
            system_data['system_info'] = system_info
            system_data['processes'] = processes
            system_data['alerts'] = alerts
            system_data['logs'] = logs
            system_data['last_update'] = datetime.now().isoformat()
            
    except Exception as e:
        print(f"Error updating system data: {e}")


def update_loop():
    """Background update loop"""
    while True:
        update_system_data()
        time.sleep(REFRESH_INTERVAL)


# HTML Template
HTML_TEMPLATE = '''
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>ASP Manager - Web Dashboard</title>
    <style>
        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }
        
        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
            background-color: #0f172a;
            color: #e2e8f0;
            line-height: 1.6;
        }
        
        .container {
            max-width: 1400px;
            margin: 0 auto;
            padding: 20px;
        }
        
        .header {
            background: linear-gradient(135deg, #3b82f6 0%, #1e40af 100%);
            padding: 20px;
            border-radius: 10px;
            margin-bottom: 30px;
            box-shadow: 0 4px 6px rgba(0, 0, 0, 0.3);
        }
        
        .header h1 {
            font-size: 2.5rem;
            margin-bottom: 10px;
        }
        
        .header p {
            opacity: 0.9;
        }
        
        .grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(350px, 1fr));
            gap: 20px;
            margin-bottom: 30px;
        }
        
        .card {
            background: #1e293b;
            border-radius: 10px;
            padding: 20px;
            box-shadow: 0 4px 6px rgba(0, 0, 0, 0.3);
            border: 1px solid #334155;
        }
        
        .card h2 {
            font-size: 1.5rem;
            margin-bottom: 15px;
            color: #60a5fa;
        }
        
        .metric {
            display: flex;
            justify-content: space-between;
            align-items: center;
            padding: 10px 0;
            border-bottom: 1px solid #334155;
        }
        
        .metric:last-child {
            border-bottom: none;
        }
        
        .metric-label {
            color: #94a3b8;
        }
        
        .metric-value {
            font-weight: bold;
            font-size: 1.1rem;
        }
        
        .progress {
            width: 100%;
            height: 8px;
            background: #334155;
            border-radius: 4px;
            overflow: hidden;
            margin-top: 5px;
        }
        
        .progress-bar {
            height: 100%;
            background: #3b82f6;
            transition: width 0.3s ease;
        }
        
        .progress-bar.warning {
            background: #f59e0b;
        }
        
        .progress-bar.danger {
            background: #ef4444;
        }
        
        .table {
            width: 100%;
            border-collapse: collapse;
        }
        
        .table th,
        .table td {
            padding: 12px;
            text-align: left;
            border-bottom: 1px solid #334155;
        }
        
        .table th {
            background: #0f172a;
            color: #60a5fa;
            font-weight: 600;
        }
        
        .alert {
            padding: 15px;
            border-radius: 8px;
            margin-bottom: 10px;
            display: flex;
            align-items: center;
            gap: 10px;
        }
        
        .alert.warning {
            background: #7c3f00;
            border: 1px solid #f59e0b;
            color: #fbbf24;
        }
        
        .alert-icon {
            font-size: 1.5rem;
        }
        
        .log-entry {
            padding: 8px;
            background: #0f172a;
            border-radius: 4px;
            margin-bottom: 5px;
            font-family: monospace;
            font-size: 0.9rem;
        }
        
        .status {
            display: inline-block;
            padding: 2px 8px;
            border-radius: 4px;
            font-size: 0.85rem;
        }
        
        .status.running {
            background: #065f46;
            color: #34d399;
        }
        
        .status.sleeping {
            background: #1e40af;
            color: #60a5fa;
        }
        
        .refresh-info {
            text-align: center;
            color: #64748b;
            margin-top: 20px;
        }
        
        @keyframes pulse {
            0% { opacity: 1; }
            50% { opacity: 0.5; }
            100% { opacity: 1; }
        }
        
        .updating {
            animation: pulse 1s infinite;
        }
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>🖥️ ASP Manager Dashboard</h1>
            <p>Real-time System Monitoring and Management</p>
        </div>
        
        <div class="grid">
            <!-- System Overview -->
            <div class="card">
                <h2>System Overview</h2>
                <div id="system-overview">
                    <div class="updating">Loading...</div>
                </div>
            </div>
            
            <!-- Resource Usage -->
            <div class="card">
                <h2>Resource Usage</h2>
                <div id="resource-usage">
                    <div class="updating">Loading...</div>
                </div>
            </div>
            
            <!-- Alerts -->
            <div class="card">
                <h2>System Alerts</h2>
                <div id="alerts">
                    <div class="updating">Loading...</div>
                </div>
            </div>
        </div>
        
        <!-- Process Table -->
        <div class="card">
            <h2>Top Processes</h2>
            <div id="process-table">
                <div class="updating">Loading...</div>
            </div>
        </div>
        
        <!-- System Logs -->
        <div class="card">
            <h2>System Logs</h2>
            <div id="system-logs">
                <div class="updating">Loading...</div>
            </div>
        </div>
        
        <div class="refresh-info">
            <p>Last updated: <span id="last-update">-</span> | Auto-refresh: <span id="refresh-interval">{{ refresh_interval }}s</span></p>
        </div>
    </div>
    
    <script>
        const API_BASE = '';
        const REFRESH_INTERVAL = {{ refresh_interval }} * 1000;
        
        function formatProgressBarClass(value, warningThreshold, dangerThreshold) {
            if (value >= dangerThreshold) return 'danger';
            if (value >= warningThreshold) return 'warning';
            return '';
        }
        
        function updateSystemOverview(data) {
            const html = `
                <div class="metric">
                    <span class="metric-label">Hostname</span>
                    <span class="metric-value">${data.hostname}</span>
                </div>
                <div class="metric">
                    <span class="metric-label">Uptime</span>
                    <span class="metric-value">${data.uptime}</span>
                </div>
                <div class="metric">
                    <span class="metric-label">Load Average</span>
                    <span class="metric-value">${data.load_avg.map(v => v.toFixed(2)).join(', ')}</span>
                </div>
                <div class="metric">
                    <span class="metric-label">Total Processes</span>
                    <span class="metric-value">${data.process_count}</span>
                </div>
            `;
            document.getElementById('system-overview').innerHTML = html;
        }
        
        function updateResourceUsage(data) {
            const cpuClass = formatProgressBarClass(data.cpu_percent, 60, 80);
            const memClass = formatProgressBarClass(data.memory_percent, 60, 80);
            const diskClass = formatProgressBarClass(data.disk_percent, 70, 90);
            
            const html = `
                <div class="metric">
                    <span class="metric-label">CPU Usage</span>
                    <span class="metric-value">${data.cpu_percent.toFixed(1)}%</span>
                </div>
                <div class="progress">
                    <div class="progress-bar ${cpuClass}" style="width: ${data.cpu_percent}%"></div>
                </div>
                
                <div class="metric" style="margin-top: 15px;">
                    <span class="metric-label">Memory</span>
                    <span class="metric-value">${data.memory_used} / ${data.memory_total} (${data.memory_percent.toFixed(1)}%)</span>
                </div>
                <div class="progress">
                    <div class="progress-bar ${memClass}" style="width: ${data.memory_percent}%"></div>
                </div>
                
                <div class="metric" style="margin-top: 15px;">
                    <span class="metric-label">Disk</span>
                    <span class="metric-value">${data.disk_used} / ${data.disk_total} (${data.disk_percent.toFixed(1)}%)</span>
                </div>
                <div class="progress">
                    <div class="progress-bar ${diskClass}" style="width: ${data.disk_percent}%"></div>
                </div>
            `;
            document.getElementById('resource-usage').innerHTML = html;
        }
        
        function updateAlerts(alerts) {
            if (alerts.length === 0) {
                document.getElementById('alerts').innerHTML = '<p style="color: #34d399;">✓ No active alerts</p>';
                return;
            }
            
            const html = alerts.map(alert => `
                <div class="alert ${alert.type}">
                    <span class="alert-icon">⚠️</span>
                    <div>
                        <div>${alert.message}</div>
                        <small style="opacity: 0.7;">${new Date(alert.timestamp).toLocaleTimeString()}</small>
                    </div>
                </div>
            `).join('');
            document.getElementById('alerts').innerHTML = html;
        }
        
        function updateProcessTable(processes) {
            const html = `
                <table class="table">
                    <thead>
                        <tr>
                            <th>PID</th>
                            <th>Name</th>
                            <th>User</th>
                            <th>CPU %</th>
                            <th>Memory %</th>
                            <th>Status</th>
                        </tr>
                    </thead>
                    <tbody>
                        ${processes.map(proc => `
                            <tr>
                                <td>${proc.pid}</td>
                                <td>${proc.name}</td>
                                <td>${proc.user}</td>
                                <td>${proc.cpu_percent.toFixed(1)}%</td>
                                <td>${proc.memory_percent.toFixed(1)}%</td>
                                <td><span class="status ${proc.status}">${proc.status}</span></td>
                            </tr>
                        `).join('')}
                    </tbody>
                </table>
            `;
            document.getElementById('process-table').innerHTML = html;
        }
        
        function updateSystemLogs(logs) {
            const html = logs.map(log => `
                <div class="log-entry">${log}</div>
            `).join('');
            document.getElementById('system-logs').innerHTML = html;
        }
        
        async function fetchData() {
            try {
                const response = await fetch(`${API_BASE}/api/system`);
                const data = await response.json();
                
                updateSystemOverview(data.system_info);
                updateResourceUsage(data.system_info);
                updateAlerts(data.alerts);
                updateProcessTable(data.processes);
                updateSystemLogs(data.logs);
                
                document.getElementById('last-update').textContent = new Date(data.last_update).toLocaleTimeString();
            } catch (error) {
                console.error('Error fetching data:', error);
            }
        }
        
        // Initial fetch
        fetchData();
        
        // Set up auto-refresh
        setInterval(fetchData, REFRESH_INTERVAL);
    </script>
</body>
</html>
'''


@app.route('/')
def index():
    """Serve the main dashboard"""
    return render_template_string(HTML_TEMPLATE, refresh_interval=REFRESH_INTERVAL)


@app.route('/api/system')
def api_system():
    """Get current system data"""
    with data_lock:
        return jsonify(system_data)


@app.route('/api/processes')
def api_processes():
    """Get detailed process list"""
    try:
        processes = []
        for proc in psutil.process_iter(['pid', 'name', 'username', 'cpu_percent', 'memory_percent', 'status', 'create_time']):
            try:
                pinfo = proc.info
                processes.append({
                    'pid': pinfo['pid'],
                    'name': pinfo['name'],
                    'user': pinfo['username'],
                    'cpu_percent': pinfo['cpu_percent'],
                    'memory_percent': pinfo['memory_percent'],
                    'status': pinfo['status'],
                    'create_time': datetime.fromtimestamp(pinfo['create_time']).isoformat()
                })
            except (psutil.NoSuchProcess, psutil.AccessDenied):
                pass
        
        # Sort by CPU usage
        processes.sort(key=lambda x: x['cpu_percent'], reverse=True)
        
        return jsonify({
            'processes': processes,
            'total': len(processes),
            'timestamp': datetime.now().isoformat()
        })
    except Exception as e:
        return jsonify({'error': str(e)}), 500


@app.route('/api/process/<int:pid>/kill', methods=['POST'])
def kill_process(pid):
    """Kill a process by PID"""
    try:
        proc = psutil.Process(pid)
        proc.terminate()
        return jsonify({'success': True, 'message': f'Process {pid} terminated'})
    except psutil.NoSuchProcess:
        return jsonify({'error': 'Process not found'}), 404
    except psutil.AccessDenied:
        return jsonify({'error': 'Access denied'}), 403
    except Exception as e:
        return jsonify({'error': str(e)}), 500


@app.route('/api/config')
def api_config():
    """Get current configuration"""
    config = {
        'host': HOST,
        'port': PORT,
        'debug': DEBUG,
        'refresh_interval': REFRESH_INTERVAL,
        'thresholds': {
            'cpu': CPU_THRESHOLD,
            'memory': MEMORY_THRESHOLD,
            'disk': DISK_THRESHOLD
        },
        'environment': dict(os.environ)
    }
    return jsonify(config)


@app.route('/health')
def health():
    """Health check endpoint"""
    return jsonify({
        'status': 'OK',
        'timestamp': datetime.now().isoformat(),
        'service': 'ASP Manager Web Dashboard'
    })


if __name__ == '__main__':
    # Start background update thread
    update_thread = threading.Thread(target=update_loop, daemon=True)
    update_thread.start()
    
    # Give it a moment to collect initial data
    time.sleep(1)
    
    print(f"ASP Manager Web Dashboard starting on http://{HOST}:{PORT}")
    app.run(host=HOST, port=PORT, debug=DEBUG)