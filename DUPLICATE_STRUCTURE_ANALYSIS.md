# Duplicate Structure Analysis Report

## Overview
The `/home/aspuser/app/ofasp-refactor` directory contains significant duplication and nested redundancy that's making the project structure confusing. This report identifies the duplications and provides recommendations for cleanup.

## Phase 1 Memory Leak Prevention APIs

### 新しいAPI追加 (New APIs Added)

#### 1. Memory Leak Detector API
**場所**: `/ofasp-refactor/src/utils/memoryLeakDetector.ts`
**機能**: Timer and event listener tracking with automatic cleanup
**使用法**:
```typescript
import { memoryLeakDetector } from '../utils/memoryLeakDetector';

// Timer registration
const timerId = memoryLeakDetector.registerInterval(() => {
  console.log('Timer tick');
}, 1000, 'Description');

// Event listener registration  
const cleanup = memoryLeakDetector.addEventListener(element, 'click', handler);

// Diagnostics
const stats = memoryLeakDetector.getDiagnostics();
```

#### 2. Timer Management Hook
**場所**: `/ofasp-refactor/src/hooks/useTimerManager.ts`
**機能**: React hook for safe timer management
**使用法**:
```typescript
import { useTimerManager } from '../hooks/useTimerManager';

const { startSystemTimer, stopSystemTimer, cleanup } = useTimerManager();

// Start system timer
startSystemTimer(() => updateTime(), 1000);

// Automatic cleanup on unmount
```

#### 3. Command History Hook  
**場所**: `/ofasp-refactor/src/hooks/useCommandHistory.ts`
**機能**: Bounded command history with memory safety
**使用法**:
```typescript
import { useCommandHistory } from '../hooks/useCommandHistory';

const { history, addCommand, clearHistory } = useCommandHistory({
  maxHistorySize: 100
});

// Add command with automatic size limiting
addCommand({ command: 'test', output: 'result', success: true });
```

#### 4. Browser Events Hook
**場所**: `/ofasp-refactor/src/hooks/useBrowserEvents.ts`  
**機能**: Safe browser event management with cleanup
**使用法**:
```typescript
import { useBrowserEvents } from '../hooks/useBrowserEvents';

const { setupBeforeUnload, cleanup } = useBrowserEvents();

// Setup page unload cleanup
setupBeforeUnload({ user: 'admin' });
```

### API設計原則
- **再利用性**: 全て独立したモジュールとして設計
- **型安全性**: 完全なTypeScript対応
- **メモリ安全**: 自動クリーンアップ機能内蔵
- **設定可能**: ハードコーディング排除、設定ベース
- **テスト可能**: 単体テスト対応設計

## Phase 2 Event Handler Management APIs

### 新しいAPI追加 (New APIs Added)

#### 1. Event Handler Registry System
**場所**: `/ofasp-refactor/src/utils/eventHandlerRegistry.ts`
**機能**: Centralized event handler management with deduplication
**使用法**:
```typescript
import { eventHandlerRegistry } from '../utils/eventHandlerRegistry';

// Single handler registration
const handlerId = eventHandlerRegistry.registerHandler('click', handler, {
  id: 'custom_id',
  description: 'Button click handler'
});

// WebSocket handler registration
const wsHandlerId = eventHandlerRegistry.registerWebSocketHandler(
  'message', handler, 'componentId', 'WebSocket message handler'
);

// Bulk registration
const handlerIds = eventHandlerRegistry.registerComponentHandlers('MyComponent', [
  { event: 'click', handler: clickHandler },
  { event: 'focus', handler: focusHandler }
]);

// Statistics
const stats = eventHandlerRegistry.getStatistics();
```

#### 2. Event Handler Hook
**場所**: `/ofasp-refactor/src/hooks/useEventHandlers.ts`
**機能**: React hook for component event handler management
**使用法**:
```typescript
import { useEventHandlers } from '../hooks/useEventHandlers';

const { registerHandler, registerWebSocketHandler, cleanup } = 
  useEventHandlers('ComponentName');

// Register handlers
const handlerId = registerHandler('click', handleClick, {
  description: 'Main button click'
});

const wsHandlerId = registerWebSocketHandler('message', handleMessage);

// Automatic cleanup on unmount
```

#### 3. Grid Event Delegation System
**場所**: `/ofasp-refactor/src/utils/gridEventDelegation.ts`
**機能**: Event delegation for large grids (24x80 = 1,920 cells)
**使用法**:
```typescript
import { GridEventDelegation } from '../utils/gridEventDelegation';

const delegator = new GridEventDelegation(containerElement, {
  gridRows: 24,
  gridCols: 80
});

// Register cell click handler
delegator.registerCellHandler('click', (position, event) => {
  console.log(`Clicked cell: ${position.row}, ${position.col}`);
});

// Multiple handlers
delegator.registerMultipleCellHandlers([
  { eventType: 'click', handler: handleCellClick },
  { eventType: 'focus', handler: handleCellFocus }
]);

// Statistics
const stats = delegator.getStatistics();
```

## Phase 3 Component Complexity Analysis APIs

### 新しいAPI追加 (New APIs Added)

#### 1. Component Splitter Utility
**場所**: `/ofasp-refactor/src/utils/componentSplitter.ts`
**機能**: Component complexity analysis and responsibility-based splitting
**使用法**:
```typescript
import { ComponentSplitter } from '../utils/componentSplitter';

const splitter = new ComponentSplitter({
  componentName: 'MyComponent',
  maxLinesPerComponent: 150,
  maxStateVariables: 8
});

// Complexity analysis
const metrics = splitter.analyzeComplexity(sourceCode);

// Split suggestions
const splits = splitter.splitByResponsibility(sourceCode);

// Hook extraction suggestions
const hooks = splitter.suggestHookExtractions(sourceCode);

// Split feasibility score
const { score, recommendation } = splitter.calculateSplitScore(metrics);
```

#### 2. Component Optimization Hook
**場所**: `/ofasp-refactor/src/hooks/useComponentOptimization.ts`
**機能**: Performance optimization with memoization and render tracking
**使用法**:
```typescript
import { useComponentOptimization } from '../hooks/useComponentOptimization';

const { 
  optimizedHandlers, 
  renderStats, 
  memoizedValue, 
  getOptimizationReport 
} = useComponentOptimization('ComponentName');

// Debounced handler
const debouncedHandler = optimizedHandlers.debounce(handleInput, 300);

// Throttled handler
const throttledHandler = optimizedHandlers.throttle(handleScroll, 100);

// Memoized expensive calculation
const result = memoizedValue(() => expensiveCalculation(), [deps]);

// Get optimization report
const report = getOptimizationReport();
```

#### 3. React Refactor Tools
**場所**: `/ofasp-refactor/src/utils/reactRefactorTools.ts`
**機能**: React component analysis and refactoring suggestions
**使用法**:
```typescript
import { ReactRefactorTools } from '../utils/reactRefactorTools';

const refactorHelper = new ReactRefactorTools();

// Component analysis
const analysis = refactorHelper.analyzeComponent(sourceCode, 'ComponentName');

console.log('Complexity Score:', analysis.complexityScore);
console.log('Refactor Suggestions:', analysis.suggestions);
console.log('Hook Opportunities:', analysis.hookOpportunities);
console.log('Performance Issues:', analysis.performanceIssues);
```

## Phase 4 WebSocket Connection Management APIs

### 新しいAPI追加 (New APIs Added)

#### 1. WebSocket Manager
**場所**: `/ofasp-refactor/src/utils/webSocketManager.ts`
**機能**: Robust WebSocket connection management with auto-reconnection
**使用法**:
```typescript
import { WebSocketManager } from '../utils/webSocketManager';

const wsManager = new WebSocketManager({
  url: 'ws://localhost:8080/websocket',
  reconnectInterval: 3000,
  maxReconnectAttempts: 10,
  messageQueueSize: 100
});

// Connect
await wsManager.connect();

// Subscribe to events
const unsubscribe = wsManager.on('message', (data) => {
  console.log('Received:', data);
});

// Send message
wsManager.send({
  type: 'command',
  data: { action: 'execute' },
  priority: 'high'
});

// Get statistics
const stats = wsManager.getStatistics();
```

#### 2. WebSocket Connection Hook
**場所**: `/ofasp-refactor/src/hooks/useWebSocketConnection.ts`
**機能**: React hook for WebSocket integration with component lifecycle
**使用法**:
```typescript
import { useWebSocketConnection } from '../hooks/useWebSocketConnection';

const { 
  connectionState, 
  isConnected, 
  sendMessage, 
  subscribe 
} = useWebSocketConnection('ws://localhost:8080/websocket', {
  autoConnect: true,
  retryOnMount: true
});

// Subscribe to events
useEffect(() => {
  const unsubscribe = subscribe('terminal_output', (data) => {
    console.log('Terminal output:', data);
  });
  return unsubscribe;
}, [subscribe]);

// Send command
const handleSendCommand = () => {
  sendMessage({
    type: 'terminal_input',
    data: { command: 'ls -la' },
    priority: 'medium'
  });
};
```

#### 3. WebSocket Hub Manager
**場所**: `/ofasp-refactor/src/utils/webSocketHub.ts`
**機能**: Multiple WebSocket connection management and load balancing
**使用法**:
```typescript
import { WebSocketHub } from '../utils/webSocketHub';

const hub = new WebSocketHub({
  maxConnections: 10,
  loadBalancing: 'round_robin'
});

// Register connections
hub.registerConnection('main', wsManager1, { priority: 1, tags: ['primary'] });
hub.registerConnection('backup', wsManager2, { priority: 2, tags: ['secondary'] });

// Broadcast message
hub.broadcast({
  type: 'system_alert',
  data: { message: 'System maintenance' },
  priority: 'high'
});

// Send with load balancing
const connectionId = hub.sendWithLoadBalancing({
  type: 'user_command',
  data: { command: 'process_data' },
  priority: 'medium'
}, ['primary']);

// Get hub statistics
const hubStats = hub.getStatistics();
```

## Phase 5 DOM Manipulation Anti-Patterns APIs

### 新しいAPI追加 (New APIs Added)

#### 1. Virtual DOM Renderer
**場所**: `/ofasp-refactor/src/utils/virtualDOMRenderer.ts`
**機能**: Large grid virtualization with viewport culling (24x80 = 1,920 cells)
**使用法**:
```typescript
import { VirtualDOMRenderer } from '../utils/virtualDOMRenderer';

const renderer = new VirtualDOMRenderer(containerElement, {
  gridRows: 24,
  gridCols: 80,
  cellWidth: 12,
  cellHeight: 20,
  enableVirtualization: true,
  reuseElements: true
});

// Render cell data
const stats = renderer.render(cellData);

// Update specific cells
renderer.updateCells([{ row: 0, col: 0, content: 'Updated' }]);

// Handle scroll for virtual rendering
renderer.onScroll();

// Get performance statistics
const renderStats = renderer.getStatistics();
```

#### 2. DOM Optimization Hook
**場所**: `/ofasp-refactor/src/hooks/useDOMOptimization.ts`
**機能**: DOM update batching and reflow optimization
**使用法**:
```typescript
import { useDOMOptimization } from '../hooks/useDOMOptimization';

const { 
  batchUpdates, 
  measureLayout, 
  optimizeReflow, 
  getOptimizationStats 
} = useDOMOptimization({
  enableBatching: true,
  enableMeasurementCache: true
});

// Batch DOM operations
await batchUpdates({
  reads: [() => element.getBoundingClientRect()],
  writes: [() => { element.style.width = '100px'; }],
  callback: () => console.log('Batch complete')
});

// Cached layout measurement
const layout = measureLayout(element, true);

// Optimize reflow operations
optimizeReflow(() => {
  element.style.transform = 'translateX(100px)';
});
```

#### 3. DOM Manipulation Safe Guards
**場所**: `/ofasp-refactor/src/utils/domManipulationSafeGuards.ts`
**機能**: Safe DOM operations with anti-pattern detection
**使用法**:
```typescript
import { DOMSafeGuards } from '../utils/domManipulationSafeGuards';

const safeDOM = new DOMSafeGuards({
  enableErrorHandling: true,
  enablePerformanceMonitoring: true,
  enableAntiPatternDetection: true
});

// Safe element queries with caching
const element = safeDOM.safeQuerySelector('#myElement', document, true);
const elements = safeDOM.safeQuerySelectorAll('.items');

// Safe element creation with sanitization
const newElement = safeDOM.safeCreateElement('div', {
  className: 'safe-element',
  textContent: 'Safe content',
  attributes: { 'data-id': '123' }
});

// Safe style updates with batching
safeDOM.safeUpdateStyles(element, {
  width: '200px',
  height: '100px',
  backgroundColor: 'blue'
}, true);

// Get safety statistics and warnings
const stats = safeDOM.getStatistics();
const warnings = safeDOM.getAntiPatternWarnings('high');
```

## 1. Duplicate MD Files

### Exact Duplicates (same MD5 hash):
- `ASP_COMMANDS_UPDATE_SUMMARY.md` - Identical in both locations
- `CLAUDE_SMED.md` - Identical in both locations  
- `CODING_RULES.md` - Identical in both locations
- `MASTER_CLAUDE.md` - Identical in both locations
- `README.md` - Identical in both locations

### Unique to ofasp-refactor:
- `WEBSOCKET_HUB_CLIENT_INTEGRATION.md`
- `WEBSOCKET_INTEGRATION_SUMMARY.md`
- `WEBSOCKET_SMED_FIXES_SUMMARY.md`

### Unique to root:
- `CLAUDE.md` (empty file in root)
- `POSITION_BASED_SMED_API.md`
- `UNIFIED_API_DOCUMENTATION.md`

## 2. Duplicate Directories

### Complete Directory Duplications:

#### asp-manager
- `/home/aspuser/app/asp-manager/` (original)
- `/home/aspuser/app/ofasp-refactor/asp-manager/` (duplicate)
- Both contain identical files and structure
- The duplicate has its own node_modules (758MB vs 743MB)

#### Nested ofasp-refactor
- `/home/aspuser/app/ofasp-refactor/ofasp-refactor/` - A nested duplicate!
- Contains different content than parent, appears to be an older version
- Has its own bore binary (7MB) and logs
- Contains Korean text report and different MD files

## 3. Server Directories
- `/home/aspuser/app/server/` - Main server (more complete)
- `/home/aspuser/app/ofasp-refactor/server/` - Partial duplicate
- The ofasp-refactor version is missing many files present in the main server:
  - Missing: cobol conversion tools, test files, demo HTML files
  - Has different api_server.py (92KB vs 176KB in main)

## 4. Volume Structure
- Main volume has complete structure: JAVA, PRODLIB, SMED, TESTLIB, XMLLIB
- ofasp-refactor volume only has: PRODLIB, TESTLIB (missing JAVA, SMED, XMLLIB)

## 5. Other Duplications
- Multiple package.json files indicating separate npm projects
- Duplicate node_modules directories consuming significant space
- Duplicate python-service directories
- Duplicate src directories with React components

## Recommendations for Cleanup

### 1. Remove Complete Duplicates
```bash
# Remove duplicate MD files in ofasp-refactor
rm /home/aspuser/app/ofasp-refactor/ASP_COMMANDS_UPDATE_SUMMARY.md
rm /home/aspuser/app/ofasp-refactor/CLAUDE_SMED.md
rm /home/aspuser/app/ofasp-refactor/CODING_RULES.md
rm /home/aspuser/app/ofasp-refactor/MASTER_CLAUDE.md
rm /home/aspuser/app/ofasp-refactor/README.md
```

### 2. Remove Nested ofasp-refactor
```bash
# This appears to be accidentally nested
rm -rf /home/aspuser/app/ofasp-refactor/ofasp-refactor/
```

### 3. Consolidate asp-manager
```bash
# Remove duplicate asp-manager
rm -rf /home/aspuser/app/ofasp-refactor/asp-manager/
```

### 4. Review and Merge Server Directories
- The main server directory is more complete
- Review any unique changes in ofasp-refactor/server/
- Consider removing the duplicate after merging any valuable changes

### 5. Clean up Volume Structure
```bash
# Remove incomplete volume structure
rm -rf /home/aspuser/app/ofasp-refactor/volume/
```

### 6. Consolidate Unique Files
Move these unique files to appropriate locations:
- WEBSOCKET_*.md files from ofasp-refactor to main docs directory
- Any unique server configurations or implementations

### 7. Remove node_modules and Rebuild
```bash
# Remove all duplicate node_modules
rm -rf /home/aspuser/app/ofasp-refactor/node_modules/
rm -rf /home/aspuser/app/ofasp-refactor/asp-manager/node_modules/
```

## Impact Analysis
- **Space Savings**: Approximately 1-2GB from removing duplicate node_modules
- **Clarity**: Removing nested directories will significantly improve navigation
- **Maintenance**: Single source of truth for each component

## Safe Cleanup Script
```bash
#!/bin/bash
# Create backup first
tar -czf ofasp-refactor-backup.tar.gz /home/aspuser/app/ofasp-refactor/

# Move unique documentation
mv /home/aspuser/app/ofasp-refactor/WEBSOCKET_*.md /home/aspuser/app/docs/

# Remove duplicates
rm -rf /home/aspuser/app/ofasp-refactor/ofasp-refactor/
rm -rf /home/aspuser/app/ofasp-refactor/asp-manager/
rm -rf /home/aspuser/app/ofasp-refactor/node_modules/
rm -rf /home/aspuser/app/ofasp-refactor/volume/

# Remove duplicate MD files
rm /home/aspuser/app/ofasp-refactor/{ASP_COMMANDS_UPDATE_SUMMARY,CLAUDE_SMED,CODING_RULES,MASTER_CLAUDE,README}.md
```

## Conclusion
The ofasp-refactor directory appears to be a partially complete attempt at refactoring that has accumulated duplicates and nested structures. Most of its content is redundant with the main project structure. After backing up any unique changes, the majority of this directory can be safely removed to clean up the project structure.